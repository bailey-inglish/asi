#!/usr/bin/env python3
"""
tpia_tracker.py — TPIA Public Records Request Tracker
======================================================
Tracks Texas Public Information Act requests sent to public colleges/universities
for student directory information as part of the ASI voter outreach research project.

TPIA Key Requirements (Gov. Code Ch. 552):
  - Agencies must respond within 10 *business* days of receipt
  - If agency needs AG opinion, it must request within 10 business days
  - AG has 45 business days to rule once request is submitted
  - Agencies may charge reasonable copying/compilation fees
  - Agencies cannot require requestor to explain purpose

Usage:
  python tpia_tracker.py init                  Initialize requests.csv from colleges.csv
  python tpia_tracker.py status                Show status dashboard
  python tpia_tracker.py send <id>             Send initial TPIA request (use --dry-run to preview)
  python tpia_tracker.py send-all              Send to all unsent institutions
  python tpia_tracker.py update <id> <status> [note]  Update a request's status
  python tpia_tracker.py deadlines             Show upcoming and overdue deadlines
  python tpia_tracker.py followup              Send follow-ups to overdue/stalled requests
  python tpia_tracker.py show-template <name> Display a template
  python tpia_tracker.py log <id>              Show full history for a request
"""

import argparse
import csv
import importlib.util
import os
import re
import smtplib
import sys
from datetime import date, timedelta
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from pathlib import Path

# ---------------------------------------------------------------------------
# Paths (relative to this script's directory)
# ---------------------------------------------------------------------------
HERE = Path(__file__).parent.resolve()
COLLEGES_CSV = HERE / "colleges.csv"
REQUESTS_CSV = HERE / "requests.csv"
TEMPLATES_DIR = HERE / "templates"
CONFIG_FILE = HERE / "config.py"

# ---------------------------------------------------------------------------
# Request statuses and their display names
# ---------------------------------------------------------------------------
STATUSES = {
    "draft":                "Draft – not yet sent",
    "sent":                 "Sent – awaiting response",
    "acknowledged":         "Acknowledged – agency confirmed receipt",
    "in_progress":          "In Progress – agency is processing",
    "fee_pending":          "Fee Pending – awaiting cost estimate",
    "fee_paid":             "Fee Paid – waiting for record delivery",
    "ag_opinion_requested": "AG Opinion Requested – agency sought AG ruling",
    "ag_opinion_pending":   "AG Opinion Pending – AG issued opinion; awaiting agency",
    "denied":               "Denied – request refused",
    "partially_complete":   "Partially Complete – some records received",
    "complete":             "Complete – all records received",
    "withdrawn":            "Withdrawn – request retracted",
}

# Templates mapped to human-readable names
TEMPLATES = {
    "01_initial_request":     "Initial TPIA request",
    "02_follow_up":           "Follow-up (approaching/past 10-day deadline)",
    "03_fee_inquiry":         "Response to fee estimate notice",
    "04_ag_opinion_response": "Response when agency seeks AG opinion",
    "05_denial_response":     "Response to denial",
    "06_records_received":    "Acknowledgment of received records",
}

# ---------------------------------------------------------------------------
# Texas state & federal holidays (non-exhaustive; update annually)
# Uses year-based approach for current and next year
# ---------------------------------------------------------------------------
def tx_holidays(year: int) -> set:
    """Return a set of date objects for Texas state holidays in the given year."""
    holidays = set()

    def nth_weekday(year, month, n, weekday):
        """Return the nth occurrence of weekday (0=Mon) in the given month."""
        first = date(year, month, 1)
        delta = (weekday - first.weekday()) % 7
        first_occurrence = first + timedelta(days=delta)
        return first_occurrence + timedelta(weeks=n - 1)

    def last_weekday(year, month, weekday):
        """Return the last occurrence of weekday in the given month."""
        next_month = date(year + 1, 1, 1) if month == 12 else date(year, month + 1, 1)
        last_day = next_month - timedelta(days=1)
        delta = (last_day.weekday() - weekday) % 7
        return last_day - timedelta(days=delta)

    fixed = [
        date(year, 1, 1),   # New Year's Day
        date(year, 3, 2),   # Texas Independence Day
        date(year, 4, 21),  # San Jacinto Day
        date(year, 6, 19),  # Juneteenth
        date(year, 7, 4),   # Independence Day
        date(year, 8, 27),  # Lyndon Baines Johnson Day
        date(year, 11, 11), # Veterans Day
        date(year, 12, 24), # Christmas Eve
        date(year, 12, 25), # Christmas Day
        date(year, 12, 26), # Day after Christmas
    ]

    floating = [
        nth_weekday(year, 1, 3, 0),  # MLK Day – 3rd Monday in Jan
        nth_weekday(year, 2, 3, 0),  # Presidents Day – 3rd Monday in Feb
        last_weekday(year, 5, 0),    # Memorial Day – last Monday in May
        nth_weekday(year, 9, 1, 0),  # Labor Day – 1st Monday in Sep
        nth_weekday(year, 10, 2, 0), # Columbus Day – 2nd Monday in Oct
        nth_weekday(year, 11, 4, 3), # Thanksgiving – 4th Thursday in Nov
        nth_weekday(year, 11, 4, 3) + timedelta(days=1),  # Day after Thanksgiving
    ]

    # Shift Saturday holidays to Friday, Sunday to Monday
    for d in fixed + floating:
        if d.weekday() == 5:   # Saturday
            holidays.add(d - timedelta(days=1))
        elif d.weekday() == 6: # Sunday
            holidays.add(d + timedelta(days=1))
        else:
            holidays.add(d)

    return holidays


def add_business_days(start: date, n: int) -> date:
    """Return the date that is n business days after start, respecting TX holidays."""
    holidays = tx_holidays(start.year) | tx_holidays(start.year + 1)
    current = start
    days_added = 0
    while days_added < n:
        current += timedelta(days=1)
        if current.weekday() < 5 and current not in holidays:
            days_added += 1
    return current


def business_days_elapsed(start: date, end: date) -> int:
    """Count business days between start and end (exclusive of start, inclusive of end)."""
    if end <= start:
        return 0
    holidays = tx_holidays(start.year) | tx_holidays(start.year + 1) | tx_holidays(end.year)
    count = 0
    current = start + timedelta(days=1)
    while current <= end:
        if current.weekday() < 5 and current not in holidays:
            count += 1
        current += timedelta(days=1)
    return count

# ---------------------------------------------------------------------------
# CSV helpers
# ---------------------------------------------------------------------------
REQUESTS_FIELDS = [
    "request_id", "institution_id", "institution", "type", "system_district",
    "city", "recipient_email", "date_sent", "status",
    "deadline_10day",          # 10 business days from date_sent
    "deadline_ag_45day",       # 45 business days from date agency notified AG (if applicable)
    "ag_notified_date",        # date agency notified AG
    "last_updated", "notes"
]


def load_colleges() -> list[dict]:
    with open(COLLEGES_CSV, newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def load_requests() -> list[dict]:
    if not REQUESTS_CSV.exists():
        return []
    with open(REQUESTS_CSV, newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def save_requests(requests: list[dict]):
    with open(REQUESTS_CSV, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=REQUESTS_FIELDS)
        writer.writeheader()
        writer.writerows(requests)


def find_request(requests: list[dict], request_id: str) -> dict | None:
    return next((r for r in requests if r["request_id"] == request_id), None)


def find_college(colleges: list[dict], institution_id: str) -> dict | None:
    return next((c for c in colleges if c["id"] == institution_id), None)

# ---------------------------------------------------------------------------
# Email / config helpers
# ---------------------------------------------------------------------------
def load_config() -> dict:
    """Load SMTP config from config.py (or config.example.py as fallback)."""
    config_path = CONFIG_FILE if CONFIG_FILE.exists() else HERE / "config.example.py"
    if not config_path.exists():
        sys.exit(
            "ERROR: No config.py found. Copy config.example.py → config.py and fill in your details."
        )
    spec = importlib.util.spec_from_file_location("tpia_config", config_path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return {k: v for k, v in vars(module).items() if not k.startswith("_")}


def render_template(template_name: str, variables: dict) -> str:
    """Load a template file and substitute {{VAR}} placeholders."""
    template_path = TEMPLATES_DIR / f"{template_name}.txt"
    if not template_path.exists():
        sys.exit(f"ERROR: Template not found: {template_path}")
    text = template_path.read_text(encoding="utf-8")
    for key, value in variables.items():
        text = text.replace(f"{{{{{key}}}}}", str(value))
    # Warn about any unfilled placeholders
    remaining = re.findall(r"\{\{[A-Z_]+\}\}", text)
    if remaining:
        print(f"  WARNING: Unfilled placeholders in template: {remaining}")
    return text


def send_email(cfg: dict, to_address: str, subject: str, body: str, dry_run: bool = False):
    """Send an email via SMTP. If dry_run, just print it."""
    if dry_run:
        print("\n" + "=" * 70)
        print(f"DRY RUN — would send to: {to_address}")
        print(f"Subject: {subject}")
        print("-" * 70)
        print(body)
        print("=" * 70 + "\n")
        return

    msg = MIMEMultipart("alternative")
    msg["From"] = cfg["SENDER_EMAIL"]
    msg["To"] = to_address
    msg["Subject"] = subject
    if cfg.get("BCC"):
        msg["Bcc"] = cfg["BCC"]
    msg.attach(MIMEText(body, "plain", "utf-8"))

    with smtplib.SMTP_SSL(cfg["SMTP_HOST"], cfg.get("SMTP_PORT", 465)) as server:
        server.login(cfg["SMTP_USER"], cfg["SMTP_PASSWORD"])
        recipients = [to_address]
        if cfg.get("BCC"):
            recipients.append(cfg["BCC"])
        server.sendmail(cfg["SENDER_EMAIL"], recipients, msg.as_string())
    print(f"  ✓ Email sent to {to_address}")

# ---------------------------------------------------------------------------
# Commands
# ---------------------------------------------------------------------------
def cmd_init(args):
    """Initialize requests.csv from colleges.csv."""
    colleges = load_colleges()
    existing = {r["institution_id"]: r for r in load_requests()}

    rows = []
    added = 0
    for c in colleges:
        if c["id"] in existing:
            rows.append(existing[c["id"]])
        else:
            rows.append({
                "request_id":      c["id"],
                "institution_id":  c["id"],
                "institution":     c["institution"],
                "type":            c["type"],
                "system_district": c["system_district"],
                "city":            c["city"],
                "recipient_email": c["public_records_email"],
                "date_sent":       "",
                "status":          "draft",
                "deadline_10day":  "",
                "deadline_ag_45day": "",
                "ag_notified_date": "",
                "last_updated":    str(date.today()),
                "notes":           "",
            })
            added += 1

    save_requests(rows)
    print(f"Initialized {REQUESTS_CSV.name}: {len(rows)} institutions "
          f"({added} newly added, {len(existing)} already present).")


def cmd_status(args):
    """Print a status dashboard."""
    requests = load_requests()
    if not requests:
        print("No requests found. Run `init` first.")
        return

    counts = {}
    for r in requests:
        counts[r["status"]] = counts.get(r["status"], 0) + 1

    total = len(requests)
    print(f"\n{'=' * 60}")
    print(f"  TPIA REQUEST STATUS DASHBOARD  ({date.today()})")
    print(f"{'=' * 60}")
    print(f"  Total institutions: {total}")
    print()
    for status_key, label in STATUSES.items():
        n = counts.get(status_key, 0)
        bar = "█" * n
        print(f"  {label:<45} {n:>3}  {bar}")
    print(f"{'=' * 60}\n")


def cmd_deadlines(args):
    """Show upcoming deadlines and overdue requests."""
    requests = load_requests()
    today = date.today()
    warning_window = timedelta(days=3)  # warn 3 calendar days before deadline

    overdue = []
    upcoming = []
    ag_overdue = []
    ag_upcoming = []

    for r in requests:
        if r["status"] in ("draft", "complete", "denied", "withdrawn", "partially_complete"):
            continue

        if r["deadline_10day"]:
            d = date.fromisoformat(r["deadline_10day"])
            if d < today:
                overdue.append((r, d, "10-day response deadline"))
            elif d - today <= warning_window:
                upcoming.append((r, d, "10-day response deadline"))

        if r["deadline_ag_45day"] and r["status"] == "ag_opinion_requested":
            d = date.fromisoformat(r["deadline_ag_45day"])
            if d < today:
                ag_overdue.append((r, d, "45-day AG opinion deadline"))
            elif d - today <= warning_window:
                ag_upcoming.append((r, d, "45-day AG opinion deadline"))

    def print_group(label, items):
        if not items:
            return
        print(f"\n  {'— ' + label + ' —':^58}")
        for r, d, dtype in sorted(items, key=lambda x: x[1]):
            delta = (today - d).days if d < today else (d - today).days
            direction = "overdue by" if d < today else "due in"
            print(f"  [{r['request_id']}] {r['institution'][:40]:<40}  "
                  f"{dtype} — {direction} {delta}d  ({d})")

    print(f"\n{'=' * 60}")
    print(f"  DEADLINES  ({today})")
    print(f"{'=' * 60}")
    print_group("OVERDUE", overdue)
    print_group("OVERDUE – AG", ag_overdue)
    print_group("UPCOMING (within 3 days)", upcoming)
    print_group("UPCOMING – AG (within 3 days)", ag_upcoming)
    if not any([overdue, ag_overdue, upcoming, ag_upcoming]):
        print("  No upcoming or overdue deadlines.")
    print(f"{'=' * 60}\n")


def cmd_send(args):
    """Send an initial TPIA request to one institution."""
    requests = load_requests()
    r = find_request(requests, args.id)
    if not r:
        sys.exit(f"ERROR: No request found with id '{args.id}'. Run `init` first.")

    if r["status"] != "draft" and not args.force:
        sys.exit(
            f"ERROR: Request {args.id} has status '{r['status']}'. "
            "Use --force to resend, or check the status first."
        )

    cfg = load_config()
    variables = {
        "INSTITUTION":      r["institution"],
        "CITY":             r["city"],
        "SENDER_NAME":      cfg.get("SENDER_NAME", ""),
        "SENDER_TITLE":     cfg.get("SENDER_TITLE", ""),
        "SENDER_ORG":       cfg.get("SENDER_ORG", ""),
        "SENDER_EMAIL":     cfg.get("SENDER_EMAIL", ""),
        "SENDER_PHONE":     cfg.get("SENDER_PHONE", ""),
        "SENDER_ADDRESS":   cfg.get("SENDER_ADDRESS", ""),
        "TODAY":            str(date.today()),
        "DEADLINE_DATE":    str(add_business_days(date.today(), 10)),
    }

    body = render_template("01_initial_request", variables)
    subject = f"Texas Public Information Act Request – Student Directory Information – {date.today()}"

    to_email = r["recipient_email"]
    if not to_email:
        print(f"WARNING: No email address for {r['institution']}. Skipping send.")
        return

    send_email(cfg, to_email, subject, body, dry_run=args.dry_run)

    if not args.dry_run:
        today_str = str(date.today())
        r["date_sent"] = today_str
        r["status"] = "sent"
        r["deadline_10day"] = str(add_business_days(date.today(), 10))
        r["last_updated"] = today_str
        save_requests(requests)
        print(f"  Request marked as 'sent'. 10-day deadline: {r['deadline_10day']}")


def cmd_send_all(args):
    """Send initial requests to all draft institutions."""
    requests = load_requests()
    drafts = [r for r in requests if r["status"] == "draft"]

    if not drafts:
        print("No draft requests to send.")
        return

    print(f"Found {len(drafts)} draft requests.")
    if not args.dry_run:
        confirm = input(f"Send all {len(drafts)} requests? [y/N] ").strip().lower()
        if confirm != "y":
            print("Aborted.")
            return

    cfg = load_config()
    for r in drafts:
        print(f"  Sending to {r['institution']} ({r['recipient_email']})...")
        variables = {
            "INSTITUTION":      r["institution"],
            "CITY":             r["city"],
            "SENDER_NAME":      cfg.get("SENDER_NAME", ""),
            "SENDER_TITLE":     cfg.get("SENDER_TITLE", ""),
            "SENDER_ORG":       cfg.get("SENDER_ORG", ""),
            "SENDER_EMAIL":     cfg.get("SENDER_EMAIL", ""),
            "SENDER_PHONE":     cfg.get("SENDER_PHONE", ""),
            "SENDER_ADDRESS":   cfg.get("SENDER_ADDRESS", ""),
            "TODAY":            str(date.today()),
            "DEADLINE_DATE":    str(add_business_days(date.today(), 10)),
        }
        body = render_template("01_initial_request", variables)
        subject = (
            f"Texas Public Information Act Request – Student Directory Information – {date.today()}"
        )
        to_email = r["recipient_email"]
        if not to_email:
            print(f"    WARNING: No email for {r['institution']}. Skipping.")
            continue

        send_email(cfg, to_email, subject, body, dry_run=args.dry_run)

        if not args.dry_run:
            today_str = str(date.today())
            r["date_sent"] = today_str
            r["status"] = "sent"
            r["deadline_10day"] = str(add_business_days(date.today(), 10))
            r["last_updated"] = today_str

    if not args.dry_run:
        save_requests(requests)
        print(f"Done. {len(drafts)} requests sent.")


def cmd_update(args):
    """Update status of a request."""
    if args.status not in STATUSES:
        sys.exit(
            f"ERROR: Unknown status '{args.status}'.\nValid statuses: {', '.join(STATUSES.keys())}"
        )

    requests = load_requests()
    r = find_request(requests, args.id)
    if not r:
        sys.exit(f"ERROR: No request found with id '{args.id}'.")

    old_status = r["status"]
    r["status"] = args.status
    r["last_updated"] = str(date.today())

    if args.note:
        existing_note = r.get("notes", "")
        timestamp = str(date.today())
        r["notes"] = f"{existing_note} | [{timestamp}] {args.note}".strip(" |")

    # Auto-set AG deadline when transitioning to ag_opinion_requested
    if args.status == "ag_opinion_requested" and not r.get("ag_notified_date"):
        r["ag_notified_date"] = str(date.today())
        r["deadline_ag_45day"] = str(add_business_days(date.today(), 45))
        print(f"  AG 45-day deadline set: {r['deadline_ag_45day']}")

    save_requests(requests)
    print(f"  [{args.id}] {r['institution']}: {old_status} → {args.status}")


def cmd_followup(args):
    """Send follow-up emails to institutions past their 10-day deadline."""
    requests = load_requests()
    today = date.today()

    overdue = [
        r for r in requests
        if r["status"] == "sent"
        and r["deadline_10day"]
        and date.fromisoformat(r["deadline_10day"]) < today
    ]

    if not overdue:
        print("No overdue requests requiring follow-up.")
        return

    print(f"Found {len(overdue)} overdue requests.")
    cfg = load_config()

    for r in overdue:
        elapsed = business_days_elapsed(date.fromisoformat(r["date_sent"]), today)
        print(f"  Following up with {r['institution']} ({elapsed} business days elapsed)...")
        variables = {
            "INSTITUTION":      r["institution"],
            "CITY":             r["city"],
            "DATE_SENT":        r["date_sent"],
            "DEADLINE_DATE":    r["deadline_10day"],
            "BUSINESS_DAYS_ELAPSED": str(elapsed),
            "SENDER_NAME":      cfg.get("SENDER_NAME", ""),
            "SENDER_TITLE":     cfg.get("SENDER_TITLE", ""),
            "SENDER_ORG":       cfg.get("SENDER_ORG", ""),
            "SENDER_EMAIL":     cfg.get("SENDER_EMAIL", ""),
            "SENDER_PHONE":     cfg.get("SENDER_PHONE", ""),
            "TODAY":            str(today),
        }
        body = render_template("02_follow_up", variables)
        subject = (
            f"Follow-Up: Texas Public Information Act Request – {r['institution']} – {today}"
        )
        send_email(cfg, r["recipient_email"], subject, body, dry_run=args.dry_run)

        if not args.dry_run:
            note = f"Follow-up sent ({today})"
            r["notes"] = (r.get("notes", "") + f" | [{today}] {note}").strip(" |")
            r["last_updated"] = str(today)

    if not args.dry_run:
        save_requests(requests)


def cmd_show_template(args):
    """Print a template to stdout."""
    name = args.template_name
    if not name.endswith(".txt"):
        name_path = TEMPLATES_DIR / f"{name}.txt"
    else:
        name_path = TEMPLATES_DIR / name

    if not name_path.exists():
        available = [p.stem for p in TEMPLATES_DIR.glob("*.txt")]
        sys.exit(
            f"ERROR: Template '{args.template_name}' not found.\n"
            f"Available templates: {', '.join(sorted(available))}"
        )
    print(name_path.read_text(encoding="utf-8"))


def cmd_log(args):
    """Show all info for a single request."""
    requests = load_requests()
    r = find_request(requests, args.id)
    if not r:
        sys.exit(f"ERROR: No request found with id '{args.id}'.")

    print(f"\n{'=' * 60}")
    print(f"  REQUEST: {r['request_id']}")
    print(f"{'=' * 60}")
    for k, v in r.items():
        print(f"  {k:<25} {v}")
    print(f"{'=' * 60}\n")

# ---------------------------------------------------------------------------
# CLI wiring
# ---------------------------------------------------------------------------
def main():
    parser = argparse.ArgumentParser(
        description="TPIA Public Records Request Tracker for Texas colleges",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    sub = parser.add_subparsers(dest="command", metavar="<command>")

    # init
    p_init = sub.add_parser("init", help="Initialize requests.csv from colleges.csv")
    p_init.set_defaults(func=cmd_init)

    # status
    p_status = sub.add_parser("status", help="Show status dashboard")
    p_status.set_defaults(func=cmd_status)

    # deadlines
    p_deadlines = sub.add_parser("deadlines", help="Show upcoming and overdue deadlines")
    p_deadlines.set_defaults(func=cmd_deadlines)

    # send
    p_send = sub.add_parser("send", help="Send initial TPIA request to one institution")
    p_send.add_argument("id", help="Request/institution ID (e.g. UT001 or CC018)")
    p_send.add_argument("--dry-run", action="store_true", help="Preview email without sending")
    p_send.add_argument("--force",   action="store_true", help="Resend even if already sent")
    p_send.set_defaults(func=cmd_send)

    # send-all
    p_send_all = sub.add_parser("send-all", help="Send initial requests to all draft institutions")
    p_send_all.add_argument("--dry-run", action="store_true", help="Preview without sending")
    p_send_all.set_defaults(func=cmd_send_all)

    # update
    p_update = sub.add_parser("update", help="Update the status of a request")
    p_update.add_argument("id",     help="Request ID")
    p_update.add_argument("status", help=f"New status. One of: {', '.join(STATUSES.keys())}")
    p_update.add_argument("note",   nargs="?", default="", help="Optional note to append")
    p_update.set_defaults(func=cmd_update)

    # followup
    p_followup = sub.add_parser("followup", help="Send follow-ups to overdue requests")
    p_followup.add_argument("--dry-run", action="store_true", help="Preview without sending")
    p_followup.set_defaults(func=cmd_followup)

    # show-template
    p_tmpl = sub.add_parser("show-template", help="Display a template")
    p_tmpl.add_argument("template_name", help=f"Template name. One of: {', '.join(TEMPLATES.keys())}")
    p_tmpl.set_defaults(func=cmd_show_template)

    # log
    p_log = sub.add_parser("log", help="Show all details for a request")
    p_log.add_argument("id", help="Request ID")
    p_log.set_defaults(func=cmd_log)

    args = parser.parse_args()
    if not args.command:
        parser.print_help()
        sys.exit(0)

    args.func(args)


if __name__ == "__main__":
    main()
