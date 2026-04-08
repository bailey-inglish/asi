#!/usr/bin/env python3
"""
app.py — TPIA Tracker (Streamlit GUI)
======================================
Graphical interface for tracking Texas Public Information Act requests
sent to public colleges and universities as part of the ASI voter outreach
research project.

Run with:
    streamlit run app.py
"""

import json
import re
import urllib.parse
from datetime import date, timedelta
from pathlib import Path

import pandas as pd
import streamlit as st

# ── Paths ─────────────────────────────────────────────────────────────────────
HERE          = Path(__file__).parent.resolve()
COLLEGES_CSV  = HERE / "colleges.csv"
REQUESTS_CSV  = HERE / "requests.csv"
TEMPLATES_DIR = HERE / "templates"
SENDER_FILE   = HERE / "sender.json"   # gitignored

# ── Status definitions ────────────────────────────────────────────────────────
# (key, display label, hex color, description)
STATUSES = {
    "draft":                ("⬜ Draft",                "#757575", "Not yet sent"),
    "sent":                 ("📤 Sent",                 "#1565C0", "Awaiting response; 10-day clock running"),
    "acknowledged":         ("👀 Acknowledged",         "#6A1B9A", "Agency confirmed receipt"),
    "in_progress":          ("⚙️ In Progress",           "#E65100", "Agency is actively processing"),
    "fee_pending":          ("💵 Fee Pending",           "#BF360C", "Agency sent cost estimate; awaiting decision"),
    "fee_paid":             ("💳 Fee Paid",              "#4E342E", "Payment sent; waiting for records"),
    "ag_opinion_requested": ("⚖️ AG Opinion Req'd",     "#B71C1C", "Agency is seeking AG ruling; 45-day clock running"),
    "ag_opinion_pending":   ("📋 AG Opinion Pending",   "#880E4F", "AG ruled; waiting for agency to comply"),
    "denied":               ("🚫 Denied",               "#C62828", "Request was refused"),
    "partially_complete":   ("📂 Partially Complete",   "#2E7D32", "Some records received; some withheld"),
    "complete":             ("✅ Complete",             "#1B5E20", "All records received"),
    "withdrawn":            ("↩️ Withdrawn",             "#546E7A", "Request retracted"),
}

STATUS_KEYS = list(STATUSES.keys())

# Row background colors for table highlighting
STATUS_BG = {
    "draft":                "#F5F5F5",
    "sent":                 "#E3F2FD",
    "acknowledged":         "#F3E5F5",
    "in_progress":          "#FFF3E0",
    "fee_pending":          "#FBE9E7",
    "fee_paid":             "#EFEBE9",
    "ag_opinion_requested": "#FFEBEE",
    "ag_opinion_pending":   "#FCE4EC",
    "denied":               "#FFCDD2",
    "partially_complete":   "#E8F5E9",
    "complete":             "#C8E6C9",
    "withdrawn":            "#ECEFF1",
}

# Which template to pre-select for each status
SUGGESTED_TEMPLATE = {
    "draft":                "01_initial_request",
    "sent":                 "02_follow_up",
    "acknowledged":         "02_follow_up",
    "in_progress":          "02_follow_up",
    "fee_pending":          "03_fee_inquiry",
    "fee_paid":             "02_follow_up",
    "ag_opinion_requested": "04_ag_opinion_response",
    "ag_opinion_pending":   "02_follow_up",
    "denied":               "05_denial_response",
    "partially_complete":   "06_records_received",
    "complete":             "06_records_received",
    "withdrawn":            "01_initial_request",
}

TEMPLATE_LABELS = {
    "01_initial_request":     "📨 Initial TPIA Request",
    "02_follow_up":           "🔔 Follow-Up (10-day deadline)",
    "03_fee_inquiry":         "💵 Fee Estimate Response",
    "04_ag_opinion_response": "⚖️ AG Opinion Response",
    "05_denial_response":     "🚫 Denial Response",
    "06_records_received":    "📂 Records Received Acknowledgment",
}

REQUESTS_FIELDS = [
    "request_id", "institution_id", "institution", "type", "system_district",
    "city", "recipient_email", "date_sent", "status",
    "deadline_10day", "deadline_ag_45day", "ag_notified_date",
    "last_updated", "notes",
]

# ── Business day calculator ───────────────────────────────────────────────────

def tx_holidays(year: int) -> set:
    """Return set of Texas state holidays for the given year."""
    holidays = set()

    def nth_weekday(y, m, n, wd):
        first = date(y, m, 1)
        delta = (wd - first.weekday()) % 7
        return first + timedelta(days=delta) + timedelta(weeks=n - 1)

    def last_weekday(y, m, wd):
        nm = date(y + 1, 1, 1) if m == 12 else date(y, m + 1, 1)
        last = nm - timedelta(days=1)
        return last - timedelta(days=(last.weekday() - wd) % 7)

    fixed = [
        date(year, 1, 1),   # New Year's Day
        date(year, 3, 2),   # Texas Independence Day
        date(year, 4, 21),  # San Jacinto Day
        date(year, 6, 19),  # Juneteenth
        date(year, 7, 4),   # Independence Day
        date(year, 8, 27),  # LBJ Day
        date(year, 11, 11), # Veterans Day
        date(year, 12, 24), # Christmas Eve
        date(year, 12, 25), # Christmas Day
        date(year, 12, 26), # Day after Christmas
    ]
    floating = [
        nth_weekday(year, 1, 3, 0),                               # MLK Day
        nth_weekday(year, 2, 3, 0),                               # Presidents Day
        last_weekday(year, 5, 0),                                 # Memorial Day
        nth_weekday(year, 9, 1, 0),                               # Labor Day
        nth_weekday(year, 10, 2, 0),                              # Columbus Day
        nth_weekday(year, 11, 4, 3),                              # Thanksgiving
        nth_weekday(year, 11, 4, 3) + timedelta(days=1),          # Day after Thanksgiving
    ]
    for d in fixed + floating:
        if d.weekday() == 5:
            holidays.add(d - timedelta(days=1))
        elif d.weekday() == 6:
            holidays.add(d + timedelta(days=1))
        else:
            holidays.add(d)
    return holidays


def add_business_days(start: date, n: int) -> date:
    holidays = tx_holidays(start.year) | tx_holidays(start.year + 1)
    cur, added = start, 0
    while added < n:
        cur += timedelta(days=1)
        if cur.weekday() < 5 and cur not in holidays:
            added += 1
    return cur


def business_days_elapsed(start: date, end: date) -> int:
    if end <= start:
        return 0
    holidays = tx_holidays(start.year) | tx_holidays(end.year)
    count, cur = 0, start + timedelta(days=1)
    while cur <= end:
        if cur.weekday() < 5 and cur not in holidays:
            count += 1
        cur += timedelta(days=1)
    return count

# ── Data helpers ──────────────────────────────────────────────────────────────

@st.cache_data
def load_colleges() -> pd.DataFrame:
    return pd.read_csv(COLLEGES_CSV, dtype=str).fillna("")


def load_requests() -> pd.DataFrame:
    if not REQUESTS_CSV.exists():
        return pd.DataFrame(columns=REQUESTS_FIELDS)
    return pd.read_csv(REQUESTS_CSV, dtype=str).fillna("")


def save_requests(df: pd.DataFrame):
    df.to_csv(REQUESTS_CSV, index=False)


def save_colleges(df: pd.DataFrame):
    df.to_csv(COLLEGES_CSV, index=False)


def college_to_request_row(college_row: pd.Series) -> dict:
    return {
        "request_id":        str(college_row.get("id", "")),
        "institution_id":    str(college_row.get("id", "")),
        "institution":       str(college_row.get("institution", "")),
        "type":              str(college_row.get("type", "")),
        "system_district":   str(college_row.get("system_district", "")),
        "city":              str(college_row.get("city", "")),
        "recipient_email":   str(college_row.get("public_records_email", "")),
        "date_sent":         "",
        "status":            "draft",
        "deadline_10day":    "",
        "deadline_ag_45day": "",
        "ag_notified_date":  "",
        "last_updated":      str(date.today()),
        "notes":             "",
    }


def sync_request_from_college(requests_df: pd.DataFrame, college_row: pd.Series) -> pd.DataFrame:
    request_id = str(college_row.get("id", ""))
    if not request_id:
        return requests_df

    mapped = {
        "institution":     str(college_row.get("institution", "")),
        "type":            str(college_row.get("type", "")),
        "system_district": str(college_row.get("system_district", "")),
        "city":            str(college_row.get("city", "")),
        "recipient_email": str(college_row.get("public_records_email", "")),
        "last_updated":    str(date.today()),
    }

    mask = requests_df["request_id"] == request_id
    if mask.any():
        for key, value in mapped.items():
            requests_df.loc[mask, key] = value
        requests_df.loc[mask, "institution_id"] = request_id
    else:
        requests_df = pd.concat(
            [requests_df, pd.DataFrame([college_to_request_row(college_row)])],
            ignore_index=True,
        )

    return requests_df


def ensure_requests_initialized() -> pd.DataFrame:
    """Create requests.csv from colleges.csv if it doesn't exist or is missing rows."""
    colleges = load_colleges()
    existing = load_requests()
    existing_ids = set(existing["request_id"].tolist()) if len(existing) else set()

    new_rows = [
        {
            "request_id":        c["id"],
            "institution_id":    c["id"],
            "institution":       c["institution"],
            "type":              c["type"],
            "system_district":   c["system_district"],
            "city":              c["city"],
            "recipient_email":   c["public_records_email"],
            "date_sent":         "",
            "status":            "draft",
            "deadline_10day":    "",
            "deadline_ag_45day": "",
            "ag_notified_date":  "",
            "last_updated":      str(date.today()),
            "notes":             "",
        }
        for _, c in colleges.iterrows()
        if c["id"] not in existing_ids
    ]

    if new_rows:
        added = pd.DataFrame(new_rows)
        result = pd.concat([existing, added], ignore_index=True) if len(existing) else added
        save_requests(result)
        return result

    return existing

# ── Sender info ───────────────────────────────────────────────────────────────

def load_sender() -> dict:
    if SENDER_FILE.exists():
        with open(SENDER_FILE) as f:
            return json.load(f)
    return {
        "name":    "",
        "title":   "",
        "org":     "The Annette Strauss Institute for Civic Life, The University of Texas at Austin",
        "email":   "",
        "phone":   "",
        "address": "2504 Whitis Ave, Austin, TX 78712",
    }


def save_sender(info: dict):
    with open(SENDER_FILE, "w") as f:
        json.dump(info, f, indent=2)

# ── Template rendering ────────────────────────────────────────────────────────

def render_template(template_name: str, variables: dict) -> str:
    path = TEMPLATES_DIR / f"{template_name}.txt"
    if not path.exists():
        return f"[Template file not found: {template_name}.txt]"
    text = path.read_text(encoding="utf-8")
    for k, v in variables.items():
        text = text.replace(f"{{{{{k}}}}}", str(v))
    # Flag any unfilled placeholders
    remaining = re.findall(r"\{\{[A-Z_]+\}\}", text)
    if remaining:
        text += f"\n\n⚠️ Unfilled placeholders: {', '.join(remaining)}"
    return text


def build_template_variables(row: pd.Series, sender: dict) -> dict:
    today = date.today()
    raw_sent = str(row.get("date_sent", "") or "")
    try:
        date_sent = date.fromisoformat(raw_sent) if raw_sent else today
    except ValueError:
        date_sent = today

    return {
        "INSTITUTION":            str(row.get("institution", "")),
        "CITY":                   str(row.get("city", "")),
        "SENDER_NAME":            sender.get("name", "[YOUR NAME]"),
        "SENDER_TITLE":           sender.get("title", "[YOUR TITLE]"),
        "SENDER_ORG":             sender.get("org", "[YOUR ORGANIZATION]"),
        "SENDER_EMAIL":           sender.get("email", "[YOUR EMAIL]"),
        "SENDER_PHONE":           sender.get("phone", "[YOUR PHONE]"),
        "SENDER_ADDRESS":         sender.get("address", "[YOUR ADDRESS]"),
        "TODAY":                  str(today),
        "DATE_SENT":              str(date_sent),
        "DEADLINE_DATE":          str(add_business_days(date_sent, 10)),
        "BUSINESS_DAYS_ELAPSED":  str(business_days_elapsed(date_sent, today)),
        "AG_LETTER_NUMBER":       "[AG LETTER NO. — check AG notice]",
        "FEE_AMOUNT":             "[FEE AMOUNT — from agency's notice]",
        "PAYMENT_METHOD":         "[CHECK / CREDIT CARD / ONLINE PORTAL]",
        "DENIAL_BASIS":           "[CITED EXCEPTION — from agency's denial letter]",
    }


SUBJECT_LINES = {
    "01_initial_request":     "Texas Public Information Act Request – Student Directory Information",
    "02_follow_up":           "Follow-Up: TPIA Request – {institution}",
    "03_fee_inquiry":         "Re: Fee Estimate – TPIA Request – {institution}",
    "04_ag_opinion_response": "Re: AG Opinion Notice – TPIA Request – {institution}",
    "05_denial_response":     "Re: Denial – TPIA Request – {institution}",
    "06_records_received":    "Confirmation: Records Received – {institution}",
}


def get_subject(template_name: str, institution: str) -> str:
    base = SUBJECT_LINES.get(template_name, f"TPIA Request – {institution}")
    return base.replace("{institution}", institution)

# ── Deadline display helpers ──────────────────────────────────────────────────

def deadline_badge(deadline_str: str) -> str:
    if not deadline_str or not deadline_str.strip():
        return ""
    try:
        d = date.fromisoformat(deadline_str.strip())
    except ValueError:
        return ""
    delta = (d - date.today()).days
    if delta < 0:
        return f'<span style="color:#B71C1C;font-weight:bold">⚠️ {abs(delta)} day(s) OVERDUE</span>'
    elif delta == 0:
        return '<span style="color:#E65100;font-weight:bold">🔴 Due TODAY</span>'
    elif delta <= 3:
        return f'<span style="color:#E65100;font-weight:bold">🟠 {delta} day(s) remaining</span>'
    else:
        return f'<span style="color:#2E7D32">🟢 {delta} day(s) remaining</span>'


def is_overdue(deadline_str: str) -> bool:
    if not deadline_str or not deadline_str.strip():
        return False
    try:
        return date.fromisoformat(deadline_str.strip()) < date.today()
    except ValueError:
        return False

# ── Page: main app ────────────────────────────────────────────────────────────

def main():
    st.set_page_config(
        page_title="TPIA Tracker — ASI",
        page_icon="🏛️",
        layout="wide",
        initial_sidebar_state="expanded",
        menu_items={"About": "ASI TPIA Student Records Tracker. Annette Strauss Institute, UT Austin."},
    )

    # Custom CSS
    st.markdown("""
    <style>
        .main .block-container { padding-top: 1.5rem; padding-bottom: 2rem; }
        .stTabs [data-baseweb="tab"] { font-size: 0.95rem; }
        .metric-card {
            background: white; border-radius: 10px; padding: 14px 18px;
            box-shadow: 0 1px 4px rgba(0,0,0,0.12); text-align: center;
        }
        .metric-card .label { font-size: 0.78rem; color: #666; text-transform: uppercase; letter-spacing: 0.04em; }
        .metric-card .value { font-size: 2rem; font-weight: 700; line-height: 1.1; }
        .institution-header { border-left: 5px solid; padding: 10px 14px; border-radius: 0 8px 8px 0; margin-bottom: 1rem; }
        div[data-testid="stDataFrame"] { border-radius: 8px; overflow: hidden; }
        .status-pill {
            display: inline-block; padding: 2px 10px; border-radius: 12px;
            font-size: 0.82em; font-weight: 600; white-space: nowrap;
        }
        .quick-btn-row { display: flex; gap: 8px; flex-wrap: wrap; margin-bottom: 0.5rem; }
    </style>
    """, unsafe_allow_html=True)

    # ── Initialize data ────────────────────────────────────────────────────────
    if not REQUESTS_CSV.exists():
        ensure_requests_initialized()

    df = load_requests()
    if df.empty:
        df = ensure_requests_initialized()

    # ── Sidebar ────────────────────────────────────────────────────────────────
    with st.sidebar:
        st.markdown("## 🏛️ TPIA Tracker")
        st.markdown("*ASI Student Records Project*")
        st.divider()

        # Sender info
        st.markdown("### 👤 Your Info")
        st.caption("Used to auto-fill email templates.")
        sender = load_sender()
        s_name    = st.text_input("Full Name",     value=sender.get("name", ""),    key="s_name")
        s_title   = st.text_input("Title/Role",    value=sender.get("title", ""),   key="s_title")
        s_org     = st.text_input("Organization",  value=sender.get("org", ""),     key="s_org")
        s_email   = st.text_input("Your Email",    value=sender.get("email", ""),   key="s_email")
        s_phone   = st.text_input("Phone",         value=sender.get("phone", ""),   key="s_phone")
        s_address = st.text_input("Mailing Address", value=sender.get("address", ""), key="s_address")

        if st.button("💾 Save Info", use_container_width=True):
            save_sender({
                "name": s_name, "title": s_title, "org": s_org,
                "email": s_email, "phone": s_phone, "address": s_address,
            })
            st.success("✓ Saved!")
            st.rerun()

        st.divider()

        # Filters
        st.markdown("### 🔍 Filters")
        filt_type = st.multiselect(
            "Institution type", ["4yr", "2yr"],
            default=["4yr", "2yr"], key="filt_type",
        )
        filt_status = st.multiselect(
            "Status", STATUS_KEYS,
            default=STATUS_KEYS, key="filt_status",
            format_func=lambda x: STATUSES[x][0],
        )

        all_systems = sorted(df["system_district"].dropna().unique().tolist())
        filt_system = st.multiselect(
            "System / District", all_systems,
            default=all_systems, key="filt_system",
        )

        search_term = st.text_input("🔎 Search institution name", key="search")

        st.divider()
        if st.button("🔄 Refresh", use_container_width=True):
            st.cache_data.clear()
            st.rerun()

    # Current sender (with live sidebar values)
    current_sender = {
        "name": s_name, "title": s_title, "org": s_org,
        "email": s_email, "phone": s_phone, "address": s_address,
    }

    # ── Summary metrics ────────────────────────────────────────────────────────
    today = date.today()

    counts = df["status"].value_counts().to_dict()
    active_statuses = {"sent", "acknowledged", "in_progress", "fee_pending",
                       "fee_paid", "ag_opinion_requested", "ag_opinion_pending"}
    n_active   = sum(counts.get(s, 0) for s in active_statuses)
    n_complete = counts.get("complete", 0) + counts.get("partially_complete", 0)
    n_denied   = counts.get("denied", 0)
    n_draft    = counts.get("draft", 0)

    # Overdue check
    overdue_10 = df[
        df["status"].isin({"sent", "acknowledged", "in_progress", "fee_pending", "fee_paid"}) &
        df["deadline_10day"].apply(is_overdue)
    ]
    overdue_ag = df[
        df["status"].isin({"ag_opinion_requested", "ag_opinion_pending"}) &
        df["deadline_ag_45day"].apply(is_overdue)
    ]
    n_overdue = len(overdue_10) + len(overdue_ag)

    c1, c2, c3, c4, c5, c6 = st.columns(6)
    def metric_card(col, label, value, color="#1565C0"):
        col.markdown(
            f'<div class="metric-card"><div class="label">{label}</div>'
            f'<div class="value" style="color:{color}">{value}</div></div>',
            unsafe_allow_html=True,
        )
    metric_card(c1, "Total",    len(df),     "#37474F")
    metric_card(c2, "Active",   n_active,    "#1565C0")
    metric_card(c3, "Complete", n_complete,  "#2E7D32")
    metric_card(c4, "Denied",   n_denied,    "#C62828")
    metric_card(c5, "Draft",    n_draft,     "#757575")
    metric_card(c6, "⚠️ Overdue", n_overdue, "#B71C1C" if n_overdue else "#2E7D32")

    st.markdown("<br>", unsafe_allow_html=True)

    # Overdue alert
    if n_overdue > 0:
        overdue_names = (
            list(overdue_10["institution"].head(3).values) +
            list(overdue_ag["institution"].head(2).values)
        )
        st.error(
            f"⚠️ **{n_overdue} overdue request(s):** "
            f"{', '.join(overdue_names[:4])}{'...' if len(overdue_names) > 4 else ''}  \n"
            "Select one below and send a follow-up or update its status."
        )

    st.divider()

    # ── Institution table ──────────────────────────────────────────────────────
    filtered = df.copy()
    if filt_type:
        filtered = filtered[filtered["type"].isin(filt_type)]
    if filt_status:
        filtered = filtered[filtered["status"].isin(filt_status)]
    if filt_system:
        filtered = filtered[filtered["system_district"].isin(filt_system)]
    if search_term:
        filtered = filtered[filtered["institution"].str.contains(search_term, case=False, na=False)]

    st.markdown(f"### 📋 Institutions — {len(filtered)} shown")

    # Build display table
    def fmt_status(s):
        label, color, _ = STATUSES.get(s, ("?", "#999", ""))
        return f'<span class="status-pill" style="background:{color};color:white">{label}</span>'

    def fmt_deadline(d_str, status):
        if not d_str or not d_str.strip():
            return "—"
        try:
            d = date.fromisoformat(d_str.strip())
        except ValueError:
            return d_str
        delta = (d - today).days
        if delta < 0:
            return f"⚠️ {d} ({abs(delta)}d over)"
        elif delta <= 3:
            return f"🟠 {d} ({delta}d left)"
        return str(d)

    tbl = filtered[["request_id", "institution", "type", "system_district",
                     "city", "status", "deadline_10day", "last_updated"]].copy()
    tbl.columns = ["ID", "Institution", "Type", "System/District",
                   "City", "Status", "10-Day Deadline", "Updated"]

    # Color rows by status
    def row_color(row):
        orig_status = filtered.loc[filtered["request_id"] == row.get("ID", ""), "status"]
        if len(orig_status):
            bg = STATUS_BG.get(orig_status.iloc[0], "white")
            return [f"background-color: {bg}"] * len(row)
        return [""] * len(row)

    st.dataframe(
        tbl,
        use_container_width=True,
        hide_index=True,
        height=min(60 + 35 * len(tbl), 350),
        column_config={
            "ID":            st.column_config.TextColumn("ID", width=80),
            "Institution":   st.column_config.TextColumn("Institution", width=250),
            "Type":          st.column_config.TextColumn("Type", width=55),
            "System/District": st.column_config.TextColumn("System/District", width=220),
            "City":          st.column_config.TextColumn("City", width=100),
            "Status":        st.column_config.TextColumn("Status", width=160),
            "10-Day Deadline": st.column_config.TextColumn("10-Day Deadline", width=130),
            "Updated":       st.column_config.TextColumn("Updated", width=100),
        },
    )

    st.divider()

    # ── Institution detail ─────────────────────────────────────────────────────
    st.markdown("### 🏫 Select Institution to View / Update")

    if filtered.empty:
        st.info("No institutions match your current filters.")
        return

    inst_options = filtered["request_id"].tolist()

    # Highlight overdue at top
    overdue_ids = set(overdue_10["request_id"].tolist()) | set(overdue_ag["request_id"].tolist())
    overdue_in_view = [i for i in inst_options if i in overdue_ids]
    non_overdue = [i for i in inst_options if i not in overdue_ids]
    ordered = overdue_in_view + non_overdue

    def fmt_option(rid):
        row = df[df["request_id"] == rid]
        if row.empty:
            return rid
        status = row.iloc[0]["status"]
        inst   = row.iloc[0]["institution"]
        label, _, _ = STATUSES.get(status, ("?", "#999", ""))
        prefix = "⚠️ " if rid in overdue_ids else ""
        return f"{prefix}{rid} — {inst}  [{label}]"

    selected_id = st.selectbox(
        "Institution",
        options=ordered,
        format_func=fmt_option,
        label_visibility="collapsed",
    )

    if not selected_id:
        return

    # Load the selected row (always from full df, not filtered)
    mask = df["request_id"] == selected_id
    row  = df[mask].iloc[0]

    status_key   = row["status"]
    status_label, status_color, status_desc = STATUSES.get(status_key, ("?", "#999", ""))

    # Institution header
    st.markdown(
        f'<div class="institution-header" style="border-color:{status_color};background:{STATUS_BG.get(status_key,"white")}">'
        f'<span style="font-size:1.4rem;font-weight:700">{row["institution"]}</span>&nbsp;&nbsp;'
        f'<span class="status-pill" style="background:{status_color};color:white;font-size:0.9em">{status_label}</span><br>'
        f'<span style="color:#555;font-size:0.88rem">{row["type"]} &nbsp;|&nbsp; {row["system_district"]} &nbsp;|&nbsp; {row["city"]}</span>'
        f'</div>',
        unsafe_allow_html=True,
    )

    left_col, right_col = st.columns([1, 2], gap="large")

    # ── Left column: info & deadlines ─────────────────────────────────────────
    with left_col:
        st.markdown("#### 📌 Contact")
        if row["recipient_email"]:
            st.markdown(f"📧 `{row['recipient_email']}`")
        else:
            st.warning("No email on file — check the institution's website.")

        st.markdown("#### 📅 Dates & Deadlines")
        if row["date_sent"]:
            st.markdown(f"**Sent:** {row['date_sent']}")
        else:
            st.markdown("**Sent:** *(not yet sent)*")

        if row["deadline_10day"]:
            st.markdown(f"**10-day deadline:** {row['deadline_10day']}")
            st.markdown(deadline_badge(row["deadline_10day"]), unsafe_allow_html=True)

        if row["ag_notified_date"]:
            st.markdown(f"**AG notified:** {row['ag_notified_date']}")
        if row["deadline_ag_45day"]:
            st.markdown(f"**AG 45-day deadline:** {row['deadline_ag_45day']}")
            st.markdown(deadline_badge(row["deadline_ag_45day"]), unsafe_allow_html=True)

        st.markdown(f"**Last updated:** {row['last_updated']}")

        st.markdown("#### 📝 Notes")
        if row["notes"]:
            for note in str(row["notes"]).split(" | "):
                note = note.strip()
                if note:
                    st.markdown(f"- {note}")
        else:
            st.caption("No notes yet.")

    # ── Right column: tabs ────────────────────────────────────────────────────
    with right_col:
        tab_status, tab_notes, tab_template, tab_college = st.tabs(
            ["📊 Update Status", "📝 Notes", "📧 Email Template", "🏫 College Record"]
        )

        # ── Update Status tab ──────────────────────────────────────────────────
        with tab_status:
            st.markdown("#### Quick Actions")
            q1, q2, q3, q4 = st.columns(4)

            def quick_update(new_status, sent_today=False, ag_today=False):
                df.loc[mask, "status"]       = new_status
                df.loc[mask, "last_updated"] = str(today)
                if sent_today and not row["date_sent"]:
                    df.loc[mask, "date_sent"]      = str(today)
                    df.loc[mask, "deadline_10day"] = str(add_business_days(today, 10))
                if ag_today and not row["ag_notified_date"]:
                    df.loc[mask, "ag_notified_date"]  = str(today)
                    df.loc[mask, "deadline_ag_45day"] = str(add_business_days(today, 45))
                save_requests(df)
                st.rerun()

            if q1.button("📤 Mark Sent\n(today)", use_container_width=True,
                         help="Sets status to Sent and starts the 10-business-day clock from today"):
                quick_update("sent", sent_today=True)

            if q2.button("✅ Mark\nComplete", use_container_width=True,
                         help="Records received in full"):
                quick_update("complete")

            if q3.button("⚖️ AG Opinion\nRequested", use_container_width=True,
                         help="Agency is seeking an Attorney General ruling; starts 45-day clock"):
                quick_update("ag_opinion_requested", ag_today=True)

            if q4.button("🚫 Mark\nDenied", use_container_width=True,
                         help="Agency denied the request"):
                quick_update("denied")

            st.markdown("#### Set Any Status")

            new_status = st.selectbox(
                "New status",
                options=STATUS_KEYS,
                index=STATUS_KEYS.index(status_key),
                format_func=lambda x: f"{STATUSES[x][0]} — {STATUSES[x][2]}",
                key="new_status_select",
            )

            # Contextual date inputs
            new_date_sent   = row["date_sent"]
            new_ag_date     = row["ag_notified_date"]

            if new_status in {"sent", "acknowledged", "in_progress", "fee_pending", "fee_paid"}:
                if not row["date_sent"]:
                    ds_input = st.date_input(
                        "📅 Date originally sent", value=today, key="ds_input"
                    )
                    new_date_sent = str(ds_input)

            if new_status == "ag_opinion_requested" and not row["ag_notified_date"]:
                ag_input = st.date_input(
                    "📅 Date agency notified AG", value=today, key="ag_input"
                )
                new_ag_date = str(ag_input)

            status_note = st.text_input(
                "Optional note (e.g. 'Received letter dated Apr 1')",
                key="status_note_input",
            )

            if st.button("✅ Save Status", type="primary", use_container_width=True):
                df.loc[mask, "status"]       = new_status
                df.loc[mask, "last_updated"] = str(today)

                if new_date_sent and not row["date_sent"]:
                    df.loc[mask, "date_sent"]      = new_date_sent
                    df.loc[mask, "deadline_10day"] = str(
                        add_business_days(date.fromisoformat(new_date_sent), 10)
                    )

                if new_ag_date and not row["ag_notified_date"]:
                    df.loc[mask, "ag_notified_date"]  = new_ag_date
                    df.loc[mask, "deadline_ag_45day"] = str(
                        add_business_days(date.fromisoformat(new_ag_date), 45)
                    )

                if status_note.strip():
                    existing = str(df.loc[mask, "notes"].iloc[0] or "")
                    df.loc[mask, "notes"] = (
                        existing + f" | [{today}] {status_note.strip()}"
                    ).strip(" |")

                save_requests(df)
                st.success(f"✓ Status updated to: {STATUSES[new_status][0]}")
                st.rerun()

        # ── Notes tab ─────────────────────────────────────────────────────────
        with tab_notes:
            st.markdown("#### Add a Note")
            new_note = st.text_area(
                "Note",
                height=120,
                placeholder="e.g. 'Called PIO on April 3 — said they'd respond by end of week.'",
                label_visibility="collapsed",
                key="new_note_input",
            )
            if st.button("💾 Save Note", use_container_width=True):
                if new_note.strip():
                    existing = str(df.loc[mask, "notes"].iloc[0] or "")
                    df.loc[mask, "notes"]        = (existing + f" | [{today}] {new_note.strip()}").strip(" |")
                    df.loc[mask, "last_updated"] = str(today)
                    save_requests(df)
                    st.success("✓ Note saved.")
                    st.rerun()
                else:
                    st.warning("Note is empty.")

            if row["notes"]:
                st.markdown("#### Full Note History")
                for note in str(row["notes"]).split(" | "):
                    note = note.strip()
                    if note:
                        st.markdown(
                            f'<div style="background:#F5F5F5;padding:6px 10px;border-radius:6px;margin:4px 0;font-size:0.9em">📌 {note}</div>',
                            unsafe_allow_html=True,
                        )

            if st.button("🗑️ Clear All Notes", help="Permanently removes all notes for this institution"):
                df.loc[mask, "notes"]        = ""
                df.loc[mask, "last_updated"] = str(today)
                save_requests(df)
                st.success("Notes cleared.")
                st.rerun()

        # ── Email Template tab ────────────────────────────────────────────────
        with tab_template:
            st.markdown("#### Email Template")

            suggested_key = SUGGESTED_TEMPLATE.get(status_key, "01_initial_request")
            template_keys = list(TEMPLATE_LABELS.keys())

            chosen_template = st.selectbox(
                "Choose template",
                options=template_keys,
                index=template_keys.index(suggested_key),
                format_func=lambda x: TEMPLATE_LABELS[x],
                key="template_picker",
            )

            variables = build_template_variables(row, current_sender)
            body      = render_template(chosen_template, variables)
            subject   = get_subject(chosen_template, row["institution"])

            # Subject line (copyable)
            st.markdown("**Subject line:**")
            st.code(subject, language=None)

            # Body (copyable — st.code has a built-in copy button)
            st.markdown("**Email body** *(click the copy icon in the top-right corner of the box below):*")
            st.code(body, language=None)

            # mailto link (To + Subject only — body too large for URL encoding)
            to_email = str(row["recipient_email"] or "").strip()
            if to_email:
                params  = urllib.parse.urlencode({"subject": subject}, quote_via=urllib.parse.quote)
                mailto  = f"mailto:{to_email}?{params}"
                st.markdown(
                    f"""
                    <a href="{mailto}" style="
                        display: inline-block;
                        background: #1565C0;
                        color: white;
                        padding: 10px 24px;
                        border-radius: 8px;
                        text-decoration: none;
                        font-weight: bold;
                        font-size: 0.95rem;
                        margin-top: 6px;
                    ">📬 Open Email Client (To + Subject pre-filled)</a>
                    """,
                    unsafe_allow_html=True,
                )
                st.caption(
                    "**Workflow:** ① Copy the subject line above → ② Copy the email body → "
                    "③ Click the button to open your email app → ④ Paste body → Send"
                )
            else:
                st.warning(
                    "No email address on file for this institution. "
                    "Look up their public information officer on their website."
                )

            # Warn about any missing sender info
            if not all([current_sender.get("name"), current_sender.get("email")]):
                st.info("💡 Fill in **Your Info** in the sidebar to auto-populate template placeholders.")

        # ── College Record tab ───────────────────────────────────────────────
        with tab_college:
            st.markdown("#### Edit College Record")
            colleges_df = load_colleges().copy()
            college_mask = colleges_df["id"] == selected_id

            if college_mask.any():
                c_row = colleges_df[college_mask].iloc[0]

                with st.form("edit_college_form"):
                    st.text_input("ID", value=str(c_row.get("id", "")), disabled=True)
                    edit_institution = st.text_input("Institution", value=str(c_row.get("institution", "")))
                    edit_type = st.selectbox(
                        "Type",
                        options=["4yr", "2yr"],
                        index=0 if str(c_row.get("type", "")) == "4yr" else 1,
                    )
                    edit_system = st.text_input("System / District", value=str(c_row.get("system_district", "")))
                    edit_city = st.text_input("City", value=str(c_row.get("city", "")))
                    edit_email = st.text_input("Public records email", value=str(c_row.get("public_records_email", "")))
                    edit_portal = st.text_input("Public records website / portal", value=str(c_row.get("public_records_portal", "")))

                    contact_options = ["email", "portal", "both"]
                    current_contact_type = str(c_row.get("contact_type", "") or "email")
                    contact_index = contact_options.index(current_contact_type) if current_contact_type in contact_options else 0
                    edit_contact_type = st.selectbox("Contact type", options=contact_options, index=contact_index)

                    verify_options = ["yes", "partial", "no"]
                    current_verified = str(c_row.get("verified", "") or "partial")
                    verify_index = verify_options.index(current_verified) if current_verified in verify_options else 1
                    edit_verified = st.selectbox("Verified", options=verify_options, index=verify_index)

                    edit_notes = st.text_area("College notes", value=str(c_row.get("notes", "")), height=110)
                    save_college_edit = st.form_submit_button("💾 Save College Changes", use_container_width=True)

                if save_college_edit:
                    if not edit_institution.strip():
                        st.error("Institution name is required.")
                    else:
                        colleges_df.loc[college_mask, "institution"] = edit_institution.strip()
                        colleges_df.loc[college_mask, "type"] = edit_type
                        colleges_df.loc[college_mask, "system_district"] = edit_system.strip()
                        colleges_df.loc[college_mask, "city"] = edit_city.strip()
                        colleges_df.loc[college_mask, "public_records_email"] = edit_email.strip()
                        colleges_df.loc[college_mask, "public_records_portal"] = edit_portal.strip()
                        colleges_df.loc[college_mask, "contact_type"] = edit_contact_type
                        colleges_df.loc[college_mask, "verified"] = edit_verified
                        colleges_df.loc[college_mask, "notes"] = edit_notes.strip()

                        save_colleges(colleges_df)

                        updated_college = colleges_df[college_mask].iloc[0]
                        df = sync_request_from_college(df, updated_college)
                        save_requests(df)

                        load_colleges.clear()
                        st.success("College record updated and synced to requests.")
                        st.rerun()
            else:
                st.warning(
                    "This request ID is not currently in colleges.csv. "
                    "Use the form below to add it as a new college record."
                )

            st.markdown("#### Add New College")
            with st.form("add_college_form"):
                new_id = st.text_input("New ID", placeholder="e.g. CC056 or UT037")
                new_institution = st.text_input("Institution name")
                new_type = st.selectbox("Type", options=["4yr", "2yr"], key="new_type")
                new_system = st.text_input("System / District")
                new_city = st.text_input("City")
                new_email = st.text_input("Public records email")
                new_portal = st.text_input("Public records website / portal")
                new_contact_type = st.selectbox("Contact type", options=["email", "portal", "both"], key="new_contact_type")
                new_verified = st.selectbox("Verified", options=["yes", "partial", "no"], index=1, key="new_verified")
                new_notes = st.text_area("Notes", height=90, key="new_college_notes")
                add_college = st.form_submit_button("➕ Add College Record", type="primary", use_container_width=True)

            if add_college:
                clean_id = new_id.strip()
                if not clean_id:
                    st.error("ID is required.")
                elif not new_institution.strip():
                    st.error("Institution name is required.")
                elif (colleges_df["id"] == clean_id).any() or (df["request_id"] == clean_id).any():
                    st.error(f"A college/request with ID '{clean_id}' already exists.")
                else:
                    new_college_row = {
                        "id": clean_id,
                        "institution": new_institution.strip(),
                        "type": new_type,
                        "system_district": new_system.strip(),
                        "city": new_city.strip(),
                        "public_records_email": new_email.strip(),
                        "public_records_portal": new_portal.strip(),
                        "contact_type": new_contact_type,
                        "verified": new_verified,
                        "notes": new_notes.strip(),
                    }

                    colleges_df = pd.concat([colleges_df, pd.DataFrame([new_college_row])], ignore_index=True)
                    save_colleges(colleges_df)

                    df = sync_request_from_college(df, pd.Series(new_college_row))
                    save_requests(df)

                    load_colleges.clear()
                    st.success(f"Added {new_institution.strip()} ({clean_id}) and initialized its draft request.")
                    st.rerun()

    # ── Footer ────────────────────────────────────────────────────────────────
    st.divider()
    st.caption(
        "🏛️ TPIA Tracker &nbsp;|&nbsp; Annette Strauss Institute for Civic Life, UT Austin &nbsp;|&nbsp; "
        f"Texas Government Code Ch. 552 &nbsp;|&nbsp; {today}"
    )


if __name__ == "__main__":
    main()
