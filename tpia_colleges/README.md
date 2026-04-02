# TPIA College Records Project

This directory contains everything needed to submit Texas Public Information Act (TPIA)
requests for student directory information to every operational public 4-year and 2-year
college and university in Texas, track responses, and manage statutory deadlines.

---

## Background

The Annette Strauss Institute for Civic Life is conducting a research project examining
whether outreach emails about voter registration and mail-in ballot applications increase
student voter turnout. To build the outreach list, we are requesting **student directory
information** (names and institutional email addresses) from Texas public institutions of
higher education using the Texas Public Information Act.

**FERPA note:** Institutions may withhold records for any student who has submitted a
written request to restrict their directory information ("FERPA opt-out"). We request
only records for students who have *not* exercised that right, which institutions are
legally required to disclose under TPIA.

---

## Texas Public Information Act — Key Requirements

| Topic | Rule |
|---|---|
| Response deadline | 10 **business** days from receipt (Tex. Gov't Code § 552.228) |
| AG opinion | Agency must request within 10 business days; AG has 45 business days to rule (§ 552.301) |
| Fee waiver basis | Research benefiting the public; request under § 552.267 |
| Partial production | Non-exempt records must be released even if some are withheld (§ 552.301(e)) |
| No purpose required | Agency cannot require you to explain *why* you want the records (§ 552.222) |
| Clarification before denial | Agency must ask for clarification rather than deny an unclear request (§ 552.222) |

**Business days** = Monday–Friday, excluding Texas state and federal holidays
(see `tpia_tracker.py` → `tx_holidays()`).

---

## Request Status Flowchart

```
[draft] → send initial request → [sent]
                                    │
              ┌─────────────────────┼───────────────────────┐
              ▼                     ▼                         ▼
      [acknowledged]        [fee_pending]           [ag_opinion_requested]
              │                     │                         │
              ▼                     ▼                         ▼
       [in_progress]          [fee_paid]            [ag_opinion_pending]
              │                     │                         │
         ┌────┴────┐           ┌────┘             ┌───────────┘
         ▼         ▼           ▼                  ▼
     [complete] [denied]  [in_progress]      [complete]
                   │                         [denied]
                   ▼
         [partially_complete]
```

**Status meanings:**

| Status | Meaning |
|---|---|
| `draft` | Request has not been sent yet |
| `sent` | Email sent; 10-day clock is running |
| `acknowledged` | Agency confirmed receipt |
| `in_progress` | Agency is actively processing |
| `fee_pending` | Agency sent cost estimate; awaiting our decision |
| `fee_paid` | We paid the fee; waiting for record delivery |
| `ag_opinion_requested` | Agency is seeking AG ruling; 45-day clock started |
| `ag_opinion_pending` | AG issued opinion; waiting for agency to comply |
| `denied` | Request was refused |
| `partially_complete` | Some records received; some withheld or denied |
| `complete` | All records received |
| `withdrawn` | We retracted the request |

---

## Files

| File | Description |
|---|---|
| `colleges.csv` | Master list of 91 Texas public institutions with TPIA contact info |
| `requests.csv` | Auto-generated tracker (one row per institution); created by `init` |
| `tpia_tracker.py` | Python CLI tool for sending requests, tracking, and follow-up |
| `config.example.py` | SMTP/sender configuration template |
| `config.py` | **Your actual config** (gitignored — never commit credentials) |
| `templates/01_initial_request.txt` | Initial TPIA request |
| `templates/02_follow_up.txt` | Follow-up when 10-day deadline is approaching/past |
| `templates/03_fee_inquiry.txt` | Response to fee estimate notice |
| `templates/04_ag_opinion_response.txt` | Response when agency seeks AG opinion |
| `templates/05_denial_response.txt` | Response to a denial |
| `templates/06_records_received.txt` | Acknowledgment of received records |

---

## Setup

**Requirements:** Python 3.10+, standard library only (no external packages needed).

1. Copy the config template and fill in your SMTP credentials:
   ```bash
   cp config.example.py config.py
   # Edit config.py with your name, email, and SMTP settings
   ```

2. Initialize the request tracker:
   ```bash
   python tpia_tracker.py init
   ```
   This creates `requests.csv` with one row per institution, all in `draft` status.

---

## Usage

### Preview emails before sending (recommended first step)

```bash
# Preview a single institution's email
python tpia_tracker.py send UT001 --dry-run

# Preview all emails
python tpia_tracker.py send-all --dry-run
```

### Send requests

```bash
# Send to a single institution
python tpia_tracker.py send UT001

# Send to all draft institutions (prompts for confirmation)
python tpia_tracker.py send-all
```

### Check status

```bash
python tpia_tracker.py status       # Summary dashboard
python tpia_tracker.py deadlines    # Upcoming and overdue deadlines
python tpia_tracker.py log CC018    # Full detail for one request
```

### Update a request's status

```bash
python tpia_tracker.py update UT001 acknowledged "Called front desk; confirmed receipt"
python tpia_tracker.py update UT001 fee_pending "Quoted $25 for compilation"
python tpia_tracker.py update UT001 ag_opinion_requested "Received notice; AG letter ORD-12345"
python tpia_tracker.py update UT001 complete "Records received; 1,847 students"
python tpia_tracker.py update UT001 denied "Cited § 552.101 (attorney–client privilege — incorrect basis)"
```

### Send follow-up emails to overdue requests

```bash
python tpia_tracker.py followup --dry-run   # Preview
python tpia_tracker.py followup             # Send
```

### View a template

```bash
python tpia_tracker.py show-template 01_initial_request
python tpia_tracker.py show-template 05_denial_response
```

---

## colleges.csv — Column Reference

| Column | Description |
|---|---|
| `id` | Unique ID (e.g., `UT001`, `CC018`) |
| `institution` | Full institution name |
| `type` | `4yr` or `2yr` |
| `system_district` | University system or community college district |
| `city` | City |
| `public_records_email` | Best-known TPIA contact email (verify before sending!) |
| `public_records_portal` | Online portal URL, if known |
| `contact_type` | `email`, `portal`, or `both` |
| `verified` | `yes` = confirmed; `partial` = needs verification; `no` = unknown |
| `notes` | Special circumstances, merged institutions, etc. |

> **Important:** All emails marked `verified: partial` should be confirmed against the
> institution's current website before sending. A good place to look is the institution's
> General Counsel, Public Information Officer, or Legal Affairs page.

---

## Institution Count Summary

| Category | Count |
|---|---|
| 4-year universities (all systems) | 36 |
| 2-year colleges & technical colleges | 55 |
| **Total** | **91** |

### 4-Year University Systems

| System | Institutions |
|---|---|
| University of Texas System | UT Austin, UTA, UTD, UTEP, UTPB, UTRGV, UTSA, UT Tyler, UT Galveston (branch) |
| Texas A&M System | TAMU, TAMU-Commerce, TAMIU, TAMU-CC, TAMU-K, TAMU-SA, TAMU-T, TAMU-CT, PVAMU, Tarleton, WTAMU, TAMU-Galveston |
| Texas State University System | Texas State, SHSU, Lamar, Sul Ross |
| University of Houston System | UH, UHCL, UHD, UHV, SFA (joined 2023) |
| Texas Tech University System | TTU, Angelo State, Midwestern State |
| University of North Texas System | UNT, UNT Dallas |
| Standalone | Texas Southern University, Texas Woman's University |

### 2-Year College Systems/Districts

- **TSTC** (statewide; 1 institution, multiple campuses)
- **Lamar State Colleges** (LIT, LSCO, LSCPA; part of TX State system)
- **Sul Ross Rio Grande College** (branch of Sul Ross)
- **52 independent community college districts** across the state

---

## Legal Notes

- This project operates entirely under the Texas Public Information Act and does not
  require court orders or subpoenas.
- Student directory information for non-opt-out students is a well-established category
  of public records under Texas AG opinions (see ORD-679, 2001).
- All data collected will be used solely for academic research and voter outreach.
- IRB approval should be obtained for the voter outreach experiment component of the
  research before contacting students.
