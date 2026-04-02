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

## Quick Start

**Requirements:** Python 3.10+, pip

```bash
# 1. Install dependencies (one-time)
pip install -r requirements.txt

# 2. Launch the tracker GUI
streamlit run app.py
```

Your browser will open automatically to the tracker. The first launch creates
`requests.csv` with all 91 institutions in "Draft" status.

---

## ⚠️ Verify Email Addresses Before Sending

The email addresses in `colleges.csv` are sourced from background knowledge and are
marked `verified: partial` — meaning they are plausible based on institutional naming
conventions, but **must be confirmed against each institution's current website** before
sending. A wrong email means your request goes to the wrong person (or bounces) and the
10-day clock may not start.

**Where to look on each institution's website:**
- Search the site for "Public Information Officer", "Open Records", or "TPIA"
- Check pages under: General Counsel → Public Information, or Legal Affairs → Records
- Many institutions now use online portals (GovQA, Granicus); look for "Submit a Public
  Records Request" or similar buttons
- If the institution is part of a system (e.g., UT System, A&M System), the system-level
  page may list PIO contacts for all member institutions

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
(the tracker calculates this automatically).

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
| `app.py` | **Main GUI** — run with `streamlit run app.py` |
| `colleges.csv` | Master list of 91 Texas public institutions with TPIA contact info |
| `requests.csv` | Auto-generated tracker (one row per institution); gitignored |
| `sender.json` | Your name/contact info for templates (gitignored) |
| `requirements.txt` | Python dependencies (`streamlit`, `pandas`) |
| `tpia_tracker.py` | Advanced CLI tool (for power users / scripting) |
| `templates/01_initial_request.txt` | Initial TPIA request |
| `templates/02_follow_up.txt` | Follow-up when 10-day deadline is approaching/past |
| `templates/03_fee_inquiry.txt` | Response to fee estimate notice |
| `templates/04_ag_opinion_response.txt` | Response when agency seeks AG opinion |
| `templates/05_denial_response.txt` | Response to a denial |
| `templates/06_records_received.txt` | Acknowledgment of received records |

---

## Using the GUI

### First-time setup

1. Run `streamlit run app.py` — your browser opens automatically
2. Fill in **Your Info** in the left sidebar (name, title, email, etc.) and click
   **Save Info** — this auto-fills your signature in all templates
3. The table shows all 91 institutions, all in "Draft" status

### Sending requests (no SMTP required)

The tracker does not send email on your behalf. It prepares the email so you send it
from your normal email app. The workflow for each institution:

1. **Select** the institution in the dropdown below the table
2. Go to the **📧 Email Template** tab — the right template is pre-selected based on
   the institution's current status
3. **Copy the subject line** (click the 📋 icon in the top-right of the code block)
4. **Copy the email body** (click the 📋 icon in the code block below)
5. Click **"Open Email Client"** — opens your default mail app with To: and Subject:
   already filled in
6. **Paste the body**, review it, and hit Send
7. Return to the tracker and click **"📤 Mark Sent (today)"** — starts the
   10-business-day deadline clock automatically

### Tracking responses

When a response arrives:

1. Select the institution in the dropdown
2. Click the appropriate **Quick Action** button, or use the "Set Any Status" dropdown:
   - **✅ Mark Complete** — records received in full
   - **⚖️ AG Opinion Requested** — auto-sets the 45-day AG deadline
   - **🚫 Mark Denied** — opens the denial response template
   - **💵 Fee Pending** — opens the fee inquiry template
3. Add a note in the **📝 Notes** tab (e.g., "Called PIO 4/5 — said they'll respond by end of week")

### Overdue alerts

If any request is past its 10-day (or 45-day AG) deadline, a red banner appears at the
top of the page. Overdue institutions are sorted to the top of the dropdown. Select one
and send a follow-up from the Email Template tab.

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
| `verified` | `yes` = confirmed; `partial` = needs verification |
| `notes` | Special circumstances, merged institutions, etc. |

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
| University of Texas System | UT Austin, UTA, UTD, UTEP, UTPB, UTRGV, UTSA, UT Tyler, UT Galveston |
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
- **IRB approval** should be obtained for the voter outreach experiment component of the
  research before contacting students.
