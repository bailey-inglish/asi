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

**Requirements:** Node.js 18+, npm

```bash
# 1. Install dependencies (one-time)
npm install

# 2. Launch the tracker UI
npm run dev
```

Open the local URL shown in the terminal. The app reads and writes directly to the
existing CSV files in this folder.

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
| `app/` | **Next.js app** — redesigned UI, dashboard, and edit workflows |
| `components/` | Client-side UI components and shared display constants |
| `lib/data.js` | CSV/file helpers for colleges, requests, sender profile, and templates |
| `package.json` | Node dependencies and app scripts |
| `colleges.csv` | Master list of 91 Texas public institutions with TPIA contact info |
| `requests.csv` | Auto-generated tracker (one row per institution); gitignored |
| `sender.json` | Your name/contact info for templates (gitignored) |
| `app.py` | Legacy Streamlit prototype kept for reference |
| `requirements.txt` | Legacy Python dependencies |
| `tpia_tracker.py` | Legacy CLI tool (for power users / scripting) |
| `templates/01_initial_request.txt` | Initial TPIA request |
| `templates/02_follow_up.txt` | Follow-up when 10-day deadline is approaching/past |
| `templates/03_fee_inquiry.txt` | Response to fee estimate notice |
| `templates/04_ag_opinion_response.txt` | Response when agency seeks AG opinion |
| `templates/05_denial_response.txt` | Response to a denial |
| `templates/06_records_received.txt` | Acknowledgment of received records |

---

## Using the UI

### First-time setup

1. Run `npm run dev`
2. Fill in **Sender profile** in the left sidebar under **Admin**
3. Use the navigation rail to switch between **Overview**, **Institutions**, **Templates**, and **Admin**

### Sending requests (no SMTP required)

The tracker still does not send email on your behalf. It prepares the subject/body so
you can send from your normal email app. The workflow for each institution:

1. Select an institution from the **Institutions** view
2. Open the **Templates** view
3. Copy the subject and body or open the mail client link
4. Return to **Institutions** and use the quick action buttons to mark the request sent

### Tracking responses

When a response arrives:

1. Select the institution in **Institutions**
2. Use the quick actions to set status, deadlines, and notes
3. Add or review notes in the timeline panel
4. Use **Admin** if the institution's contact record changed and needs updating

### Overdue alerts

If any request is past its deadline, the top summary cards and selected-record detail
make that status obvious at a glance.

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
