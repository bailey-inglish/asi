# config.example.py — Copy this file to config.py and fill in your details.
# config.py is gitignored; never commit real credentials.

# ── Sender identity (used in email templates) ────────────────────────────────
SENDER_NAME    = "Your Name"
SENDER_TITLE   = "Research Coordinator"
SENDER_ORG     = "The Annette Strauss Institute for Civic Life, The University of Texas at Austin"
SENDER_EMAIL   = "yourname@utexas.edu"
SENDER_PHONE   = "(512) 555-0000"
SENDER_ADDRESS = "2504 Whitis Ave, Austin, TX 78712"

# ── SMTP settings ─────────────────────────────────────────────────────────────
# Example uses Gmail with an App Password.  Adjust for your mail provider.
SMTP_HOST     = "smtp.gmail.com"
SMTP_PORT     = 465          # 465 = SSL/TLS; use 587 for STARTTLS
SMTP_USER     = "yourname@utexas.edu"
SMTP_PASSWORD = "your-app-password-here"

# Optional: BCC yourself on every outgoing message for your own records
BCC = "yourname+bcc@utexas.edu"
