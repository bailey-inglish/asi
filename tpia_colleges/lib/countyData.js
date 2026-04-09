import fs from 'node:fs/promises';
import path from 'node:path';
import { Pool } from 'pg';
import { addBusinessDays, loadColleges, loadSender, parseCollegeAllCounties } from './data';

const DATABASE_URL = process.env.DATABASE_URL;

if (!DATABASE_URL) {
  throw new Error('DATABASE_URL is required. Set it to your Neon Postgres connection string.');
}

const pool = new Pool({
  connectionString: DATABASE_URL,
  ssl: { rejectUnauthorized: false },
});

export const COUNTY_TEMPLATE_DIR = path.join(process.cwd(), 'templates', 'county');
const COUNTY_CONTACT_SEED_PATH = path.join(process.cwd(), 'data', 'county_contacts_seed.json');

export const COUNTY_TEMPLATE_LABELS = {
  '01_county_initial_request': 'Initial voter list request',
  '02_county_follow_up': 'Follow-up',
  '03_county_fee_waiver': 'Fee waiver response',
  '04_county_fee_payment': 'Fee payment response',
  '05_county_ag_opinion_response': 'AG opinion response',
  '06_county_denial_response': 'Denial response',
  '07_county_records_received': 'Records received',
};

export const COUNTY_SUBJECT_LINES = {
  '01_county_initial_request': 'Texas Public Information Act Request - Voter Registration and Voting History',
  '02_county_follow_up': 'Follow-Up: TPIA Voter Records Request - {county}',
  '03_county_fee_waiver': 'Re: Fee Waiver Request - TPIA Voter Records Request - {county}',
  '04_county_fee_payment': 'Re: Fee Estimate - TPIA Voter Records Request - {county}',
  '05_county_ag_opinion_response': 'Re: AG Opinion Notice - TPIA Voter Records Request - {county}',
  '06_county_denial_response': 'Re: Denial - TPIA Voter Records Request - {county}',
  '07_county_records_received': 'Confirmation: Voter Records Received - {county}',
};

export const COUNTY_SUGGESTED_TEMPLATE = {
  draft: '01_county_initial_request',
  sent: '02_county_follow_up',
  acknowledged: '02_county_follow_up',
  in_progress: '02_county_follow_up',
  fee_pending: '03_county_fee_waiver',
  fee_paid: '04_county_fee_payment',
  ag_opinion_requested: '05_county_ag_opinion_response',
  ag_opinion_pending: '02_county_follow_up',
  denied: '06_county_denial_response',
  partially_complete: '07_county_records_received',
  complete: '07_county_records_received',
  withdrawn: '01_county_initial_request',
};

// Mirrors the shared client-side county catalog for full under-the-hood county tracking.
export const TEXAS_COUNTIES = [
  'Anderson', 'Andrews', 'Angelina', 'Aransas', 'Archer', 'Armstrong', 'Atascosa', 'Austin', 'Bailey', 'Bandera', 'Bastrop', 'Baylor', 'Bee', 'Bell', 'Bexar', 'Blanco', 'Borden', 'Bosque', 'Bowie', 'Brazoria', 'Brazos', 'Brewster', 'Briscoe', 'Brooks', 'Brown', 'Burleson', 'Burnet', 'Caldwell', 'Calhoun', 'Callahan', 'Cameron', 'Camp', 'Carson', 'Cass', 'Castro', 'Chambers', 'Cherokee', 'Childress', 'Clay', 'Cochran', 'Coke', 'Coleman', 'Collin', 'Collingsworth', 'Colorado', 'Comal', 'Comanche', 'Concho', 'Cooke', 'Coryell', 'Cottle', 'Crane', 'Crockett', 'Crosby', 'Culberson', 'Dallam', 'Dallas', 'Dawson', 'Deaf Smith', 'Delta', 'Denton', 'DeWitt', 'Dickens', 'Dimmit', 'Donley', 'Duval', 'Eastland', 'Ector', 'Edwards', 'Ellis', 'El Paso', 'Erath', 'Falls', 'Fannin', 'Fayette', 'Fisher', 'Floyd', 'Foard', 'Fort Bend', 'Franklin', 'Freestone', 'Frio', 'Gaines', 'Galveston', 'Garza', 'Gillespie', 'Glasscock', 'Goliad', 'Gonzales', 'Gray', 'Grayson', 'Gregg', 'Grimes', 'Guadalupe', 'Hale', 'Hall', 'Hamilton', 'Hansford', 'Hardeman', 'Hardin', 'Harris', 'Harrison', 'Hartley', 'Haskell', 'Hays', 'Hemphill', 'Henderson', 'Hidalgo', 'Hill', 'Hockley', 'Hood', 'Hopkins', 'Houston', 'Howard', 'Hudspeth', 'Hunt', 'Hutchinson', 'Irion', 'Jack', 'Jackson', 'Jasper', 'Jeff Davis', 'Jefferson', 'Jim Hogg', 'Jim Wells', 'Johnson', 'Jones', 'Karnes', 'Kaufman', 'Kendall', 'Kenedy', 'Kent', 'Kerr', 'Kimble', 'King', 'Kinney', 'Kleberg', 'Knox', 'La Salle', 'Lamar', 'Lamb', 'Lampasas', 'Lavaca', 'Lee', 'Leon', 'Liberty', 'Limestone', 'Lipscomb', 'Live Oak', 'Llano', 'Loving', 'Lubbock', 'Lynn', 'McCulloch', 'McLennan', 'McMullen', 'Madison', 'Marion', 'Martin', 'Mason', 'Matagorda', 'Maverick', 'Medina', 'Menard', 'Midland', 'Milam', 'Mills', 'Mitchell', 'Montague', 'Montgomery', 'Moore', 'Morris', 'Motley', 'Nacogdoches', 'Navarro', 'Newton', 'Nolan', 'Nueces', 'Ochiltree', 'Oldham', 'Orange', 'Palo Pinto', 'Panola', 'Parker', 'Parmer', 'Pecos', 'Polk', 'Potter', 'Presidio', 'Rains', 'Randall', 'Reagan', 'Real', 'Red River', 'Reeves', 'Refugio', 'Roberts', 'Robertson', 'Rockwall', 'Runnels', 'Rusk', 'Sabine', 'San Augustine', 'San Jacinto', 'San Patricio', 'San Saba', 'Schleicher', 'Scurry', 'Shackelford', 'Shelby', 'Sherman', 'Smith', 'Somervell', 'Starr', 'Stephens', 'Sterling', 'Stonewall', 'Sutton', 'Swisher', 'Tarrant', 'Taylor', 'Terrell', 'Terry', 'Throckmorton', 'Titus', 'Tom Green', 'Travis', 'Trinity', 'Tyler', 'Upshur', 'Upton', 'Uvalde', 'Val Verde', 'Van Zandt', 'Victoria', 'Walker', 'Waller', 'Ward', 'Washington', 'Webb', 'Wharton', 'Wheeler', 'Wichita', 'Wilbarger', 'Willacy', 'Williamson', 'Wilson', 'Winkler', 'Wise', 'Wood', 'Yoakum', 'Young', 'Zapata', 'Zavala',
];

let countySchemaReadyPromise = null;
let countyContactSeedPromise = null;

function countyIdFromName(name) {
  return String(name || '')
    .trim()
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-+|-+$/g, '');
}

function dateValueToIso(value) {
  if (value == null || value === '') return '';
  if (value instanceof Date && !Number.isNaN(value.getTime())) {
    return value.toISOString();
  }

  const text = String(value).trim();
  if (!text) return '';
  if (/^\d{4}-\d{2}-\d{2}$/.test(text)) {
    return new Date(`${text}T00:00:00.000Z`).toISOString();
  }

  const parsed = new Date(text.replace(/\s*\([^)]*\)\s*$/, '').trim() || text);
  if (Number.isNaN(parsed.getTime())) return '';
  return parsed.toISOString();
}

function dateValueToDb(value) {
  const iso = dateValueToIso(value);
  return iso || null;
}

function parseStatusLog(value) {
  if (Array.isArray(value)) return value;
  if (value == null) return [];

  try {
    const parsed = typeof value === 'string' ? JSON.parse(value) : value;
    if (!Array.isArray(parsed)) return [];

    return parsed
      .map((entry) => {
        if (!entry || typeof entry !== 'object') return null;
        const toStatus = String(entry.to || '').trim();
        if (!toStatus) return null;
        return {
          type: String(entry.type || 'status_change'),
          from: entry.from == null ? null : String(entry.from),
          to: toStatus,
          user: String(entry.user || 'System'),
          at: dateValueToIso(entry.at) || new Date().toISOString(),
        };
      })
      .filter(Boolean);
  } catch {
    return [];
  }
}

function parseStatusDates(value) {
  if (value == null) return {};
  try {
    const parsed = typeof value === 'string' ? JSON.parse(value) : value;
    if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) return {};
    return Object.entries(parsed).reduce((acc, [status, timestamp]) => {
      const normalized = dateValueToIso(timestamp);
      if (normalized) {
        acc[String(status)] = normalized;
      }
      return acc;
    }, {});
  } catch {
    return {};
  }
}

function normalizeOutputRow(row) {
  const normalized = {};
  for (const [key, value] of Object.entries(row)) {
    normalized[key] = value == null ? '' : String(value);
  }
  return normalized;
}

function inferVerification({ email, portal, phone, contactName }) {
  const score = [email, portal, phone, contactName].filter((value) => String(value || '').trim()).length;
  if (score >= 3) return 'yes';
  if (score >= 1) return 'partial';
  return 'no';
}

function extractPhone(value) {
  const text = String(value || '').trim();
  if (!text) return '';
  const match = text.match(/\(?\d{3}\)?[\s.-]*\d{3}[\s.-]*\d{4}(?:\s*(?:x|ext\.?|extension)\s*\d+)?/i);
  return match ? match[0].replace(/\s+/g, ' ').trim() : '';
}

function normalizeCountyLookupKey(name) {
  return String(name || '')
    .trim()
    .toLowerCase()
    .replace(/\bcounty\b/g, '')
    .replace(/[^a-z0-9]+/g, ' ')
    .trim();
}

async function loadCountyContactSeedMap() {
  if (!countyContactSeedPromise) {
    countyContactSeedPromise = (async () => {
      try {
        const raw = await fs.readFile(COUNTY_CONTACT_SEED_PATH, 'utf8');
        const parsed = JSON.parse(raw);
        if (!Array.isArray(parsed)) return new Map();

        const map = new Map();
        for (const entry of parsed) {
          if (!entry || typeof entry !== 'object') continue;
          const key = normalizeCountyLookupKey(entry.county_name);
          if (!key) continue;

          const contact = {
            contact_name: String(entry.contact_name || '').trim(),
            email: String(entry.email || '').trim(),
            phone: extractPhone(entry.phone),
            source: String(entry.source || '').trim(),
          };

          const existing = map.get(key);
          const existingScore = existing
            ? [existing.contact_name, existing.email, existing.phone].filter((value) => value).length
            : -1;
          const currentScore = [contact.contact_name, contact.email, contact.phone].filter((value) => value).length;
          if (!existing || currentScore > existingScore) {
            map.set(key, contact);
          }
        }
        return map;
      } catch {
        return new Map();
      }
    })();
  }

  return countyContactSeedPromise;
}

async function ensureCountySchema() {
  if (countySchemaReadyPromise) return countySchemaReadyPromise;

  countySchemaReadyPromise = (async () => {
    const client = await pool.connect();
    try {
      await client.query('BEGIN');
      await client.query(`
        CREATE TABLE IF NOT EXISTS public.county_requests (
          county_id text PRIMARY KEY,
          county_name text NOT NULL UNIQUE,
          contact_name text NOT NULL DEFAULT '',
          email text NOT NULL DEFAULT '',
          portal text NOT NULL DEFAULT '',
          phone text NOT NULL DEFAULT '',
          verified text NOT NULL DEFAULT 'no',
          status text NOT NULL DEFAULT 'draft',
          notes text NOT NULL DEFAULT '',
          date_sent timestamptz,
          deadline_10day timestamptz,
          deadline_ag_45day timestamptz,
          ag_notified_date timestamptz,
          last_updated timestamptz,
          status_log jsonb NOT NULL DEFAULT '[]'::jsonb,
          status_dates jsonb NOT NULL DEFAULT '{}'::jsonb,
          status_changed_at timestamptz,
          status_changed_by text
        )
      `);

      await client.query(`
        CREATE INDEX IF NOT EXISTS county_requests_status_idx
        ON public.county_requests (status)
      `);

      await client.query('COMMIT');
    } catch (error) {
      await client.query('ROLLBACK');
      throw error;
    } finally {
      client.release();
    }
  })();

  return countySchemaReadyPromise;
}

export async function loadCountyRequests() {
  await ensureCountySchema();
  const { rows } = await pool.query(`
    SELECT county_id, county_name, contact_name, email, portal, phone,
           verified, status, notes, date_sent, deadline_10day, deadline_ag_45day,
           ag_notified_date, last_updated, status_log, status_dates,
           status_changed_at, status_changed_by
    FROM public.county_requests
    ORDER BY county_name ASC
  `);

  return rows.map((row) => {
    const normalized = normalizeOutputRow(row);
    return {
      ...normalized,
      county_id: String(normalized.county_id || countyIdFromName(normalized.county_name)),
      county_name: String(normalized.county_name || ''),
      status: String(normalized.status || 'draft').trim() || 'draft',
      date_sent: dateValueToIso(row.date_sent),
      deadline_10day: dateValueToIso(row.deadline_10day),
      deadline_ag_45day: dateValueToIso(row.deadline_ag_45day),
      ag_notified_date: dateValueToIso(row.ag_notified_date),
      last_updated: dateValueToIso(row.last_updated),
      status_log: parseStatusLog(row.status_log),
      status_dates: parseStatusDates(row.status_dates),
      status_changed_at: dateValueToIso(row.status_changed_at),
      status_changed_by: String(row.status_changed_by || '').trim(),
    };
  });
}

export async function saveCountyRequests(rows) {
  await ensureCountySchema();
  const client = await pool.connect();

  try {
    await client.query('BEGIN');

    for (const row of rows) {
      const countyName = String(row.county_name || '').trim();
      if (!countyName) continue;
      const countyId = String(row.county_id || countyIdFromName(countyName));

      await client.query(
        `
        INSERT INTO public.county_requests (
          county_id, county_name, contact_name, email, portal, phone,
          verified, status, notes, date_sent, deadline_10day, deadline_ag_45day,
          ag_notified_date, last_updated, status_log, status_dates,
          status_changed_at, status_changed_by
        ) VALUES (
          $1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15::jsonb,$16::jsonb,$17,$18
        )
        ON CONFLICT (county_id) DO UPDATE SET
          county_name = EXCLUDED.county_name,
          contact_name = EXCLUDED.contact_name,
          email = EXCLUDED.email,
          portal = EXCLUDED.portal,
          phone = EXCLUDED.phone,
          verified = EXCLUDED.verified,
          status = EXCLUDED.status,
          notes = EXCLUDED.notes,
          date_sent = EXCLUDED.date_sent,
          deadline_10day = EXCLUDED.deadline_10day,
          deadline_ag_45day = EXCLUDED.deadline_ag_45day,
          ag_notified_date = EXCLUDED.ag_notified_date,
          last_updated = EXCLUDED.last_updated,
          status_log = EXCLUDED.status_log,
          status_dates = EXCLUDED.status_dates,
          status_changed_at = EXCLUDED.status_changed_at,
          status_changed_by = EXCLUDED.status_changed_by
        `,
        [
          countyId,
          countyName,
          String(row.contact_name || ''),
          String(row.email || ''),
          String(row.portal || ''),
          String(row.phone || ''),
          String(row.verified || 'no') || 'no',
          String(row.status || 'draft') || 'draft',
          String(row.notes || ''),
          dateValueToDb(row.date_sent),
          dateValueToDb(row.deadline_10day),
          dateValueToDb(row.deadline_ag_45day),
          dateValueToDb(row.ag_notified_date),
          dateValueToDb(row.last_updated),
          JSON.stringify(parseStatusLog(row.status_log)),
          JSON.stringify(parseStatusDates(row.status_dates)),
          dateValueToDb(row.status_changed_at),
          String(row.status_changed_by || '') || null,
        ],
      );
    }

    await client.query('COMMIT');
  } catch (error) {
    await client.query('ROLLBACK');
    throw error;
  } finally {
    client.release();
  }
}

export async function ensureCountyRequestRows() {
  await ensureCountySchema();
  const countySeedMap = await loadCountyContactSeedMap();

  const existingRows = await loadCountyRequests();
  const existingById = new Map(existingRows.map((row) => [String(row.county_id), row]));

  const nowIso = new Date().toISOString();
  let existingChanged = false;
  const updatedExistingRows = existingRows.map((row) => {
    const seed = countySeedMap.get(normalizeCountyLookupKey(row.county_name));
    if (!seed) return row;

    const currentEmail = String(row.email || '').trim();
    const currentPortal = String(row.portal || '').trim();
    const currentPhone = extractPhone(row.phone);
    const currentContact = String(row.contact_name || '').trim();
    const seedPhone = extractPhone(seed.phone);

    const nextEmail = (seed.email && (!currentEmail || /\.edu\b/i.test(currentEmail))) ? seed.email : currentEmail;
    const nextContact = (!currentContact && seed.contact_name) ? seed.contact_name : currentContact;
    const nextPhone = seedPhone && (!currentPhone || currentPhone !== String(row.phone || '').trim()) ? seedPhone : currentPhone;
    const nextPortal = /\.edu\b/i.test(currentPortal) ? '' : currentPortal;

    if (
      nextEmail === currentEmail
      && nextContact === currentContact
      && nextPhone === currentPhone
      && nextPortal === currentPortal
    ) {
      return row;
    }

    existingChanged = true;
    return {
      ...row,
      email: nextEmail,
      contact_name: nextContact,
      phone: nextPhone,
      portal: nextPortal,
      verified: inferVerification({
        email: nextEmail,
        portal: nextPortal,
        phone: nextPhone,
        contactName: nextContact,
      }),
      last_updated: nowIso,
    };
  });

  const upserts = [];

  for (const countyName of TEXAS_COUNTIES) {
    const countyId = countyIdFromName(countyName);
    if (existingById.has(countyId)) continue;

    const seed = countySeedMap.get(normalizeCountyLookupKey(countyName));
    const contactName = String(seed?.contact_name || '').trim();
    const email = String(seed?.email || '').trim();
    const phone = extractPhone(seed?.phone);
    const portal = '';

    upserts.push({
      county_id: countyId,
      county_name: countyName,
      contact_name: contactName,
      email,
      portal,
      phone,
      verified: inferVerification({ email, portal, phone, contactName }),
      status: 'draft',
      notes: '',
      date_sent: '',
      deadline_10day: '',
      deadline_ag_45day: '',
      ag_notified_date: '',
      last_updated: nowIso,
      status_dates: { draft: nowIso },
      status_changed_at: nowIso,
      status_changed_by: 'System',
      status_log: [
        {
          type: 'status_change',
          from: null,
          to: 'draft',
          user: 'System',
          at: nowIso,
        },
      ],
    });
  }

  if (upserts.length || existingChanged) {
    await saveCountyRequests([...updatedExistingRows, ...upserts]);
  }

  return loadCountyRequests();
}

function buildAssociatedInstitutions(colleges) {
  const map = new Map();
  for (const college of colleges) {
    const associationCounties = parseCollegeAllCounties(college.allcounties, college.county);
    for (const county of associationCounties) {
      if (!county) continue;
      if (!map.has(county)) map.set(county, []);
      map.get(county).push({
        id: String(college.id || ''),
        institution: String(college.institution || ''),
        city: String(college.city || ''),
        verified: String(college.verified || 'partial'),
        public_records_email: String(college.public_records_email || ''),
        public_records_portal: String(college.public_records_portal || ''),
      });
    }
  }

  for (const list of map.values()) {
    list.sort((a, b) => a.institution.localeCompare(b.institution));
  }

  return map;
}

function buildEnrollmentByPrimaryCounty(colleges) {
  const map = new Map();
  for (const college of colleges) {
    const primaryCounty = String(college.county || '').trim();
    if (!primaryCounty) continue;
    const enrollment = Number.parseInt(String(college.enrollment_2025 || '').trim(), 10);
    if (!Number.isFinite(enrollment)) continue;
    map.set(primaryCounty, (map.get(primaryCounty) || 0) + enrollment);
  }
  return map;
}

export function buildCountyTemplateVariables(record, sender) {
  const today = new Date().toISOString().slice(0, 10);
  const statusDates = parseStatusDates(record?.status_dates);
  const dateSent = statusDates.sent || dateValueToIso(record?.date_sent) || today;
  const deadlineDate = dateValueToIso(record?.deadline_10day) || addBusinessDays(dateSent, 10);

  return {
    COUNTY_NAME: String(record?.county_name || ''),
    CONTACT_NAME: String(record?.contact_name || '[PUBLIC INFORMATION OFFICER]'),
    RECIPIENT_EMAIL: String(record?.email || '[RECIPIENT EMAIL]'),
    TODAY: today,
    DATE_SENT: dateSent,
    DEADLINE_DATE: deadlineDate,
    SENDER_NAME: String(sender?.name || '[YOUR NAME]'),
    SENDER_TITLE: String(sender?.title || '[YOUR TITLE]'),
    SENDER_ORG: String(sender?.org || '[YOUR ORGANIZATION]'),
    SENDER_EMAIL: String(sender?.email || '[YOUR EMAIL]'),
    SENDER_PHONE: String(sender?.phone || '[YOUR PHONE]'),
    SENDER_ADDRESS: String(sender?.address || '[YOUR ADDRESS]'),
  };
}

export async function renderCountyTemplate(templateName, variables) {
  const filePath = path.join(COUNTY_TEMPLATE_DIR, `${templateName}.txt`);
  const text = await fs.readFile(filePath, 'utf8');
  return Object.entries(variables).reduce((acc, [key, value]) => acc.replaceAll(`{{${key}}}`, String(value ?? '')), text);
}

export async function loadCountyState() {
  const [countyRequests, colleges, sender] = await Promise.all([ensureCountyRequestRows(), loadColleges(), loadSender()]);
  const associatedByCounty = buildAssociatedInstitutions(colleges);
  const enrollmentByPrimaryCounty = buildEnrollmentByPrimaryCounty(colleges);

  const countyRecords = countyRequests.map((row) => {
    const associatedInstitutions = associatedByCounty.get(String(row.county_name || '').trim()) || [];
    return {
      ...row,
      associated_institutions: associatedInstitutions,
      associated_institutions_count: associatedInstitutions.length,
      primary_county_enrollment_total: enrollmentByPrimaryCounty.get(String(row.county_name || '').trim()) || 0,
    };
  });

  const selectableCountyRecords = countyRecords.filter((row) => row.associated_institutions_count > 0);

  return {
    sender,
    countyRecords,
    selectableCountyRecords,
    countyTemplateLabels: COUNTY_TEMPLATE_LABELS,
    countySubjectLines: COUNTY_SUBJECT_LINES,
    countySuggestedTemplate: COUNTY_SUGGESTED_TEMPLATE,
  };
}
