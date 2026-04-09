import fs from 'node:fs/promises';
import path from 'node:path';
import crypto from 'node:crypto';
import { Pool } from 'pg';

export const ROOT = process.cwd();
export const TEMPLATES_DIR = path.join(ROOT, 'templates');

const DATABASE_URL = process.env.DATABASE_URL;

if (!DATABASE_URL) {
  throw new Error('DATABASE_URL is required. Set it to your Neon Postgres connection string.');
}

const pool = new Pool({
  connectionString: DATABASE_URL,
  ssl: { rejectUnauthorized: false },
});

let schemaReadyPromise = null;

export const STATUS_META = {
  draft: { label: 'Draft', color: '#667085', bg: '#F4F7FB', description: 'Not yet sent' },
  sent: { label: 'Sent', color: '#155EEF', bg: '#EAF2FF', description: 'Awaiting response' },
  acknowledged: { label: 'Acknowledged', color: '#7A4CE0', bg: '#F3ECFF', description: 'Receipt confirmed' },
  in_progress: { label: 'In Progress', color: '#C2410C', bg: '#FFF2E5', description: 'Agency is processing' },
  fee_pending: { label: 'Fee Pending', color: '#B45309', bg: '#FFF6E8', description: 'Awaiting cost decision' },
  fee_paid: { label: 'Fee Paid', color: '#7C2D12', bg: '#FCEFE8', description: 'Payment sent' },
  ag_opinion_requested: { label: 'AG Opinion', color: '#B42318', bg: '#FDECEC', description: 'AG ruling requested' },
  ag_opinion_pending: { label: 'AG Pending', color: '#9F1239', bg: '#FBE7F0', description: 'Waiting on agency' },
  denied: { label: 'Denied', color: '#C0262D', bg: '#FDE8E8', description: 'Request refused' },
  partially_complete: { label: 'Partial', color: '#15803D', bg: '#EAF8EF', description: 'Some records received' },
  complete: { label: 'Complete', color: '#166534', bg: '#DCFCE7', description: 'All records received' },
  withdrawn: { label: 'Withdrawn', color: '#475569', bg: '#EAEFF5', description: 'Request withdrawn' },
};

export const STATUS_KEYS = Object.keys(STATUS_META);

export const SUGGESTED_TEMPLATE = {
  draft: '01_initial_request',
  sent: '02_follow_up',
  acknowledged: '02_follow_up',
  in_progress: '02_follow_up',
  fee_pending: '03_fee_inquiry',
  fee_paid: '02_follow_up',
  ag_opinion_requested: '04_ag_opinion_response',
  ag_opinion_pending: '02_follow_up',
  denied: '05_denial_response',
  partially_complete: '06_records_received',
  complete: '06_records_received',
  withdrawn: '01_initial_request',
};

export const TEMPLATE_LABELS = {
  '01_initial_request': 'Initial request',
  '02_follow_up': 'Follow-up',
  '03_fee_inquiry': 'Fee estimate response',
  '04_ag_opinion_response': 'AG opinion response',
  '05_denial_response': 'Denial response',
  '06_records_received': 'Records received',
};

export const SUBJECT_LINES = {
  '01_initial_request': 'Texas Public Information Act Request - Student Directory Information',
  '02_follow_up': 'Follow-Up: TPIA Request - {institution}',
  '03_fee_inquiry': 'Re: Fee Estimate - TPIA Request - {institution}',
  '04_ag_opinion_response': 'Re: AG Opinion Notice - TPIA Request - {institution}',
  '05_denial_response': 'Re: Denial - TPIA Request - {institution}',
  '06_records_received': 'Confirmation: Records Received - {institution}',
};

const CITY_TO_COUNTY = {
  alpine: 'Brewster',
  alvin: 'Brazoria',
  amarillo: 'Potter',
  arlington: 'Tarrant',
  athens: 'Henderson',
  austin: 'Travis',
  baytown: 'Harris',
  beaumont: 'Jefferson',
  beeville: 'Bee',
  'big spring': 'Howard',
  borger: 'Hutchinson',
  brenham: 'Washington',
  brownsville: 'Cameron',
  canyon: 'Randall',
  carthage: 'Panola',
  cisco: 'Eastland',
  clarendon: 'Donley',
  'college station': 'Brazos',
  commerce: 'Hunt',
  'corpus christi': 'Nueces',
  corsicana: 'Navarro',
  dallas: 'Dallas',
  'del rio': 'Val Verde',
  denison: 'Grayson',
  denton: 'Denton',
  edinburg: 'Hidalgo',
  'el paso': 'El Paso',
  'fort worth': 'Tarrant',
  gainesville: 'Cooke',
  galveston: 'Galveston',
  hillsboro: 'Hill',
  houston: 'Harris',
  huntsville: 'Walker',
  kilgore: 'Gregg',
  killeen: 'Bell',
  kingsville: 'Kleberg',
  'lake jackson': 'Brazoria',
  laredo: 'Webb',
  levelland: 'Hockley',
  lubbock: 'Lubbock',
  lufkin: 'Angelina',
  mcallen: 'Hidalgo',
  mckinney: 'Collin',
  midland: 'Midland',
  'mount pleasant': 'Titus',
  nacogdoches: 'Nacogdoches',
  odessa: 'Ector',
  orange: 'Orange',
  paris: 'Lamar',
  pasadena: 'Harris',
  'port arthur': 'Jefferson',
  'prairie view': 'Waller',
  ranger: 'Eastland',
  richardson: 'Dallas',
  'san angelo': 'Tom Green',
  'san antonio': 'Bexar',
  'san marcos': 'Hays',
  snyder: 'Scurry',
  stephenville: 'Erath',
  temple: 'Bell',
  texarkana: 'Bowie',
  'texas city': 'Galveston',
  'the woodlands': 'Montgomery',
  tyler: 'Smith',
  uvalde: 'Uvalde',
  vernon: 'Wilbarger',
  victoria: 'Victoria',
  waco: 'McLennan',
  weatherford: 'Parker',
  wharton: 'Wharton',
  'wichita falls': 'Wichita',
};

const MULTI_COUNTY_OVERRIDES = {
  'sul ross state university system (all campuses)': ['Brewster', 'Val Verde', 'Maverick', 'Uvalde'],
  'university of north texas system (all campuses)': ['Denton', 'Dallas', 'Tarrant'],
  'texas tech university system (all campuses)': ['Lubbock', 'Tom Green'],
  'texas state technical college': ['McLennan', 'Bell', 'Taylor', 'Hidalgo', 'El Paso'],
  'austin community college district': ['Travis', 'Williamson', 'Hays', 'Bastrop'],
  'blinn college district': ['Washington', 'Brazos', 'Waller'],
  'lone star college system': ['Montgomery', 'Harris'],
};

function normalizeCity(city) {
  return String(city || '')
    .replace(/\(.*?\)/g, '')
    .replace(/\s+/g, ' ')
    .trim()
    .toLowerCase();
}

function normalizeOutputRow(row) {
  const normalized = {};
  for (const [key, value] of Object.entries(row)) {
    normalized[key] = value == null ? '' : String(value);
  }
  return normalized;
}

function normalizeCountyToken(value) {
  return String(value || '').replace(/\s+/g, ' ').trim();
}

function dedupeCountyList(list) {
  const seen = new Set();
  const ordered = [];
  for (const county of list) {
    const normalized = normalizeCountyToken(county);
    if (!normalized) continue;
    const key = normalized.toLowerCase();
    if (seen.has(key)) continue;
    seen.add(key);
    ordered.push(normalized);
  }
  return ordered;
}

export function parseCollegeAllCounties(value, fallbackCounty = '') {
  const fallback = normalizeCountyToken(fallbackCounty);
  if (Array.isArray(value)) {
    const parsed = dedupeCountyList(value);
    return parsed.length ? parsed : (fallback ? [fallback] : []);
  }

  if (value == null || value === '') {
    return fallback ? [fallback] : [];
  }

  try {
    const parsed = typeof value === 'string' ? JSON.parse(value) : value;
    if (Array.isArray(parsed)) {
      const list = dedupeCountyList(parsed);
      return list.length ? list : (fallback ? [fallback] : []);
    }
  } catch {
    // Fall through to string parsing below.
  }

  const list = dedupeCountyList(String(value).split(',').map((item) => item.trim()));
  return list.length ? list : (fallback ? [fallback] : []);
}

function deriveAllCountiesForCollege(college, collegesBySystemDistrict) {
  const primaryCounty = normalizeCountyToken(college.county);
  const institutionKey = String(college.institution || '').trim().toLowerCase();
  const systemDistrict = String(college.system_district || '').trim();

  const counties = [];
  if (primaryCounty) counties.push(primaryCounty);

  const override = MULTI_COUNTY_OVERRIDES[institutionKey] || [];
  counties.push(...override);

  // For explicit consolidated rows, expand by counties represented across the same system in this dataset.
  if (institutionKey.includes('all campuses') && systemDistrict) {
    const fromSystem = collegesBySystemDistrict.get(systemDistrict) || [];
    counties.push(...fromSystem);
  }

  return dedupeCountyList(counties);
}

function enrichCollegeAllCounties(colleges) {
  const bySystemDistrict = new Map();
  for (const college of colleges) {
    const systemDistrict = String(college.system_district || '').trim();
    const primaryCounty = normalizeCountyToken(college.county);
    if (!systemDistrict || !primaryCounty) continue;
    if (!bySystemDistrict.has(systemDistrict)) bySystemDistrict.set(systemDistrict, new Set());
    bySystemDistrict.get(systemDistrict).add(primaryCounty);
  }

  const bySystemDistrictArray = new Map(
    Array.from(bySystemDistrict.entries()).map(([system, counties]) => [system, Array.from(counties)]),
  );

  return colleges.map((college) => {
    const existing = parseCollegeAllCounties(college.allcounties, college.county);
    const derived = deriveAllCountiesForCollege(college, bySystemDistrictArray);

    const merged = dedupeCountyList([...existing, ...derived]);
    return {
      ...college,
      county: normalizeCountyToken(college.county),
      allcounties: merged,
    };
  });
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

  // Browser Date.toString() often includes trailing timezone labels in parentheses.
  const normalizedText = text.replace(/\s*\([^)]*\)\s*$/, '').trim();

  const parsed = new Date(normalizedText || text);
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
        return {
          type: String(entry.type || 'status_change'),
          from: entry.from == null ? null : String(entry.from),
          to: String(entry.to || ''),
          user: String(entry.user || 'System'),
          at: dateValueToIso(entry.at) || new Date().toISOString(),
        };
      })
      .filter((entry) => entry && entry.to);
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
      if (!normalized) return acc;
      acc[String(status)] = normalized;
      return acc;
    }, {});
  } catch {
    return {};
  }
}

async function ensureTrackerSchema() {
  if (schemaReadyPromise) {
    return schemaReadyPromise;
  }

  schemaReadyPromise = (async () => {
    const client = await pool.connect();
    try {
      await client.query('BEGIN');

      await client.query(`
        ALTER TABLE public.colleges
        ADD COLUMN IF NOT EXISTS fee_amount numeric(10,2) NOT NULL DEFAULT 0,
        ADD COLUMN IF NOT EXISTS allcounties jsonb NOT NULL DEFAULT '[]'::jsonb
      `);

      await client.query(`
        UPDATE public.colleges
        SET allcounties = to_jsonb(ARRAY[NULLIF(trim(county), '')])
        WHERE jsonb_typeof(COALESCE(allcounties, '[]'::jsonb)) = 'array'
          AND jsonb_array_length(COALESCE(allcounties, '[]'::jsonb)) = 0
          AND NULLIF(trim(county), '') IS NOT NULL
      `);

      await client.query(`
        ALTER TABLE public.requests
        ADD COLUMN IF NOT EXISTS status_log jsonb NOT NULL DEFAULT '[]'::jsonb,
        ADD COLUMN IF NOT EXISTS status_dates jsonb NOT NULL DEFAULT '{}'::jsonb,
        ADD COLUMN IF NOT EXISTS status_changed_at timestamptz,
        ADD COLUMN IF NOT EXISTS status_changed_by text
      `);

      await client.query(`
        UPDATE public.requests
        SET status_dates = jsonb_set(
          COALESCE(status_dates, '{}'::jsonb),
          ARRAY[COALESCE(NULLIF(status, ''), 'draft')]::text[],
          to_jsonb(COALESCE(status_changed_at, last_updated, date_sent, NOW())::text),
          true
        )
        WHERE NOT (COALESCE(status_dates, '{}'::jsonb) ? COALESCE(NULLIF(status, ''), 'draft'))
      `);

      await client.query(`
        UPDATE public.requests
        SET status_log = jsonb_build_array(
          jsonb_build_object(
            'type', 'status_change',
            'from', NULL,
            'to', COALESCE(NULLIF(status, ''), 'draft'),
            'user', COALESCE(NULLIF(status_changed_by, ''), 'System'),
            'at', COALESCE(status_changed_at, last_updated, date_sent, NOW())
          )
        )
        WHERE jsonb_typeof(COALESCE(status_log, '[]'::jsonb)) = 'array'
          AND jsonb_array_length(COALESCE(status_log, '[]'::jsonb)) = 0
      `);

      await client.query('COMMIT');
    } catch (error) {
      await client.query('ROLLBACK');
      throw error;
    } finally {
      client.release();
    }
  })().catch((error) => {
    schemaReadyPromise = null;
    throw error;
  });

  return schemaReadyPromise;
}

export function generateCollegeId(institution, city, fallback = '') {
  const source = `${String(institution || '').trim().toLowerCase()}|${String(city || '').trim().toLowerCase()}|${String(fallback || '').trim().toLowerCase()}`;
  const digest = crypto.createHash('sha256').update(source).digest('hex').slice(0, 16);
  return `col_${digest}`;
}

export function inferContactType(email, portal) {
  const hasEmail = String(email || '').trim().length > 0;
  const hasPortal = String(portal || '').trim().length > 0;
  if (hasEmail && hasPortal) return 'both';
  if (hasPortal) return 'portal';
  return 'email';
}

export function countyFromCity(city) {
  const normalized = normalizeCity(city);
  return CITY_TO_COUNTY[normalized] || '';
}

export function getTexasCounties() {
  const counties = new Set(Object.values(CITY_TO_COUNTY));
  return Array.from(counties).sort();
}

function toDate(value) {
  const parsed = new Date(value);
  return Number.isNaN(parsed.getTime()) ? new Date() : parsed;
}

function isoDate(value) {
  return toDate(value).toISOString().slice(0, 10);
}

function formatHumanDate(value) {
  const date = toDate(value);
  return new Intl.DateTimeFormat('en-US', {
    month: 'long',
    day: 'numeric',
    year: 'numeric',
    timeZone: 'UTC',
  }).format(date);
}

function formatCurrency(value) {
  const number = Number.parseFloat(String(value ?? '').trim());
  const safeNumber = Number.isFinite(number) ? number : 0;
  return new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency: 'USD',
    minimumFractionDigits: 2,
    maximumFractionDigits: 2,
  }).format(safeNumber);
}

function txHolidays(year) {
  const holidays = new Set();
  const add = (date) => holidays.add(date.toISOString().slice(0, 10));
  const nthWeekday = (y, month, n, weekday) => {
    const first = new Date(Date.UTC(y, month - 1, 1));
    const delta = (weekday - first.getUTCDay() + 7) % 7;
    return new Date(Date.UTC(y, month - 1, 1 + delta + 7 * (n - 1)));
  };
  const lastWeekday = (y, month, weekday) => {
    const last = new Date(Date.UTC(y, month, 0));
    const delta = (last.getUTCDay() - weekday + 7) % 7;
    return new Date(Date.UTC(y, month - 1, last.getUTCDate() - delta));
  };

  [
    new Date(Date.UTC(year, 0, 1)),
    new Date(Date.UTC(year, 2, 2)),
    new Date(Date.UTC(year, 3, 21)),
    new Date(Date.UTC(year, 5, 19)),
    new Date(Date.UTC(year, 6, 4)),
    new Date(Date.UTC(year, 7, 27)),
    new Date(Date.UTC(year, 10, 11)),
    new Date(Date.UTC(year, 11, 24)),
    new Date(Date.UTC(year, 11, 25)),
    new Date(Date.UTC(year, 11, 26)),
    nthWeekday(year, 1, 3, 1),
    nthWeekday(year, 2, 3, 1),
    lastWeekday(year, 5, 1),
    nthWeekday(year, 9, 1, 1),
    nthWeekday(year, 10, 2, 1),
    nthWeekday(year, 11, 4, 4),
    new Date(Date.UTC(year, 10, nthWeekday(year, 11, 4, 4).getUTCDate() + 1)),
  ].forEach(add);

  return holidays;
}

export function addBusinessDays(startValue, count) {
  let cursor = toDate(startValue);
  const startYear = cursor.getUTCFullYear();
  const holidays = new Set([...txHolidays(startYear), ...txHolidays(startYear + 1)]);
  let added = 0;

  while (added < count) {
    cursor = new Date(cursor.getTime() + 24 * 60 * 60 * 1000);
    const iso = cursor.toISOString().slice(0, 10);
    const weekday = cursor.getUTCDay();
    if (weekday !== 0 && weekday !== 6 && !holidays.has(iso)) {
      added += 1;
    }
  }

  return isoDate(cursor);
}

export async function loadColleges() {
  await ensureTrackerSchema();
  const { rows } = await pool.query(`
    SELECT id, institution, type, system_district, city, county,
           public_records_email, public_records_portal, contact_type,
           verified, notes, enrollment_2025, fee_amount, allcounties
    FROM public.colleges
    ORDER BY institution ASC, city ASC
  `);
  const normalized = rows.map((row) => {
    const base = normalizeOutputRow(row);
    return {
      ...base,
      allcounties: parseCollegeAllCounties(row.allcounties, row.county),
    };
  });
  return enrichCollegeAllCounties(normalized);
}

export async function loadRequests() {
  await ensureTrackerSchema();
  const { rows } = await pool.query(`
    SELECT request_id, id AS institution_id, institution, type, system_district,
           city, recipient_email, date_sent, status, deadline_10day,
           deadline_ag_45day, ag_notified_date, last_updated, notes,
           status_log, status_dates, status_changed_at, status_changed_by
    FROM public.requests
    ORDER BY institution ASC, city ASC
  `);

  return rows.map((row) => {
    const normalized = normalizeOutputRow(row);
    const normalizedId = String(normalized.request_id || normalized.institution_id || '').trim();
    return {
      ...normalized,
      request_id: normalizedId,
      institution_id: String(normalized.institution_id || normalizedId),
      status: String(normalized.status || '').trim() || 'draft',
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

export async function saveColleges(rows) {
  await ensureTrackerSchema();
  const client = await pool.connect();
  try {
    await client.query('BEGIN');

    const ids = rows
      .map((row) => String(row.id || '').trim() || generateCollegeId(row.institution, row.city))
      .filter(Boolean);
    if (ids.length) {
      await client.query('DELETE FROM public.colleges WHERE id <> ALL($1::text[])', [ids]);
    } else {
      await client.query('TRUNCATE TABLE public.colleges CASCADE');
    }

    for (const row of rows) {
      const id = String(row.id || '').trim() || generateCollegeId(row.institution, row.city);
      if (!id) continue;
      await client.query(
        `
        INSERT INTO public.colleges (
          id, institution, type, system_district, city, county,
          public_records_email, public_records_portal, contact_type,
          verified, notes, enrollment_2025, fee_amount, allcounties
        ) VALUES (
          $1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14::jsonb
        )
        ON CONFLICT (id) DO UPDATE SET
          institution = EXCLUDED.institution,
          type = EXCLUDED.type,
          system_district = EXCLUDED.system_district,
          city = EXCLUDED.city,
          county = EXCLUDED.county,
          public_records_email = EXCLUDED.public_records_email,
          public_records_portal = EXCLUDED.public_records_portal,
          contact_type = EXCLUDED.contact_type,
          verified = EXCLUDED.verified,
          notes = EXCLUDED.notes,
          enrollment_2025 = EXCLUDED.enrollment_2025,
          fee_amount = EXCLUDED.fee_amount,
          allcounties = EXCLUDED.allcounties
        `,
        [
          id,
          String(row.institution || '').trim(),
          String(row.type || ''),
          String(row.system_district || '').trim(),
          String(row.city || '').trim(),
          String(row.county || '').trim(),
          String(row.public_records_email || '').trim(),
          String(row.public_records_portal || '').trim(),
          String(row.contact_type || '').trim(),
          String(row.verified || '').trim(),
          String(row.notes || '').trim(),
          row.enrollment_2025 ? Number.parseInt(String(row.enrollment_2025), 10) || null : null,
          Number.parseFloat(String(row.fee_amount || '').trim()) || 0,
          JSON.stringify(parseCollegeAllCounties(row.allcounties, row.county)),
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

export async function saveRequests(rows) {
  await ensureTrackerSchema();
  const client = await pool.connect();
  try {
    await client.query('BEGIN');

    const collegeIdsResult = await client.query('SELECT id FROM public.colleges');
    const validCollegeIds = new Set(collegeIdsResult.rows.map((row) => String(row.id)));

    const requestIds = rows.map((row) => String(row.request_id || '').trim()).filter(Boolean);
    if (requestIds.length) {
      await client.query('DELETE FROM public.requests WHERE request_id <> ALL($1::text[])', [requestIds]);
    } else {
      await client.query('TRUNCATE TABLE public.requests CASCADE');
    }

    for (const row of rows) {
      const requestId = String(row.request_id || '').trim();
      if (!requestId) continue;
      const institutionId = String(row.institution_id || row.id || '').trim();
      const linkedCollegeId = institutionId && validCollegeIds.has(institutionId) ? institutionId : null;
      await client.query(
        `
        INSERT INTO public.requests (
          request_id, id, institution, type, system_district,
          city, recipient_email, date_sent, status, deadline_10day,
          deadline_ag_45day, ag_notified_date, last_updated, notes,
          status_log, status_dates, status_changed_at, status_changed_by
        ) VALUES (
          $1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15::jsonb,$16::jsonb,$17,$18
        )
        ON CONFLICT (request_id) DO UPDATE SET
          id = EXCLUDED.id,
          institution = EXCLUDED.institution,
          type = EXCLUDED.type,
          system_district = EXCLUDED.system_district,
          city = EXCLUDED.city,
          recipient_email = EXCLUDED.recipient_email,
          date_sent = EXCLUDED.date_sent,
          status = EXCLUDED.status,
          deadline_10day = EXCLUDED.deadline_10day,
          deadline_ag_45day = EXCLUDED.deadline_ag_45day,
          ag_notified_date = EXCLUDED.ag_notified_date,
          last_updated = EXCLUDED.last_updated,
          notes = EXCLUDED.notes,
          status_log = EXCLUDED.status_log,
          status_dates = EXCLUDED.status_dates,
          status_changed_at = EXCLUDED.status_changed_at,
          status_changed_by = EXCLUDED.status_changed_by
        `,
        [
          requestId,
          linkedCollegeId,
          String(row.institution || '').trim(),
          String(row.type || '').trim(),
          String(row.system_district || '').trim(),
          String(row.city || '').trim(),
          String(row.recipient_email || '').trim(),
          dateValueToDb(row.date_sent),
          String(row.status || '').trim() || 'draft',
          dateValueToDb(row.deadline_10day),
          dateValueToDb(row.deadline_ag_45day),
          dateValueToDb(row.ag_notified_date),
          dateValueToDb(row.last_updated),
          String(row.notes || '').trim(),
          JSON.stringify(parseStatusLog(row.status_log)),
          JSON.stringify(parseStatusDates(row.status_dates)),
          dateValueToDb(row.status_changed_at),
          String(row.status_changed_by || '').trim() || null,
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

export async function loadSender() {
  const { rows } = await pool.query(
    `
    SELECT name, title, org, email, phone, address
    FROM public.sender_profile
    WHERE singleton_id = true
    LIMIT 1
    `,
  );

  if (!rows.length) {
    return {
      name: '',
      title: '',
      org: 'The Annette Strauss Institute for Civic Life, The University of Texas at Austin',
      email: '',
      phone: '',
      address: '2504 Whitis Ave, Austin, TX 78712',
    };
  }

  return normalizeOutputRow(rows[0]);
}

export async function saveSender(sender) {
  await pool.query(
    `
    INSERT INTO public.sender_profile (singleton_id, name, title, org, email, phone, address)
    VALUES (true, $1, $2, $3, $4, $5, $6)
    ON CONFLICT (singleton_id) DO UPDATE SET
      name = EXCLUDED.name,
      title = EXCLUDED.title,
      org = EXCLUDED.org,
      email = EXCLUDED.email,
      phone = EXCLUDED.phone,
      address = EXCLUDED.address
    `,
    [
      String(sender.name || ''),
      String(sender.title || ''),
      String(sender.org || ''),
      String(sender.email || ''),
      String(sender.phone || ''),
      String(sender.address || ''),
    ],
  );
}

export function collegeToRequestRow(college) {
  const nowIso = new Date().toISOString();
  return {
    request_id: String(college.id || ''),
    institution_id: String(college.id || ''),
    institution: String(college.institution || ''),
    type: String(college.type || ''),
    system_district: String(college.system_district || ''),
    city: String(college.city || ''),
    recipient_email: String(college.public_records_email || ''),
    date_sent: '',
    status: 'draft',
    deadline_10day: '',
    deadline_ag_45day: '',
    ag_notified_date: '',
    last_updated: nowIso,
    notes: '',
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
  };
}

export function mergeRecords(colleges, requests) {
  const requestMap = new Map(requests.map((row) => [String(row.request_id), row]));
  const requestByInstitutionCity = new Map(
    requests.map((row) => [
      `${String(row.institution || '').trim().toLowerCase()}|${String(row.city || '').trim().toLowerCase()}`,
      row,
    ]),
  );

  return colleges.map((college) => {
    const byId = requestMap.get(String(college.id || ''));
    const byInstitutionCity = requestByInstitutionCity.get(
      `${String(college.institution || '').trim().toLowerCase()}|${String(college.city || '').trim().toLowerCase()}`,
    );
    const request = byId || byInstitutionCity || collegeToRequestRow(college);
    const email = String(college.public_records_email || '');
    const portal = String(college.public_records_portal || '');

    return {
      ...request,
      ...college,
      public_records_email: email,
      public_records_portal: portal,
      contact_type: inferContactType(email, portal),
      verified: String(college.verified || 'partial'),
      notes: request.notes,
    };
  });
}

export async function ensureRequestRows() {
  const colleges = await loadColleges();
  const requests = await loadRequests();
  const requestIds = new Set(requests.map((row) => String(row.request_id)));
  const requestInstitutionCity = new Set(
    requests.map((row) => `${String(row.institution || '').trim().toLowerCase()}|${String(row.city || '').trim().toLowerCase()}`),
  );
  const missing = colleges
    .filter((college) => {
      const collegeId = String(college.id || '').trim();
      const key = `${String(college.institution || '').trim().toLowerCase()}|${String(college.city || '').trim().toLowerCase()}`;
      return !requestIds.has(collegeId) && !requestInstitutionCity.has(key);
    })
    .map(collegeToRequestRow);

  if (missing.length) {
    const next = [...requests, ...missing];
    await saveRequests(next);
    return next;
  }

  return requests;
}

export async function loadAppState() {
  const colleges = await loadColleges();
  const requests = await ensureRequestRows();
  const sender = await loadSender();
  return {
    colleges,
    requests,
    sender,
    records: mergeRecords(colleges, requests),
  };
}

export function buildTemplateVariables(record, sender) {
  const today = new Date().toISOString().slice(0, 10);
  const statusDates = parseStatusDates(record?.status_dates);
  const dateSent = statusDates.sent || dateValueToIso(record?.date_sent) || today;
  const feeRequestedDate = statusDates.fee_pending || '';
  const feePaidDate = statusDates.fee_paid || '';
  const agRequestedDate = statusDates.ag_opinion_requested || dateValueToIso(record?.ag_notified_date) || '';
  const deadlineDate = addBusinessDays(today, 10);
  return {
    INSTITUTION: String(record?.institution || ''),
    CITY: String(record?.city || ''),
    SENDER_NAME: String(sender?.name || '[YOUR NAME]'),
    SENDER_TITLE: String(sender?.title || '[YOUR TITLE]'),
    SENDER_ORG: String(sender?.org || '[YOUR ORGANIZATION]'),
    SENDER_EMAIL: String(sender?.email || '[YOUR EMAIL]'),
    SENDER_PHONE: String(sender?.phone || '[YOUR PHONE]'),
    SENDER_ADDRESS: String(sender?.address || '[YOUR ADDRESS]'),
    TODAY: today,
    TODAY_HUMAN: formatHumanDate(today),
    DATE_SENT: dateSent,
    DATE_SENT_HUMAN: formatHumanDate(dateSent),
    DEADLINE_DATE: deadlineDate,
    DEADLINE_DATE_HUMAN: formatHumanDate(deadlineDate),
    BUSINESS_DAYS_ELAPSED: String(Math.max(0, Math.floor((toDate(today) - toDate(dateSent)) / (24 * 60 * 60 * 1000)))),
    FEE_REQUESTED_DATE: feeRequestedDate,
    FEE_REQUESTED_DATE_HUMAN: feeRequestedDate ? formatHumanDate(feeRequestedDate) : '',
    FEE_PAID_DATE: feePaidDate,
    FEE_PAID_DATE_HUMAN: feePaidDate ? formatHumanDate(feePaidDate) : '',
    AG_REQUESTED_DATE: agRequestedDate,
    AG_REQUESTED_DATE_HUMAN: agRequestedDate ? formatHumanDate(agRequestedDate) : '',
    STATUS_CHANGED_AT: dateValueToIso(record?.status_changed_at) || '',
    STATUS_CHANGED_AT_HUMAN: record?.status_changed_at ? formatHumanDate(record.status_changed_at) : '',
    STATUS_CHANGED_BY: String(record?.status_changed_by || ''),
    AG_LETTER_NUMBER: String(record?.ag_letter_number || '[AG LETTER NO. - check AG notice]'),
    FEE_AMOUNT: formatCurrency(record?.fee_amount),
    FEE_AMOUNT_RAW: String(record?.fee_amount || '0'),
    DENIAL_BASIS: String(record?.denial_basis || '[CITED EXCEPTION - from denial letter]'),
  };
}

export async function renderTemplate(templateName, variables) {
  const filePath = path.join(TEMPLATES_DIR, `${templateName}.txt`);
  const text = await fs.readFile(filePath, 'utf8');
  return Object.entries(variables).reduce((acc, [key, value]) => acc.replaceAll(`{{${key}}}`, String(value)), text);
}
