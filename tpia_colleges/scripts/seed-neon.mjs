import fs from 'node:fs/promises';
import path from 'node:path';
import crypto from 'node:crypto';
import { parse } from 'csv-parse/sync';
import { Pool } from 'pg';

const ROOT = process.cwd();
const COLLEGES_PATH = path.join(ROOT, 'colleges.csv');
const REQUESTS_PATH = path.join(ROOT, 'requests.csv');
const SENDER_PATH = path.join(ROOT, 'sender.json');

const DATABASE_URL = process.env.DATABASE_URL;
if (!DATABASE_URL) {
  throw new Error('DATABASE_URL is required to seed Neon.');
}

const pool = new Pool({
  connectionString: DATABASE_URL,
  ssl: { rejectUnauthorized: false },
});

function cleanText(value) {
  return value == null ? '' : String(value).replace(/\s+$/g, '');
}

function hashCollegeId(institution, city, fallback = '') {
  const source = `${cleanText(institution).toLowerCase()}|${cleanText(city).toLowerCase()}|${cleanText(fallback).toLowerCase()}`;
  const digest = crypto.createHash('sha256').update(source).digest('hex').slice(0, 16);
  return `col_${digest}`;
}

function asDateOrNull(value) {
  const text = cleanText(value);
  if (!text) return null;
  if (!/^\d{4}-\d{2}-\d{2}$/.test(text)) return null;
  return text;
}

async function readCsv(filePath) {
  const text = await fs.readFile(filePath, 'utf8');
  if (!text.trim()) return [];
  return parse(text, {
    columns: true,
    skip_empty_lines: true,
    relax_quotes: true,
    relax_column_count: true,
    trim: true,
  }).map((row) => {
    const cleaned = {};
    for (const [key, value] of Object.entries(row)) {
      cleaned[key] = cleanText(value);
    }
    return cleaned;
  });
}

async function main() {
  const colleges = await readCsv(COLLEGES_PATH);
  const requests = await readCsv(REQUESTS_PATH).catch(() => []);
  let sender = {};
  try {
    sender = JSON.parse(await fs.readFile(SENDER_PATH, 'utf8'));
  } catch {
    sender = {};
  }

  const client = await pool.connect();
  try {
    await client.query('BEGIN');

    await client.query('TRUNCATE TABLE public.requests, public.colleges RESTART IDENTITY CASCADE');

    const collegeKeyToId = new Map();

    for (const row of colleges) {
      const id = cleanText(row.id) || hashCollegeId(row.institution, row.city);
      if (!id) continue;
      const collegeKey = `${cleanText(row.institution).toLowerCase()}|${cleanText(row.city).toLowerCase()}`;
      collegeKeyToId.set(collegeKey, id);
      await client.query(
        `
        INSERT INTO public.colleges (
          id, institution, type, system_district, city, county,
          public_records_email, public_records_portal, contact_type,
          verified, notes, enrollment_2025, fee_amount
        ) VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13)
        `,
        [
          id,
          cleanText(row.institution),
          cleanText(row.type),
          cleanText(row.system_district),
          cleanText(row.city),
          cleanText(row.county),
          cleanText(row.public_records_email),
          cleanText(row.public_records_portal),
          cleanText(row.contact_type),
          cleanText(row.verified),
          cleanText(row.notes),
          cleanText(row.enrollment_2025) ? Number.parseInt(cleanText(row.enrollment_2025), 10) || null : null,
          Number.parseFloat(cleanText(row.fee_amount || '0')) || 0,
        ],
      );
    }

    const validCollegeIds = new Set(collegeKeyToId.values());

    for (const row of requests) {
      const requestId = cleanText(row.request_id || row.institution_id || row.id);
      if (!requestId) continue;
      const institutionId = cleanText(row.institution_id || row.id || requestId);
      const key = `${cleanText(row.institution).toLowerCase()}|${cleanText(row.city).toLowerCase()}`;
      const mappedCollegeId = collegeKeyToId.get(key) || '';
      const linkedCollegeId = institutionId && validCollegeIds.has(institutionId)
        ? institutionId
        : (mappedCollegeId || null);
      await client.query(
        `
        INSERT INTO public.requests (
          request_id, id, institution, type, system_district,
          city, recipient_email, date_sent, status, deadline_10day,
          deadline_ag_45day, ag_notified_date, last_updated, notes,
          status_log, status_dates, status_changed_at, status_changed_by
        ) VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15::jsonb,$16::jsonb,$17,$18)
        ON CONFLICT (request_id) DO NOTHING
        `,
        [
          requestId,
          linkedCollegeId,
          cleanText(row.institution),
          cleanText(row.type),
          cleanText(row.system_district),
          cleanText(row.city),
          cleanText(row.recipient_email || row.public_records_email),
          asDateOrNull(row.date_sent),
          cleanText(row.status),
          asDateOrNull(row.deadline_10day),
          asDateOrNull(row.deadline_ag_45day),
          asDateOrNull(row.ag_notified_date),
          asDateOrNull(row.last_updated),
          cleanText(row.notes),
          JSON.stringify([
            {
              type: 'status_change',
              from: null,
              to: cleanText(row.status) || 'draft',
              user: 'Seed import',
              at: asDateOrNull(row.last_updated) || asDateOrNull(row.date_sent) || new Date().toISOString(),
            },
          ]),
          JSON.stringify({
            [cleanText(row.status) || 'draft']:
              asDateOrNull(row.last_updated) || asDateOrNull(row.date_sent) || new Date().toISOString(),
          }),
          asDateOrNull(row.last_updated) || asDateOrNull(row.date_sent),
          'Seed import',
        ],
      );
    }

    await client.query(
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
        cleanText(sender.name),
        cleanText(sender.title),
        cleanText(sender.org),
        cleanText(sender.email),
        cleanText(sender.phone),
        cleanText(sender.address),
      ],
    );

    await client.query('COMMIT');
    console.log(`Seeded ${colleges.length} colleges and ${requests.length} requests into Neon.`);
  } catch (error) {
    await client.query('ROLLBACK');
    throw error;
  } finally {
    client.release();
    await pool.end();
  }
}

main().catch((error) => {
  console.error(error);
  process.exit(1);
});
