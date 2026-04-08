import crypto from 'node:crypto';
import { Pool } from 'pg';

const DATABASE_URL = process.env.DATABASE_URL;
if (!DATABASE_URL) {
  throw new Error('DATABASE_URL is required to migrate college IDs.');
}

const pool = new Pool({
  connectionString: DATABASE_URL,
  ssl: { rejectUnauthorized: false },
});

function hashCollegeId(institution, city, fallback = '') {
  const source = `${String(institution || '').trim().toLowerCase()}|${String(city || '').trim().toLowerCase()}|${String(fallback || '').trim().toLowerCase()}`;
  const digest = crypto.createHash('sha256').update(source).digest('hex').slice(0, 16);
  return `col_${digest}`;
}

async function main() {
  const client = await pool.connect();
  try {
    await client.query('BEGIN');

    const { rows: colleges } = await client.query(
      'SELECT id, institution, city FROM public.colleges ORDER BY institution ASC, city ASC',
    );

    const usedIds = new Set();
    const mapping = [];

    for (const row of colleges) {
      const oldId = String(row.id || '').trim();
      if (!oldId) continue;

      let nextId = hashCollegeId(row.institution, row.city);
      if (usedIds.has(nextId)) {
        nextId = hashCollegeId(row.institution, row.city, oldId);
      }
      usedIds.add(nextId);

      if (nextId !== oldId) {
        mapping.push({ oldId, nextId });
      }
    }

    if (!mapping.length) {
      await client.query('COMMIT');
      console.log('No college ID changes needed.');
      return;
    }

    await client.query('ALTER TABLE public.requests DROP CONSTRAINT IF EXISTS requests_college_id_fk');

    for (const { oldId, nextId } of mapping) {
      await client.query('UPDATE public.colleges SET id = $1 WHERE id = $2', [nextId, oldId]);
      await client.query('UPDATE public.requests SET id = $1 WHERE id = $2', [nextId, oldId]);
      await client.query(
        `
        UPDATE public.requests
        SET request_id = $1
        WHERE request_id = $2
          AND NOT EXISTS (
            SELECT 1 FROM public.requests r2 WHERE r2.request_id = $1
          )
        `,
        [nextId, oldId],
      );
    }

    await client.query(`
      ALTER TABLE public.requests
      ADD CONSTRAINT requests_college_id_fk
      FOREIGN KEY (id)
      REFERENCES public.colleges(id)
      ON DELETE SET NULL
      ON UPDATE CASCADE
    `);

    await client.query('COMMIT');
    console.log(`Migrated ${mapping.length} college IDs to hash IDs.`);
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
