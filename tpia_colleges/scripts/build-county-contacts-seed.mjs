import fs from 'node:fs';
import path from 'node:path';

const url = 'https://www.sos.state.tx.us/elections/voter/county.shtml';

function toTitleCase(text) {
  return String(text || '')
    .toLowerCase()
    .replace(/\b\w/g, (char) => char.toUpperCase())
    .trim();
}

function stripTags(text) {
  return String(text || '')
    .replace(/<[^>]+>/g, ' ')
    .replace(/&nbsp;/g, ' ')
    .replace(/\s+/g, ' ')
    .trim();
}

function normalizePhone(value) {
  const text = String(value || '').trim();
  if (!text) return '';

  const match = text.match(/\(?\d{3}\)?[\s.-]*\d{3}[\s.-]*\d{4}(?:\s*(?:x|ext\.?|extension)\s*\d+)?/i);
  return match ? match[0].replace(/\s+/g, ' ').trim() : '';
}

const response = await fetch(url);
if (!response.ok) {
  throw new Error(`Failed to fetch county contacts: ${response.status} ${response.statusText}`);
}

const html = await response.text();
const blocks = [...html.matchAll(/<dl>([\s\S]*?)<\/dl>/gi)].map((match) => match[1]);

const contacts = [];
for (const block of blocks) {
  const countyMatch = block.match(/<dt>\s*<strong>(?:<a[^>]*>)?\s*([A-Z\s]+?)\s+COUNTY\s*(?:<\/a>)?\s*<\/strong>\s*<\/dt>/i);
  if (!countyMatch) continue;

  const countyName = toTitleCase(countyMatch[1].replace(/\s+/g, ' '));
  if (!countyName) continue;

  const details = [...block.matchAll(/<dd>([\s\S]*?)<\/dd>/gi)]
    .map((match) => stripTags(match[1]))
    .filter(Boolean);

  const links = [...block.matchAll(/href=\"mailto:([^\"\?]+)[^\"]*\"[^>]*>([\s\S]*?)<\/a>/gi)]
    .map((match) => ({
      email: String(match[1] || '').trim().toLowerCase(),
      label: stripTags(match[2]),
    }))
    .filter((entry) => entry.email);

  const role = String(details[0] || '').slice(0, 140);
  const name = String(details[1] || '').slice(0, 140);
  const phone = details.map(normalizePhone).find(Boolean) || '';

  const preferred = links.find((entry) => /county email address/i.test(entry.label));
  const fallback = links[0];
  const email = (preferred || fallback || {}).email || '';

  contacts.push({
    county_name: countyName,
    contact_name: name,
    contact_title: role,
    email,
    phone,
    source: 'texas_sos_election_duties',
    source_url: url,
  });
}

contacts.sort((a, b) => a.county_name.localeCompare(b.county_name));

const outDir = path.join(process.cwd(), 'data');
fs.mkdirSync(outDir, { recursive: true });
const outPath = path.join(outDir, 'county_contacts_seed.json');
fs.writeFileSync(outPath, JSON.stringify(contacts, null, 2));

console.log(`WROTE=${contacts.length}`);
console.log(`OUT=${outPath}`);
