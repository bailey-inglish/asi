import { NextResponse } from 'next/server';
import { loadColleges, loadRequests, saveColleges, saveRequests, collegeToRequestRow, countyFromCity, inferContactType, generateCollegeId } from '../../../lib/data';

function normalizeVerifiedValue(value) {
  const status = String(value || '').trim().toLowerCase();
  if (status === 'confirmed') return 'yes';
  if (status === 'incomplete') return 'partial';
  if (status === 'yes' || status === 'partial' || status === 'no') return status;
  return 'partial';
}

export async function POST(request) {
  const payload = await request.json();
  const colleges = await loadColleges();
  const requests = await loadRequests();

  const id = String(payload.id || '').trim() || generateCollegeId(payload.institution, payload.city);
  if (colleges.some((row) => String(row.id) === id)) {
    return NextResponse.json({ error: 'That college ID already exists.' }, { status: 409 });
  }

  const publicRecordsEmail = String(payload.public_records_email || '').trim();
  const publicRecordsPortal = String(payload.public_records_portal || '').trim();

  const college = {
    id,
    institution: String(payload.institution || '').trim(),
    type: String(payload.type || '4yr'),
    system_district: String(payload.system_district || '').trim(),
    city: String(payload.city || '').trim(),
    county: countyFromCity(payload.city),
    public_records_email: publicRecordsEmail,
    public_records_portal: publicRecordsPortal,
    contact_type: inferContactType(publicRecordsEmail, publicRecordsPortal),
    verified: normalizeVerifiedValue(payload.verified),
    notes: String(payload.notes || '').trim(),
    fee_amount: Number.parseFloat(String(payload.fee_amount || '').trim()) || 0,
  };

  if (!college.institution) {
    return NextResponse.json({ error: 'Institution name is required.' }, { status: 400 });
  }

  const nextColleges = [...colleges, college];
  const nextRequests = [...requests, collegeToRequestRow(college)];
  await saveColleges(nextColleges);
  await saveRequests(nextRequests);
  return NextResponse.json({ ok: true, college });
}
