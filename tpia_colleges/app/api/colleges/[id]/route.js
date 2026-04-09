import { NextResponse } from 'next/server';
import { countyFromCity, inferContactType, loadColleges, loadRequests, saveColleges, saveRequests } from '../../../../lib/data';

export async function PATCH(request, { params }) {
  try {
    const payload = await request.json();
    const id = params.id;
    const colleges = await loadColleges();
    const requests = await loadRequests();

    // Try to find by request_id first in requests
    let requestIndex = requests.findIndex((row) => String(row.request_id) === id);

    // Legacy fallback: some rows are keyed by id/institution_id instead of request_id
    if (requestIndex === -1) {
      requestIndex = requests.findIndex((row) => String(row.id || row.institution_id || '') === id);
    }
    
    // If not found and id looks like a fallback key, try parsing Institution-City-Index format
    if (requestIndex === -1 && id.includes('-')) {
      // Parse: Institution name may contain dashes, City typically doesn't, Index is numeric
      // Format: "Institution-City-Index" where Index is always numeric
      const parts = id.split('-');
      if (parts.length >= 3) {
        const city = parts[parts.length - 2];  // Second-to-last is city
        const institutionName = parts.slice(0, -2).join('-'); // Everything else is institution
        
        // Find by institution + city combo
        requestIndex = requests.findIndex(
          (row) => row.institution === institutionName && row.city === city
        );
      }
    }

    // Stable fallback format: Institution::City
    if (requestIndex === -1 && id.includes('::')) {
      const [institutionName, city] = id.split('::');
      requestIndex = requests.findIndex(
        (row) => row.institution === institutionName && row.city === city
      );
    }
    
    if (requestIndex === -1) {
      console.error(`Request with id=${id} not found in requests.csv`);
      return NextResponse.json({ error: 'College not found.' }, { status: 404 });
    }

    // Find matching college by institution name
    const request_record = requests[requestIndex];
    console.log(`Looking for college: "${request_record.institution}"`);
    const index = colleges.findIndex((row) => row.institution === request_record.institution);
    if (index === -1) {
      console.error(`College "${request_record.institution}" not found in colleges.csv`);
      return NextResponse.json({ error: 'College not found.' }, { status: 404 });
    }

    const current = colleges[index];
    const nextCity = payload.city == null ? current.city : String(payload.city).trim();
    const nextEmail = payload.public_records_email == null ? String(current.public_records_email || '').trim() : String(payload.public_records_email).trim();
    const nextPortal = payload.public_records_portal == null ? String(current.public_records_portal || '').trim() : String(payload.public_records_portal).trim();

    const next = {
      ...current,
      institution: payload.institution == null ? current.institution : String(payload.institution).trim(),
      type: payload.type == null ? current.type : String(payload.type),
      system_district: payload.system_district == null ? current.system_district : String(payload.system_district).trim(),
      city: nextCity,
      county: countyFromCity(nextCity),
      public_records_email: nextEmail,
      public_records_portal: nextPortal,
      contact_type: inferContactType(nextEmail, nextPortal),
      verified: payload.verified == null ? current.verified : String(payload.verified),
      notes: payload.notes == null ? current.notes : String(payload.notes).trim(),
      fee_amount: payload.fee_amount == null
        ? Number.parseFloat(String(current.fee_amount || '').trim()) || 0
        : Number.parseFloat(String(payload.fee_amount).trim()) || 0,
    };

    if (!next.institution) {
      return NextResponse.json({ error: 'Institution name is required.' }, { status: 400 });
    }

    colleges[index] = next;
    const updatedDate = new Date().toISOString();

    requests[requestIndex] = {
      ...requests[requestIndex],
      institution: next.institution,
      type: next.type,
      system_district: next.system_district,
      city: next.city,
      recipient_email: next.public_records_email,
      last_updated: updatedDate,
    };

    await saveColleges(colleges);
    await saveRequests(requests);
    return NextResponse.json({ ok: true, college: next });
  } catch (error) {
    console.error('Error in PATCH /api/colleges/[id]:', error);
    return NextResponse.json({ error: error.message }, { status: 500 });
  }
}
