import { NextResponse } from 'next/server';
import { loadRequests, saveRequests } from '../../../../lib/data';

export async function PATCH(request, { params }) {
  try {
    const payload = await request.json();
    const id = params.id;
    const requests = await loadRequests();
    
    // Try to find by request_id first
    let index = requests.findIndex((row) => String(row.request_id) === id);

    // Legacy fallback: some rows are keyed by id/institution_id instead of request_id
    if (index === -1) {
      index = requests.findIndex((row) => String(row.id || row.institution_id || '') === id);
    }

    // If not found and id looks like a fallback key, parse Institution-City-Index format
    if (index === -1 && id.includes('-')) {
      // Format: "Institution-City-Index" where Index is numeric
      const parts = id.split('-');
      if (parts.length >= 3) {
        const city = parts[parts.length - 2]; // Second-to-last is city
        const institutionName = parts.slice(0, -2).join('-'); // Everything else is institution

        // Find by institution + city combo
        index = requests.findIndex(
          (row) => row.institution === institutionName && row.city === city
        );
      }
    }

    // Stable fallback format: Institution::City
    if (index === -1 && id.includes('::')) {
      const [institutionName, city] = id.split('::');
      index = requests.findIndex(
        (row) => row.institution === institutionName && row.city === city
      );
    }
    
    if (index === -1) {
      console.error(`Request with id=${id} not found in requests.csv`);
      return NextResponse.json({ error: 'Request not found.' }, { status: 404 });
    }

    const current = requests[index];
    const nowIso = new Date().toISOString();
    const mergedDate = nowIso;
    const next = {
      ...current,
      status: payload.status == null ? current.status : String(payload.status),
      date_sent: mergedDate,
      ag_notified_date: mergedDate,
      notes: payload.notes == null ? current.notes : String(payload.notes),
      deadline_10day: payload.deadline_10day == null ? current.deadline_10day : payload.deadline_10day,
      deadline_ag_45day: payload.deadline_ag_45day == null ? current.deadline_ag_45day : payload.deadline_ag_45day,
      last_updated: mergedDate,
    };

    if (payload.append_note) {
      const note = String(payload.append_note).trim();
      if (note) {
        const author = String(payload.note_author || '').trim();
        const noteDate = nowIso.slice(0, 10);
        const noteEntry = `[${noteDate}] ${author ? `${author}: ${note}` : note}`;
        next.notes = [current.notes, noteEntry].filter(Boolean).join(' | ');
      }
    }

    requests[index] = next;
    await saveRequests(requests);
    return NextResponse.json({ ok: true, request: next });
  } catch (error) {
    console.error('Error in PATCH /api/requests/[id]:', error);
    return NextResponse.json({ error: error.message }, { status: 500 });
  }
}
