import { NextResponse } from 'next/server';
import { addBusinessDays, loadRequests, saveRequests } from '../../../../lib/data';

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

function normalizeStatusDates(value) {
  if (!value) return {};
  try {
    const parsed = typeof value === 'string' ? JSON.parse(value) : value;
    if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) return {};
    return Object.entries(parsed).reduce((acc, [status, timestamp]) => {
      const iso = dateValueToIso(timestamp);
      if (iso) {
        acc[String(status)] = iso;
      }
      return acc;
    }, {});
  } catch {
    return {};
  }
}

function normalizeStatusLog(value) {
  if (!value) return [];
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

function maybeAddDeadline(status, nowIso, existingDeadline) {
  if (existingDeadline) return existingDeadline;
  if (status === 'sent') return addBusinessDays(nowIso, 10);
  if (status === 'ag_opinion_requested') return addBusinessDays(nowIso, 45);
  return existingDeadline;
}

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
    const currentStatus = String(current.status || 'draft').trim() || 'draft';
    const nextStatus = payload.status == null ? currentStatus : String(payload.status).trim() || 'draft';
    const statusChanged = nextStatus !== currentStatus;
    const statusActor = String(payload.status_user || payload.user || payload.note_author || '').trim() || 'System';

    const currentStatusDates = normalizeStatusDates(current.status_dates);
    const currentStatusLog = normalizeStatusLog(current.status_log);

    const nextStatusDates = { ...currentStatusDates };
    if (!nextStatusDates.draft) {
      nextStatusDates.draft = dateValueToIso(current.last_updated) || nowIso;
    }
    if (statusChanged && !nextStatusDates[nextStatus]) {
      nextStatusDates[nextStatus] = nowIso;
    }

    const nextStatusLog = statusChanged
      ? [
        ...currentStatusLog,
        {
          type: 'status_change',
          from: currentStatus,
          to: nextStatus,
          user: statusActor,
          at: nowIso,
        },
      ]
      : currentStatusLog;

    let nextDateSent = payload.date_sent == null ? current.date_sent : payload.date_sent;
    if (!nextDateSent && nextStatusDates.sent) {
      nextDateSent = nextStatusDates.sent;
    }

    let nextAgNotifiedDate = payload.ag_notified_date == null ? current.ag_notified_date : payload.ag_notified_date;
    if (!nextAgNotifiedDate && nextStatusDates.ag_opinion_requested) {
      nextAgNotifiedDate = nextStatusDates.ag_opinion_requested;
    }

    const nextDeadline10day = payload.deadline_10day == null
      ? maybeAddDeadline(nextStatus, nowIso, current.deadline_10day)
      : payload.deadline_10day;
    const nextDeadlineAg45day = payload.deadline_ag_45day == null
      ? maybeAddDeadline(nextStatus, nowIso, current.deadline_ag_45day)
      : payload.deadline_ag_45day;

    const next = {
      ...current,
      status: nextStatus || 'draft',
      date_sent: nextDateSent,
      ag_notified_date: nextAgNotifiedDate,
      notes: payload.notes == null ? current.notes : String(payload.notes),
      deadline_10day: nextDeadline10day,
      deadline_ag_45day: nextDeadlineAg45day,
      last_updated: nowIso,
      status_dates: nextStatusDates,
      status_log: nextStatusLog,
      status_changed_at: statusChanged ? nowIso : (current.status_changed_at || ''),
      status_changed_by: statusChanged ? statusActor : (current.status_changed_by || ''),
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
