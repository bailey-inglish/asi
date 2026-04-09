import { NextResponse } from 'next/server';
import { addBusinessDays } from '../../../../lib/data';
import { loadCountyRequests, saveCountyRequests } from '../../../../lib/countyData';

function normalizeVerifiedValue(value, fallback = 'no') {
  const status = String(value || '').trim().toLowerCase();
  if (status === 'confirmed') return 'yes';
  if (status === 'incomplete') return 'partial';
  if (status === 'yes' || status === 'partial' || status === 'no') return status;
  return fallback;
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
    const rows = await loadCountyRequests();
    const index = rows.findIndex((row) => String(row.county_id) === String(id));

    if (index === -1) {
      return NextResponse.json({ error: 'County request not found.' }, { status: 404 });
    }

    const current = rows[index];
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

    const nextDateSent = payload.date_sent == null
      ? (current.date_sent || (nextStatusDates.sent || ''))
      : payload.date_sent;
    const nextAgNotifiedDate = payload.ag_notified_date == null
      ? (current.ag_notified_date || (nextStatusDates.ag_opinion_requested || ''))
      : payload.ag_notified_date;
    const nextDeadline10day = payload.deadline_10day == null
      ? maybeAddDeadline(nextStatus, nowIso, current.deadline_10day)
      : payload.deadline_10day;
    const nextDeadlineAg45day = payload.deadline_ag_45day == null
      ? maybeAddDeadline(nextStatus, nowIso, current.deadline_ag_45day)
      : payload.deadline_ag_45day;

    const next = {
      ...current,
      county_name: payload.county_name == null ? String(current.county_name || '') : String(payload.county_name),
      contact_name: payload.contact_name == null ? String(current.contact_name || '') : String(payload.contact_name),
      email: payload.email == null ? String(current.email || '') : String(payload.email),
      portal: payload.portal == null ? String(current.portal || '') : String(payload.portal),
      phone: payload.phone == null ? String(current.phone || '') : String(payload.phone),
      verified: payload.verified == null ? String(current.verified || 'no') : normalizeVerifiedValue(payload.verified, String(current.verified || 'no')),
      status: nextStatus,
      notes: payload.notes == null ? String(current.notes || '') : String(payload.notes),
      date_sent: nextDateSent,
      ag_notified_date: nextAgNotifiedDate,
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

    rows[index] = next;
    await saveCountyRequests(rows);
    return NextResponse.json({ ok: true, county: next });
  } catch (error) {
    console.error('Error in PATCH /api/counties/[id]:', error);
    return NextResponse.json({ error: error?.message || 'Failed to update county request.' }, { status: 500 });
  }
}
