'use client';

import { useDeferredValue, useEffect, useMemo, useState, useTransition } from 'react';
import { STATUS_KEYS, STATUS_META, SUBJECT_LINES, TEMPLATE_LABELS, SUGGESTED_TEMPLATE, TEXAS_COUNTIES } from './constants';
import ActionBar from './tracker/ActionBar';
import EditRecordForm from './tracker/EditRecordForm';
import MobilePortraitShell from './tracker/MobilePortraitShell';
import NotesTimeline from './tracker/NotesTimeline';
import RecordHeader from './tracker/RecordHeader';
import TemplateStudio from './tracker/TemplateStudio';

function statusStyle(status) {
  return STATUS_META[status] || STATUS_META.draft;
}

function verificationIcon(verified) {
  const status = String(verified || 'no').toLowerCase();
  if (status === 'yes') return '';
  return '?';
}

function getVerifiedDisplayLabel(verified) {
  return String(verified || '').toLowerCase() === 'yes' ? 'Confirmed' : 'Incomplete';
}

function getVerifiedSelectValue(verified) {
  return String(verified || '').toLowerCase() === 'yes' ? 'confirmed' : 'incomplete';
}

function getStatusUpdatedAt(record) {
  const candidates = [record?.status_changed_at, record?.last_updated, record?.date_sent, record?.created_at, record?.updated_at];
  for (const candidate of candidates) {
    const parsed = coerceDate(candidate);
    if (parsed) return parsed.getTime();
  }
  return 0;
}

function isTerminalStatus(status) {
  return ['complete', 'partially_complete', 'denied', 'closed'].includes(String(status || '').trim());
}

function getRowKey(record) {
  const institution = String(record?.institution || 'institution').trim();
  const city = String(record?.city || 'city').trim();
  return String(record?.request_id || record?.id || `${institution}::${city}`);
}

function getApiRecordId(record, fallback = '') {
  const institution = String(record?.institution || '').trim();
  const city = String(record?.city || '').trim();
  return String(record?.request_id || record?.id || `${institution}::${city}` || fallback);
}

function applyTemplate(template, variables) {
  return Object.entries(variables).reduce(
    (text, [key, value]) => text.replaceAll(`{{${key}}}`, String(value ?? '')),
    template,
  );
}

function formatHumanDate(value) {
  if (!value) return '';
  const parsed = new Date(value);
  if (Number.isNaN(parsed.getTime())) return '';
  return new Intl.DateTimeFormat('en-US', {
    month: 'long',
    day: 'numeric',
    year: 'numeric',
  }).format(parsed);
}

function parseStatusDates(value) {
  if (!value) return {};
  try {
    const parsed = typeof value === 'string' ? JSON.parse(value) : value;
    if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) return {};
    return Object.entries(parsed).reduce((acc, [status, timestamp]) => {
      const parsedTimestamp = new Date(timestamp);
      if (!Number.isNaN(parsedTimestamp.getTime())) {
        acc[String(status)] = parsedTimestamp.toISOString();
      }
      return acc;
    }, {});
  } catch {
    return {};
  }
}

function parseStatusLog(value) {
  if (!value) return [];
  try {
    const parsed = typeof value === 'string' ? JSON.parse(value) : value;
    if (!Array.isArray(parsed)) return [];
    return parsed
      .map((entry) => {
        if (!entry || typeof entry !== 'object') return null;
        const toStatus = String(entry.to || '').trim();
        if (!toStatus) return null;

        const atDate = new Date(entry.at || '');
        const at = Number.isNaN(atDate.getTime()) ? new Date().toISOString() : atDate.toISOString();
        return {
          type: String(entry.type || 'status_change'),
          from: entry.from == null ? null : String(entry.from),
          to: toStatus,
          user: String(entry.user || 'System'),
          at,
        };
      })
      .filter(Boolean);
  } catch {
    return [];
  }
}

function formatStatusTransition(from, to) {
  const previous = String(from || '').trim();
  const next = String(to || '').trim();
  if ((!previous || previous === 'draft') && next === 'draft') {
    return 'Record created';
  }

  const fromLabel = STATUS_META[previous]?.label || previous || 'Unknown';
  const toLabel = STATUS_META[next]?.label || next || 'Unknown';
  return `${fromLabel} -> ${toLabel}`;
}

function coerceDate(value) {
  if (!value) return null;
  const parsed = new Date(value);
  if (!Number.isNaN(parsed.getTime())) return parsed;

  const raw = String(value).trim();
  if (/^\d{4}-\d{2}-\d{2}$/.test(raw)) {
    const fallback = new Date(`${raw}T00:00:00.000Z`);
    return Number.isNaN(fallback.getTime()) ? null : fallback;
  }
  return null;
}

function addBusinessDaysSimple(startValue, count) {
  let cursor = coerceDate(startValue) || new Date();
  let added = 0;

  while (added < count) {
    cursor = new Date(cursor.getTime() + 24 * 60 * 60 * 1000);
    const weekday = cursor.getUTCDay();
    if (weekday !== 0 && weekday !== 6) {
      added += 1;
    }
  }

  return cursor.toISOString();
}

function businessDaysBetween(startValue, endValue) {
  const startDate = coerceDate(startValue);
  const endDate = coerceDate(endValue);
  if (!startDate || !endDate || endDate < startDate) return 0;

  const start = new Date(Date.UTC(startDate.getUTCFullYear(), startDate.getUTCMonth(), startDate.getUTCDate()));
  const end = new Date(Date.UTC(endDate.getUTCFullYear(), endDate.getUTCMonth(), endDate.getUTCDate()));

  let cursor = start;
  let days = 0;
  while (cursor < end) {
    cursor = new Date(cursor.getTime() + 24 * 60 * 60 * 1000);
    const weekday = cursor.getUTCDay();
    if (weekday !== 0 && weekday !== 6) {
      days += 1;
    }
  }
  return days;
}

function buildVariables(record, sender) {
  const today = new Date().toISOString().slice(0, 10);
  const statusDates = parseStatusDates(record?.status_dates);
  const dateSent = statusDates.sent || record?.date_sent || today;
  const deadlineDate = record?.deadline_10day || addBusinessDaysSimple(dateSent, 10);
  const feeRequestedDate = statusDates.fee_pending || '';
  const feePaidDate = statusDates.fee_paid || '';
  const agRequestedDate = statusDates.ag_opinion_requested || record?.ag_notified_date || '';

  const feeAmountRaw = String(record?.fee_amount || '0').trim();
  const parsedFee = Number.parseFloat(feeAmountRaw);
  const feeAmount = Number.isFinite(parsedFee)
    ? new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD' }).format(parsedFee)
    : '$0.00';

  return {
    INSTITUTION: record?.institution || '',
    CITY: record?.city || '',
    SENDER_NAME: sender?.name || '[YOUR NAME]',
    SENDER_TITLE: sender?.title || '[YOUR TITLE]',
    SENDER_ORG: sender?.org || '[YOUR ORGANIZATION]',
    SENDER_EMAIL: sender?.email || '[YOUR EMAIL]',
    SENDER_PHONE: sender?.phone || '[YOUR PHONE]',
    SENDER_ADDRESS: sender?.address || '[YOUR ADDRESS]',
    TODAY: today,
    TODAY_HUMAN: formatHumanDate(today),
    DATE_SENT: dateSent,
    DATE_SENT_HUMAN: formatHumanDate(dateSent),
    DEADLINE_DATE: deadlineDate,
    DEADLINE_DATE_HUMAN: formatHumanDate(deadlineDate),
    BUSINESS_DAYS_ELAPSED: String(businessDaysBetween(dateSent, today)),
    FEE_REQUESTED_DATE: feeRequestedDate,
    FEE_REQUESTED_DATE_HUMAN: feeRequestedDate ? formatHumanDate(feeRequestedDate) : '',
    FEE_PAID_DATE: feePaidDate,
    FEE_PAID_DATE_HUMAN: feePaidDate ? formatHumanDate(feePaidDate) : '',
    AG_REQUESTED_DATE: agRequestedDate,
    AG_REQUESTED_DATE_HUMAN: agRequestedDate ? formatHumanDate(agRequestedDate) : '',
    STATUS_CHANGED_AT: record?.status_changed_at || '',
    STATUS_CHANGED_AT_HUMAN: record?.status_changed_at ? formatHumanDate(record.status_changed_at) : '',
    STATUS_CHANGED_BY: record?.status_changed_by || '',
    AG_LETTER_NUMBER: '[AG LETTER NO. - check AG notice]',
    FEE_AMOUNT: feeAmount,
    FEE_AMOUNT_RAW: feeAmountRaw || '0',
    PAYMENT_METHOD: '[CHECK / CREDIT CARD / ONLINE PORTAL]',
    DENIAL_BASIS: '[CITED EXCEPTION - from denial letter]',
  };
}

function inferContactType(email, portal) {
  const hasEmail = String(email || '').trim().length > 0;
  const hasPortal = String(portal || '').trim().length > 0;
  if (hasEmail && hasPortal) return 'both';
  if (hasPortal) return 'portal';
  return 'email';
}

function getRecordDraft(record) {
  return {
    institution: record?.institution || '',
    type: record?.type || '4yr',
    city: record?.city || '',
    county: record?.county || '',
    system_district: record?.system_district || '',
    verified: record?.verified || 'partial',
    public_records_email: record?.public_records_email || '',
    public_records_portal: record?.public_records_portal || '',
    fee_amount: String(record?.fee_amount || '0'),
  };
}

function getSenderLabel(sender) {
  return String(sender?.name || sender?.org || sender?.email || 'Sender').trim() || 'Sender';
}

const PROFILE_STORAGE_KEY = 'tpia_sender_profile';

const SENDER_PROFILES = {
  bailey: {
    label: 'Bailey',
    sender: null,
  },
  eugenia: {
    label: 'Eugenia Quintanilla',
    sender: {
      name: 'Eugenia Quintanilla',
      title: 'Postdoctoral Research Fellow',
      email: 'eugenia.quintanilla@austin.utexas.edu',
      phone: '',
    },
  },
};

function formatTimeAgo(value) {
  if (!value) return 'N/A';

  const parsed = new Date(value);
  if (Number.isNaN(parsed.getTime())) return 'N/A';

  const now = Date.now();
  const diffMs = now - parsed.getTime();
  if (diffMs < 0) return 'just now';

  const seconds = Math.floor(diffMs / 1000);
  if (seconds < 5) return 'just now';
  if (seconds < 60) return `${seconds} second${seconds === 1 ? '' : 's'} ago`;

  const minutes = Math.floor(seconds / 60);
  if (minutes < 60) return `${minutes} minute${minutes === 1 ? '' : 's'} ago`;

  const hours = Math.floor(minutes / 60);
  if (hours < 24) return `${hours} hour${hours === 1 ? '' : 's'} ago`;

  const days = Math.floor(hours / 24);
  return `${days} day${days === 1 ? '' : 's'} ago`;
}

function parseNoteEntry(noteStr) {
  if (!noteStr) {
    return { date: '', author: '', body: '', at: '' };
  }
  
  const text = String(noteStr).trim();
  if (!text) {
    return { date: '', author: '', body: '', at: '' };
  }

  // Support multiline note bodies while still extracting the leading [DATE].
  const match = text.match(/^\[([^\]]+)\]\s*([\s\S]*)$/);
  if (!match) {
    // No date bracket found; return as body with no date
    return { date: '', author: '', body: text, at: '' };
  }

  const [, date, remainder] = match;
  if (!remainder.trim()) {
    // Just a date, no content after it
    return { date, author: '', body: '', at: '' };
  }

  // Try to extract author: AUTHOR: BODY
  const authorMatch = remainder.match(/^([^:\n]+):\s*([\s\S]*)$/);
  if (authorMatch) {
    const author = authorMatch[1].trim();
    const body = authorMatch[2].trim();
    return { date: date.trim(), author, body, at: coerceDate(date)?.toISOString() || '' };
  }

  // No author colon found; entire remainder is body
  return { date: date.trim(), author: '', body: remainder.trim(), at: coerceDate(date)?.toISOString() || '' };
}

function getOverdueDeadline(record) {
  const status = String(record?.status || 'draft').trim();
  const statusDates = parseStatusDates(record?.status_dates);

  if (['ag_opinion_requested', 'ag_opinion_pending'].includes(status)) {
    return record?.deadline_ag_45day || (statusDates.ag_opinion_requested ? addBusinessDaysSimple(statusDates.ag_opinion_requested, 45) : '');
  }

  if (['sent', 'acknowledged', 'in_progress', 'fee_pending', 'fee_paid'].includes(status)) {
    return record?.deadline_10day || (statusDates.sent ? addBusinessDaysSimple(statusDates.sent, 10) : '');
  }

  return '';
}

function isRecordOverdue(record) {
  const deadline = getOverdueDeadline(record);
  const parsedDeadline = coerceDate(deadline);
  if (!parsedDeadline) return false;

  const today = new Date();
  const todayUtc = new Date(Date.UTC(today.getUTCFullYear(), today.getUTCMonth(), today.getUTCDate()));
  const deadlineUtc = new Date(Date.UTC(parsedDeadline.getUTCFullYear(), parsedDeadline.getUTCMonth(), parsedDeadline.getUTCDate()));
  return deadlineUtc < todayUtc;
}

export default function TrackerApp({ preloadedState = null, onOpenScreenMenu = null } = {}) {
  const [state, setState] = useState(null);
  const [selectedId, setSelectedId] = useState('');
  const [search, setSearch] = useState('');
  const [typeFilter, setTypeFilter] = useState([]);
  const [statusFilter, setStatusFilter] = useState([]);
  const [sortOrder, setSortOrder] = useState('alpha');
  const [templateKey, setTemplateKey] = useState('01_initial_request');
  const [templateBody, setTemplateBody] = useState('');
  const [templateBodies, setTemplateBodies] = useState({});
  const [showSenderModal, setShowSenderModal] = useState(false);
  const [showProfilePicker, setShowProfilePicker] = useState(false);
  const [profileReady, setProfileReady] = useState(false);
  const [activeProfileId, setActiveProfileId] = useState('');
  const [showAddModal, setShowAddModal] = useState(false);
  const [showEditModal, setShowEditModal] = useState(false);
  const [showNotesModal, setShowNotesModal] = useState(false);
  const [showTemplateDrawer, setShowTemplateDrawer] = useState(false);
  const [showCommandPalette, setShowCommandPalette] = useState(false);
  const [commandSearch, setCommandSearch] = useState('');
  const [actionsOpen, setActionsOpen] = useState(false);
  const [filtersOpen, setFiltersOpen] = useState(false);
  const [bulkMode, setBulkMode] = useState(false);
  const [bulkSelected, setBulkSelected] = useState([]);
  const [recentIds, setRecentIds] = useState([]);
  const [statusDraft, setStatusDraft] = useState('draft');
  const [recordDraft, setRecordDraft] = useState(getRecordDraft(null));
  const [noteDraft, setNoteDraft] = useState('');
  const [isNarrow, setIsNarrow] = useState(false);
  const [message, setMessage] = useState('');
  const [savingCount, setSavingCount] = useState(0);
  const [, startTransition] = useTransition();
  const deferredSearch = useDeferredValue(search);
  const isSaving = savingCount > 0;

  function beginSaving() {
    setSavingCount((value) => value + 1);
  }

  function endSaving() {
    setSavingCount((value) => Math.max(0, value - 1));
  }

  async function withSaving(action) {
    beginSaving();
    try {
      return await action();
    } finally {
      endSaving();
    }
  }

  function applyLoadedState(payload) {
    setState(payload);
    const nextRecords = payload.records || [];
    setSelectedId((current) => {
      const currentKey = String(current || '');
      const firstKey = String(nextRecords[0] ? getRowKey(nextRecords[0]) : '');
      if (!currentKey) return firstKey;

      const exists = nextRecords.some((record) => getRowKey(record) === currentKey);

      return exists ? currentKey : firstKey;
    });
  }

  async function loadState() {
    const response = await fetch('/api/state', { cache: 'no-store' });
    const payload = await response.json();
    applyLoadedState(payload);
  }

  useEffect(() => {
    if (preloadedState) {
      applyLoadedState(preloadedState);
      return;
    }
    loadState().catch((error) => setMessage(error.message));
  }, [preloadedState]);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    const media = window.matchMedia('(max-width: 1280px)');
    const sync = () => setIsNarrow(media.matches);
    sync();
    media.addEventListener('change', sync);
    return () => media.removeEventListener('change', sync);
  }, []);

  useEffect(() => {
    if (typeof window === 'undefined' || !isSaving) return undefined;

    const handleBeforeUnload = (event) => {
      event.preventDefault();
      event.returnValue = 'Changes are still saving. Leaving now may discard them.';
      return event.returnValue;
    };

    window.addEventListener('beforeunload', handleBeforeUnload);
    return () => window.removeEventListener('beforeunload', handleBeforeUnload);
  }, [isSaving]);

  useEffect(() => {
    if (!message) return undefined;
    const timeoutId = window.setTimeout(() => setMessage(''), 5000);
    return () => window.clearTimeout(timeoutId);
  }, [message]);

  const records = state?.records || [];
  const storedSender = state?.sender || {};
  const sender = useMemo(() => {
    const preset = SENDER_PROFILES[activeProfileId]?.sender;
    if (!preset) return storedSender;

    return {
      ...storedSender,
      ...preset,
      // Keep org/address from the shared stored profile while allowing per-user name/title/email overrides.
      org: String(storedSender?.org || ''),
      address: String(storedSender?.address || ''),
    };
  }, [storedSender, activeProfileId]);
  const selected = records.find((record) => getRowKey(record) === String(selectedId)) || records[0] || null;

  const filteredRecords = useMemo(() => {
    const filtered = records.filter((record) => {
      const matchesType = typeFilter.length === 0 || typeFilter.includes(record.type);
      const matchesStatus = statusFilter.length === 0 || statusFilter.includes(record.status);
      const matchesText = !deferredSearch || String(record.institution).toLowerCase().includes(deferredSearch.toLowerCase());
      return matchesType && matchesStatus && matchesText;
    });

    const collator = new Intl.Collator('en', { sensitivity: 'base' });
    return [...filtered].sort((left, right) => {
      if (sortOrder === 'enrollment') {
        const leftEnrollment = Number(left.enrollment_2025 || 0);
        const rightEnrollment = Number(right.enrollment_2025 || 0);
        if (leftEnrollment !== rightEnrollment) return rightEnrollment - leftEnrollment;
        return collator.compare(String(left.institution || ''), String(right.institution || ''));
      }

      if (sortOrder === 'recent') {
        const leftUpdated = getStatusUpdatedAt(left);
        const rightUpdated = getStatusUpdatedAt(right);
        if (leftUpdated !== rightUpdated) return rightUpdated - leftUpdated;
        return collator.compare(String(left.institution || ''), String(right.institution || ''));
      }

      if (sortOrder === 'staleness') {
        const leftTerminal = isTerminalStatus(left.status);
        const rightTerminal = isTerminalStatus(right.status);
        if (leftTerminal !== rightTerminal) return leftTerminal ? 1 : -1;

        const leftUpdated = getStatusUpdatedAt(left);
        const rightUpdated = getStatusUpdatedAt(right);
        if (leftUpdated !== rightUpdated) return leftUpdated - rightUpdated;
        return collator.compare(String(left.institution || ''), String(right.institution || ''));
      }

      return collator.compare(String(left.institution || ''), String(right.institution || ''));
    });
  }, [deferredSearch, records, sortOrder, statusFilter, typeFilter]);

  useEffect(() => {
    let active = true;

    fetch(`/api/templates/${templateKey}`, { cache: 'no-store' })
      .then(async (response) => {
        if (!response.ok) {
          throw new Error(`Template ${templateKey} could not be loaded`);
        }
        return response.text();
      })
      .then((text) => {
        if (active) {
          setTemplateBody(text);
          setTemplateBodies((current) => ({ ...current, [templateKey]: text }));
        }
      })
      .catch((error) => {
        if (active) {
          setTemplateBody('');
          setMessage(error.message);
        }
      });

    return () => {
      active = false;
    };
  }, [templateKey]);

  async function loadTemplateText(templateName) {
    if (templateBodies[templateName]) {
      return templateBodies[templateName];
    }

    const response = await fetch(`/api/templates/${templateName}`, { cache: 'no-store' });
    if (!response.ok) {
      throw new Error(`Template ${templateName} could not be loaded`);
    }
    const text = await response.text();
    setTemplateBodies((current) => ({ ...current, [templateName]: text }));
    return text;
  }

  useEffect(() => {
    if (selected && !filteredRecords.some((record) => getRowKey(record) === String(selectedId)) && filteredRecords[0]) {
      setSelectedId(getRowKey(filteredRecords[0]));
    }
  }, [search, typeFilter.join(','), statusFilter.join(','), selectedId, selected?.request_id, selected?.institution, selected?.city, filteredRecords]);

  useEffect(() => {
    if (!selected) return;
    setStatusDraft(selected.status || 'draft');
    setRecordDraft(getRecordDraft(selected));
    setShowTemplateDrawer(false);
    setRecentIds((current) => {
      const selectedKey = getRowKey(selected);
      const next = [selectedKey, ...current.filter((id) => id !== selectedKey)];
      return next.slice(0, 8);
    });
  }, [selected?.request_id, selected?.institution, selected?.city]);

  async function saveJson(url, payload, successText) {
    return withSaving(async () => {
      const response = await fetch(url, {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload),
      });
      if (!response.ok) {
        const error = await response.json().catch(() => ({}));
        throw new Error(error.error || 'Save failed');
      }
      if (successText) {
        setMessage(successText);
      }
      await loadState();
    });
  }

  async function patchJson(url, payload, successText) {
    return withSaving(async () => {
      const response = await fetch(url, {
        method: 'PATCH',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload),
      });
      const data = await response.json().catch(() => ({}));
      if (!response.ok) {
        throw new Error(data.error || 'Save failed');
      }
      if (successText) {
        setMessage(successText);
      }
      await loadState();
      return data;
    });
  }

  function mergeRequestIntoState(updatedRequest, requestKey) {
    setState((current) => {
      if (!current?.records) return current;

      const nextRecords = current.records.map((record) => {
        const matchesByRequestId = updatedRequest?.request_id && String(record.request_id || '') === String(updatedRequest.request_id || '');
        const matchesByKey = String(getRowKey(record)) === String(requestKey);
        const matchesByInstitutionCity = String(record.institution || '') === String(updatedRequest?.institution || '')
          && String(record.city || '') === String(updatedRequest?.city || '');

        if (!matchesByRequestId && !matchesByKey && !matchesByInstitutionCity) {
          return record;
        }

        return {
          ...record,
          status: updatedRequest?.status ?? record.status,
          notes: updatedRequest?.notes ?? record.notes,
          deadline_10day: updatedRequest?.deadline_10day ?? record.deadline_10day,
          deadline_ag_45day: updatedRequest?.deadline_ag_45day ?? record.deadline_ag_45day,
          date_sent: updatedRequest?.date_sent ?? record.date_sent,
          ag_notified_date: updatedRequest?.ag_notified_date ?? record.ag_notified_date,
          last_updated: updatedRequest?.last_updated ?? record.last_updated,
          status_log: updatedRequest?.status_log ?? record.status_log,
          status_dates: updatedRequest?.status_dates ?? record.status_dates,
          status_changed_at: updatedRequest?.status_changed_at ?? record.status_changed_at,
          status_changed_by: updatedRequest?.status_changed_by ?? record.status_changed_by,
        };
      });

      return {
        ...current,
        records: nextRecords,
      };
    });
  }

  function mergeCollegeIntoState(updatedCollege, requestKey) {
    setState((current) => {
      if (!current?.records) return current;

      const nextRecords = current.records.map((record) => {
        const matchesByKey = String(getRowKey(record)) === String(requestKey);
        const matchesByInstitutionCity = String(record.institution || '') === String(updatedCollege?.institution || '')
          && String(record.city || '') === String(updatedCollege?.city || '');

        if (!matchesByKey && !matchesByInstitutionCity) {
          return record;
        }

        const nextEmail = updatedCollege?.public_records_email ?? record.public_records_email;
        const nextPortal = updatedCollege?.public_records_portal ?? record.public_records_portal;

        return {
          ...record,
          ...updatedCollege,
          public_records_email: nextEmail,
          public_records_portal: nextPortal,
          county: updatedCollege?.county ?? record.county,
          contact_type: inferContactType(nextEmail, nextPortal),
        };
      });

      return {
        ...current,
        records: nextRecords,
      };
    });
  }

  function applyOptimisticRequestUpdate(requestKey, updater) {
    setState((current) => {
      if (!current?.records) return current;

      return {
        ...current,
        records: current.records.map((record) => {
          if (String(getRowKey(record)) !== String(requestKey)) return record;
          const nextRecord = updater(record);
          return nextRecord;
        }),
      };
    });
  }

  async function patchRequest(requestKey, payload, successText) {
    return withSaving(async () => {
      const response = await fetch(`/api/requests/${encodeURIComponent(String(requestKey))}`, {
        method: 'PATCH',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload),
      });

      const data = await response.json().catch(() => ({}));
      if (!response.ok) {
        throw new Error(data.error || 'Save failed');
      }

      if (successText) {
        setMessage(successText);
      }

      if (data?.request) {
        mergeRequestIntoState(data.request, requestKey);
      } else {
        await loadState();
      }

      return data;
    });
  }

  async function postJson(url, payload, successText) {
    return withSaving(async () => {
      const response = await fetch(url, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload),
      });
      if (!response.ok) {
        const error = await response.json().catch(() => ({}));
        throw new Error(error.error || 'Save failed');
      }
      if (successText) {
        setMessage(successText);
      }
      await loadState();
    });
  }

  async function copyText(text) {
    await navigator.clipboard.writeText(text);
    setMessage('Copied to clipboard');
  }

  function toggleInList(value, setter) {
    setter((current) => (current.includes(value) ? current.filter((entry) => entry !== value) : [...current, value]));
  }

  async function handleSaveRecord(silent = false) {
    if (!selected) return;
    const requestKey = getApiRecordId(selected, selectedId);
    mergeCollegeIntoState(
      {
        ...recordDraft,
        county: recordDraft.county || selected.county || '',
        contact_type: inferContactType(recordDraft.public_records_email, recordDraft.public_records_portal),
      },
      requestKey,
    );
    startTransition(async () => {
      await patchJson(
        `/api/colleges/${encodeURIComponent(requestKey)}`,
        recordDraft,
        silent ? null : `Saved college record for ${selected.institution}`,
      );
    });
  }

  async function handleAddCollege(formData) {
    const payload = Object.fromEntries(formData.entries());
    startTransition(async () => {
      await postJson('/api/colleges', payload, `Added ${payload.institution}`);
      setShowAddModal(false);
    });
  }

  async function handleSenderUpdate(formData) {
    const payload = Object.fromEntries(formData.entries());

    startTransition(() => {
      withSaving(async () => {
        const response = await fetch('/api/sender', {
          method: 'PUT',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify(payload),
        });

        const data = await response.json().catch(() => ({}));
        if (!response.ok) {
          throw new Error(data.error || 'Save failed');
        }

        setState((current) => {
          if (!current) return current;
          return {
            ...current,
            sender: {
              ...(current.sender || {}),
              ...(data.sender || payload),
            },
          };
        });

        setMessage('Saved sender profile');
        setShowSenderModal(false);
      }).catch((error) => setMessage(error.message));
    });
  }

  function getProfileIdFromStorage() {
    if (typeof window === 'undefined') return '';
    const raw = String(window.localStorage.getItem(PROFILE_STORAGE_KEY) || '').trim();
    return Object.prototype.hasOwnProperty.call(SENDER_PROFILES, raw) ? raw : '';
  }

  function handleSelectProfile(profileId) {
    if (typeof window !== 'undefined') {
      window.localStorage.setItem(PROFILE_STORAGE_KEY, profileId);
    }
    setActiveProfileId(profileId);
    setShowProfilePicker(false);
    setProfileReady(true);
    setMessage(`Profile set to ${SENDER_PROFILES[profileId]?.label || profileId}`);
  }

  useEffect(() => {
    if (!state || profileReady) return;

    const profileId = getProfileIdFromStorage();
    if (!profileId) {
      setShowProfilePicker(true);
      setProfileReady(true);
      return;
    }

    setActiveProfileId(profileId);
    setProfileReady(true);
    setShowProfilePicker(false);
  }, [state, profileReady]);

  async function handleStatusChange(nextStatus) {
    if (!selected) return;
    const previousStatus = currentStatus;
    const statusActor = noteAuthor;
    setStatusDraft(nextStatus);

    const requestKey = getApiRecordId(selected, selectedId);
    applyOptimisticRequestUpdate(requestKey, (record) => {
      const nowIso = new Date().toISOString();
      return {
        ...record,
        status: nextStatus,
        last_updated: nowIso,
        status_changed_at: nowIso,
        status_changed_by: statusActor,
        status_dates: {
          ...parseStatusDates(record.status_dates),
          [nextStatus]: parseStatusDates(record.status_dates)[nextStatus] || nowIso,
        },
        status_log: [
          ...parseStatusLog(record.status_log),
          {
            type: 'status_change',
            from: String(record.status || 'draft') || 'draft',
            to: nextStatus,
            user: statusActor,
            at: nowIso,
          },
        ],
      };
    });

    startTransition(() => {
      patchRequest(
        requestKey,
        { status: nextStatus, status_user: statusActor },
        `Saved request status for ${selected.institution}`,
      ).catch((error) => {
        setStatusDraft(previousStatus);
        setMessage(error.message);
      });
    });
  }

  const suggestedTemplateKey = SUGGESTED_TEMPLATE[selected?.status] || '01_initial_request';

  async function handleAddNote() {
    if (!selected) return;
    const note = String(noteDraft || '').trim();
    if (!note) {
      setMessage('Enter a note before saving.');
      return;
    }

    const noteAuthor = getSenderLabel(sender);
    const requestKey = getApiRecordId(selected, selectedId);
    const nowIso = new Date().toISOString();
    const noteEntry = `[${nowIso.slice(0, 10)}] ${noteAuthor ? `${noteAuthor}: ${note}` : note}`;
    applyOptimisticRequestUpdate(requestKey, (record) => ({
      ...record,
      notes: [record.notes, noteEntry].filter(Boolean).join(' | '),
      last_updated: nowIso,
    }));

    startTransition(() => {
      patchRequest(
        requestKey,
        { append_note: note, note_author: noteAuthor },
        `Added note for ${selected.institution}`,
      ).then(() => setNoteDraft('')).catch((error) => setMessage(error.message));
    });
  }
  const templateName = templateKey || suggestedTemplateKey;
  const subject = (SUBJECT_LINES[templateName] || `TPIA Request – ${selected?.institution || ''}`).replace('{institution}', selected?.institution || '');
  const templateText = applyTemplate(templateBody, buildVariables(selected, sender));
  const fullEmailText = `${subject}\n\n${templateText}`;
  const selectedEmail = String(recordDraft.public_records_email || selected?.public_records_email || '').trim();
  const selectedPortal = String(recordDraft.public_records_portal || selected?.public_records_portal || '').trim();
  const activeContactType = inferContactType(selectedEmail, selectedPortal);
  const emailPrimary = activeContactType === 'email';
  const showEmailAction = activeContactType === 'email' || activeContactType === 'both';
  const showPortalAction = activeContactType === 'portal' || activeContactType === 'both';
  const noteAuthor = getSenderLabel(sender);
  const noteEntries = useMemo(
    () => String(selected?.notes || '')
      .split(' | ')
      .map((noteText) => noteText.trim())
      .filter(Boolean)
      .map(parseNoteEntry)
      .map((entry, index) => ({
        kind: 'note',
        date: entry.date,
        author: entry.author,
        body: entry.body,
        at: entry.at,
        sortOrder: index,
      })),
    [selected?.notes],
  );
  const statusEntries = useMemo(
    () => parseStatusLog(selected?.status_log).map((entry, index) => ({
      kind: 'status',
      date: formatHumanDate(entry.at) || 'Status change',
      author: entry.user,
      body: formatStatusTransition(entry.from, entry.to),
      at: entry.at,
      sortOrder: index,
    })),
    [selected?.status_log],
  );
  const timelineEntries = useMemo(() => {
    return [...statusEntries, ...noteEntries].sort((a, b) => {
      const aTime = coerceDate(a.at)?.getTime() || 0;
      const bTime = coerceDate(b.at)?.getTime() || 0;
      if (aTime === bTime) {
        return a.sortOrder - b.sortOrder;
      }
      return aTime - bTime;
    });
  }, [noteEntries, statusEntries]);
  const currentStatus = statusDraft || selected?.status || 'draft';

  function closeTemplateStudio() {
    setShowTemplateDrawer(false);
  }

  const recordDirty = !!selected && (
    recordDraft.institution !== (selected.institution || '') ||
    recordDraft.type !== (selected.type || '4yr') ||
    recordDraft.city !== (selected.city || '') ||
    recordDraft.county !== (selected.county || '') ||
    recordDraft.system_district !== (selected.system_district || '') ||
    recordDraft.verified !== (selected.verified || 'partial') ||
    recordDraft.public_records_email !== (selected.public_records_email || '') ||
    recordDraft.public_records_portal !== (selected.public_records_portal || '') ||
    String(recordDraft.fee_amount || '0') !== String(selected.fee_amount || '0')
  );

  useEffect(() => {
    if (!selected) return;
    setTemplateKey(SUGGESTED_TEMPLATE[selected.status] || '01_initial_request');
  }, [selected?.request_id, selected?.status]);

  useEffect(() => {
    const onKeyDown = (event) => {
      const targetTag = event.target?.tagName || '';
      const isTyping = ['INPUT', 'TEXTAREA', 'SELECT'].includes(targetTag);

      if ((event.ctrlKey || event.metaKey) && event.key.toLowerCase() === 'k') {
        event.preventDefault();
        setShowCommandPalette(true);
        return;
      }

      if (event.key === 'Escape') {
        setShowSenderModal(false);
        setShowAddModal(false);
        setShowTemplateDrawer(false);
        setShowCommandPalette(false);
        setActionsOpen(false);
        return;
      }

      if (isTyping) return;

      if (event.key.toLowerCase() === 't') {
        event.preventDefault();
        copyText(fullEmailText).catch((error) => setMessage(error.message));
      }
      if (event.key.toLowerCase() === 'e') {
        event.preventDefault();
        setShowEditModal(true);
        setShowNotesModal(false);
        setShowTemplateDrawer(false);
      }
    };

    window.addEventListener('keydown', onKeyDown);
    return () => window.removeEventListener('keydown', onKeyDown);
  }, [fullEmailText, recordDraft, selected?.request_id]);

  const commandResults = useMemo(() => {
    const q = commandSearch.trim().toLowerCase();
    if (!q) return records.slice(0, 12);
    return records.filter((record) => record.institution.toLowerCase().includes(q)).slice(0, 12);
  }, [commandSearch, records]);

  const recentRecords = recentIds
    .map((id) => records.find((record) => getRowKey(record) === String(id)))
    .filter(Boolean)
    .slice(0, 5);

  async function handleBulkCopy() {
    const chosen = records.filter((record) => bulkSelected.includes(getRowKey(record)));
    const combined = chosen.map((record) => {
      const localSubject = (SUBJECT_LINES[templateName] || `TPIA Request – ${record.institution}`).replace('{institution}', record.institution);
      const localBody = applyTemplate(templateBody, buildVariables(record, sender));
      return `${record.institution}\n${localSubject}\n\n${localBody}`;
    }).join('\n\n==============================\n\n');

    if (!combined) {
      setMessage('No institutions selected for bulk copy');
      return;
    }
    await copyText(combined);
  }

  async function handleSendEmail() {
    if (!selectedEmail) {
      setMessage('No public records email is available for this institution.');
      return;
    }

    const bestTemplateKey = SUGGESTED_TEMPLATE[selected?.status] || '01_initial_request';
    const bestTemplateBody = await loadTemplateText(bestTemplateKey);
    const bestTemplateSubject = (SUBJECT_LINES[bestTemplateKey] || `TPIA Request – ${selected?.institution || ''}`)
      .replace('{institution}', selected?.institution || '');
    const bestTemplateText = applyTemplate(bestTemplateBody, buildVariables(selected, sender));

    const mailto = `mailto:${encodeURIComponent(selectedEmail)}?subject=${encodeURIComponent(bestTemplateSubject)}&body=${encodeURIComponent(bestTemplateText)}`;
    window.location.href = mailto;
  }

  async function handleQuickCopyTemplate() {
    if (!selected) return;
    const bestTemplateKey = SUGGESTED_TEMPLATE[selected?.status] || '01_initial_request';
    const bestTemplateBody = await loadTemplateText(bestTemplateKey);
    const bestTemplateSubject = (SUBJECT_LINES[bestTemplateKey] || `TPIA Request – ${selected?.institution || ''}`)
      .replace('{institution}', selected?.institution || '');
    const bestTemplateText = applyTemplate(bestTemplateBody, buildVariables(selected, sender));
    const fullText = `${bestTemplateSubject}\n\n${bestTemplateText}`;
    await copyText(fullText);
  }

  if (!state || !selected) {
    return (
      <div className="workspace">
        <div className="hero-card panel skeleton-shell desktop-skeleton-shell">
          <div className="hero-copy">
            <div className="skeleton-line skeleton-title" />
            <div className="skeleton-line skeleton-subtitle" />
            <div className="skeleton-grid" style={{ marginTop: 14 }}>
              <div className="skeleton-card" />
              <div className="skeleton-card" />
              <div className="skeleton-card" />
              <div className="skeleton-card" />
            </div>
          </div>
        </div>

        <div className="hero-card panel skeleton-shell mobile-skeleton-shell">
          <div className="mobile-skeleton-toolbar">
            <div className="skeleton-line mobile-skeleton-circle" />
            <div className="skeleton-line mobile-skeleton-circle" />
          </div>
          <div className="mobile-skeleton-card">
            <div className="skeleton-line skeleton-title" />
            <div className="skeleton-line skeleton-subtitle" />
            <div className="mobile-skeleton-grid">
              <div className="skeleton-card" />
              <div className="skeleton-card" />
            </div>
          </div>
        </div>
      </div>
    );
  }

  const stats = {
    total: records.length,
    active: records.filter((record) => ['sent', 'acknowledged', 'in_progress', 'fee_pending', 'fee_paid', 'ag_opinion_requested', 'ag_opinion_pending'].includes(record.status)).length,
    complete: records.filter((record) => ['complete', 'partially_complete'].includes(record.status)).length,
    overdue: records.filter((record) => isRecordOverdue(record)).length,
  };

  return (
    <>
      <MobilePortraitShell
        selected={selected}
        selectedId={selectedId}
        records={records}
        stats={stats}
        isSaving={isSaving}
        selectedEmail={selectedEmail}
        selectedPortal={selectedPortal}
        showEmailAction={showEmailAction}
        showPortalAction={showPortalAction}
        currentStatus={currentStatus}
        statusMetaMap={STATUS_META}
        statusKeys={STATUS_KEYS}
        selectedStatusMeta={statusStyle(selected.status)}
        lastUpdated={formatTimeAgo(selected.last_updated)}
        recentRecords={recentRecords}
        noteAuthor={noteAuthor}
        noteEntries={noteEntries}
        timelineEntries={timelineEntries}
        recordDraft={recordDraft}
        recordDirty={recordDirty}
        selectedCounty={selected.county}
        texasCounties={TEXAS_COUNTIES}
        onFieldChange={(field, value) => setRecordDraft((draft) => ({ ...draft, [field]: value }))}
        onSaveRecord={() => handleSaveRecord().catch((error) => setMessage(error.message))}
        noteDraft={noteDraft}
        onChangeNote={setNoteDraft}
        onAddNote={() => handleAddNote().catch((error) => setMessage(error.message))}
        onSelectRecord={(record) => setSelectedId(getRowKey(record))}
        onCopyEmail={() => copyText(selectedEmail).catch((error) => setMessage(error.message))}
        onCopyTemplate={() => handleQuickCopyTemplate().catch((error) => setMessage(error.message))}
        onStatusChange={(nextStatus) => handleStatusChange(nextStatus).catch((error) => setMessage(error.message))}
        templateName={templateName}
        templateLabels={TEMPLATE_LABELS}
        templateKey={templateKey}
        templateText={templateText}
        onTemplateKeyChange={setTemplateKey}
        subject={subject}
        onOpenSenderModal={() => setShowSenderModal(true)}
        onOpenAddModal={() => setShowAddModal(true)}
        onOpenScreenMenu={onOpenScreenMenu}
        onOpenCommandPalette={() => setShowCommandPalette(true)}
        bulkMode={bulkMode}
        bulkSelectedCount={bulkSelected.length}
        onToggleBulkMode={() => setBulkMode((value) => !value)}
        onClearBulkSelected={() => setBulkSelected([])}
        onBulkCopy={() => handleBulkCopy().catch((error) => setMessage(error.message))}
      />

      <div className="workspace workspace-fixed desktop-layout">
      <section className="hero-card panel compact-hero">
        <div className="compact-hero-grid">
          <div>
            <h1 className="page-title">Student Directories</h1>
            <div className="saving-status" aria-live="polite" aria-atomic="true">
              {isSaving ? (
                <span className="saving-pill">
                  <span className="saving-spinner" aria-hidden="true" />
                  Saving...
                </span>
              ) : (
                <span className="saving-pill saving-pill-idle">All changes saved</span>
              )}
            </div>
          </div>
          <div className="stat-grid compact-stats">
            <div className="stat-card"><div className="stat-label">Institutions</div><div className="stat-value">{stats.total}</div></div>
            <div className="stat-card"><div className="stat-label">Active</div><div className="stat-value">{stats.active}</div></div>
            <div className="stat-card"><div className="stat-label">Complete</div><div className="stat-value">{stats.complete}</div></div>
            <div className="stat-card"><div className="stat-label">Overdue</div><div className="stat-value">{stats.overdue}</div></div>
          </div>
        </div>
      </section>

      <div className="workspace-gap" />

      <div className={`workflow-grid ${isNarrow ? 'two-col' : ''}`}>
        <section className="main-card list-panel">
          <div className="record-header" style={{ marginBottom: 10 }}>
            <div>
              <h2 className="section-title" style={{ marginBottom: 4 }}>Institutions</h2>
              <p className="subtle">{filteredRecords.length} of {records.length} shown</p>
            </div>
            <div className="record-actions">
              <div className="dropdown-wrap">
                <button className="button-secondary" type="button" onClick={() => setActionsOpen((open) => !open)}>Actions</button>
                {actionsOpen ? (
                  <div className="dropdown-menu">
                    <button className="dropdown-item" type="button" onClick={() => { setShowSenderModal(true); setActionsOpen(false); }}>Edit sender profile</button>
                    <button className="dropdown-item" type="button" onClick={() => { setShowAddModal(true); setActionsOpen(false); }}>Add institution</button>
                    <button className="dropdown-item" type="button" onClick={() => { setBulkMode((value) => !value); setActionsOpen(false); }}>Toggle bulk mode</button>
                  </div>
                ) : null}
              </div>
              <button className="button-secondary" type="button" onClick={() => setFiltersOpen((value) => !value)}>
                {filtersOpen ? 'Hide filters' : 'Show filters'}
              </button>
            </div>
          </div>

          <div className="list-controls">
            {bulkMode ? <span className="badge">Bulk mode on</span> : null}
          </div>

          {filtersOpen ? (
            <div className="search-stack">
              <div className="search-row">
                <label className="label" htmlFor="search">Search institutions</label>
                <input id="search" className="field" value={search} onChange={(event) => setSearch(event.target.value)} placeholder="UTA, Blinn, etc." />
              </div>
              <div className="search-row">
                <div className="label">Institution type</div>
                <div className="inline-meta">
                  {['4yr', '2yr'].map((type) => (
                    <button key={type} className={typeFilter.includes(type) ? 'chip-active' : 'chip'} onClick={() => toggleInList(type, setTypeFilter)}>
                      {type}
                    </button>
                  ))}
                </div>
              </div>
              <div className="search-row">
                <div className="label">Statuses</div>
                <div className="inline-meta">
                  {STATUS_KEYS.slice(0, 8).map((status) => (
                    <button key={status} className={statusFilter.includes(status) ? 'chip-active' : 'chip'} onClick={() => toggleInList(status, setStatusFilter)}>
                      {STATUS_META[status].label}
                    </button>
                  ))}
                </div>
              </div>
              <div className="search-row">
                <label className="label" htmlFor="sort-order">Sort by</label>
                <select id="sort-order" className="select" value={sortOrder} onChange={(event) => setSortOrder(event.target.value)}>
                  <option value="alpha">A-Z</option>
                  <option value="enrollment">Enrollment</option>
                  <option value="recent">Most recently changed</option>
                  <option value="staleness">Longest since status update</option>
                </select>
              </div>
            </div>
          ) : null}

          {recentRecords.length ? (
            <div className="recent-strip">
              <div className="label" style={{ marginBottom: 6 }}>Recent</div>
              <div className="inline-meta">
                {recentRecords.map((record) => (
                  <button key={getRowKey(record)} className="chip" type="button" onClick={() => setSelectedId(getRowKey(record))}>
                    {record.institution}
                  </button>
                ))}
              </div>
            </div>
          ) : null}

          {bulkMode ? (
            <div className="bulk-bar">
              <span>{bulkSelected.length} selected</span>
              <div className="record-actions">
                <button className="button-secondary" type="button" onClick={() => setBulkSelected([])}>Clear</button>
                <button className="button" type="button" onClick={() => handleBulkCopy().catch((error) => setMessage(error.message))}>Copy template for selected</button>
              </div>
            </div>
          ) : null}

          <div className="record-list">
            {filteredRecords.map((record) => {
              const meta = statusStyle(record.status);
              const rowKey = getRowKey(record);
              const selectedRow = String(rowKey) === String(selectedId);
              const checked = bulkSelected.includes(String(rowKey));
              const overdue = isRecordOverdue(record);
              return (
                <button
                  key={rowKey}
                  type="button"
                  className={`record-row ${selectedRow ? 'active' : ''}`}
                  style={{ borderLeftColor: meta.color }}
                  onClick={() => setSelectedId(rowKey)}
                >
                  <div className="record-top">
                    <div>
                      <p className="record-name" title={record.institution}>
                        {record.institution}{' '}
                        {overdue ? (
                          <span className="record-status-icon overdue" title="Overdue request" aria-label="Overdue request">
                            <span className="clock-icon" aria-hidden="true" />
                          </span>
                        ) : (
                          verificationIcon(record.verified) ? (
                            <span
                              className="record-status-icon verification"
                              style={{ color: '#ea580c' }}
                              title={`Verified: ${getVerifiedDisplayLabel(record.verified)}`}
                              aria-label={`Verified ${getVerifiedDisplayLabel(record.verified)}`}
                            >
                              {verificationIcon(record.verified)}
                            </span>
                          ) : null
                        )}
                      </p>
                      <p className="meta meta-wrap">
                        <span className="record-meta-type">{record.type}</span> · <span className="record-meta-city">{record.city}</span> · <span className="record-meta-enrollment">{record.enrollment_2025 ? `${Number(record.enrollment_2025).toLocaleString()} enrolled` : 'Enrollment n/a'}</span> · <span className="record-meta-email">{record.public_records_email || 'No email on file'}</span>
                      </p>
                    </div>
                    <span className="badge" style={{ color: meta.color, background: meta.bg }}>{meta.label}</span>
                  </div>
                  {bulkMode ? (
                    <div className="record-meta">
                      <label className="bulk-check">
                        <input
                          type="checkbox"
                          checked={checked}
                          onChange={(event) => {
                            event.stopPropagation();
                            setBulkSelected((current) => (
                              checked ? current.filter((id) => id !== String(rowKey)) : [...current, String(rowKey)]
                            ));
                          }}
                        />
                        Include in bulk copy
                      </label>
                    </div>
                  ) : null}
                </button>
              );
            })}
          </div>
        </section>

        <section className="main-card workflow-panel">
          <RecordHeader
            selected={selected}
            statusMeta={statusStyle(selected.status)}
            lastUpdated={formatTimeAgo(selected.last_updated)}
            showEditButton
            onEditInstitution={() => { setShowEditModal(true); setShowNotesModal(false); setShowTemplateDrawer(false); }}
          />

          <ActionBar
            selectedPortal={showPortalAction && selectedPortal ? selectedPortal : ''}
            selectedEmail={showEmailAction && selectedEmail ? selectedEmail : ''}
            emailPrimary={emailPrimary}
            onSendEmail={() => handleSendEmail().catch((error) => setMessage(error.message))}
            onCopyTemplate={() => handleQuickCopyTemplate().catch((error) => setMessage(error.message))}
            onOpenNotes={() => { setShowNotesModal(true); setShowEditModal(false); setShowTemplateDrawer(false); }}
            onOpenTemplate={() => setShowTemplateDrawer(true)}
          />

          <div className="kv-grid" style={{ marginTop: 14 }}>
            <div className="kv"><strong>Public records email</strong>{selectedEmail ? <a className="kv-value kv-link" href={`mailto:${selectedEmail}`}>{selectedEmail}</a> : <span className="kv-value">None</span>}</div>
            <div className="kv"><strong>Portal</strong>{selectedPortal ? <a className="kv-value kv-url kv-link" href={selectedPortal} target="_blank" rel="noreferrer">{selectedPortal}</a> : <span className="kv-value">None</span>}</div>
          </div>

          <div className="kv-grid" style={{ marginTop: 10 }}>
            <div className="kv">
              <strong>Status</strong>
              <select
                name="status"
                className="select"
                value={currentStatus}
                onChange={(event) => handleStatusChange(event.target.value).catch((error) => setMessage(error.message))}
              >
                {STATUS_KEYS.map((status) => <option key={status} value={status}>{STATUS_META[status].label}</option>)}
              </select>
            </div>
            {selected.enrollment_2025 ? <div className="kv"><strong>2025 Enrollment</strong><span className="kv-value">{Number(selected.enrollment_2025).toLocaleString()}</span></div> : <div className="kv"><strong>2025 Enrollment</strong><span className="kv-value">N/A</span></div>}
          </div>
        </section>
      </div>
      </div>

      {showEditModal ? (
        <div className="modal-overlay" role="dialog" aria-modal="true" aria-label="Edit Institution">
          <div className="modal-card">
            <EditRecordForm
              recordDraft={recordDraft}
              currentStatus={currentStatus}
              selectedCounty={selected.county}
              texasCounties={TEXAS_COUNTIES}
              onFieldChange={(field, value) => setRecordDraft((draft) => ({ ...draft, [field]: value }))}
              onSave={() => handleSaveRecord().then(() => setShowEditModal(false)).catch((error) => setMessage(error.message))}
              recordDirty={recordDirty}
              onClose={() => setShowEditModal(false)}
            />
          </div>
        </div>
      ) : null}

      {showNotesModal ? (
        <div className="modal-overlay" role="dialog" aria-modal="true" aria-label="Notes">
          <div className="modal-card">
            <NotesTimeline
              noteDraft={noteDraft}
              noteAuthor={noteAuthor}
              noteEntries={noteEntries}
              timelineEntries={timelineEntries}
              onChangeNote={setNoteDraft}
              onAddNote={() => handleAddNote().catch((error) => setMessage(error.message))}
              onClose={() => setShowNotesModal(false)}
            />
          </div>
        </div>
      ) : null}

      {showTemplateDrawer ? (
        <div className="modal-overlay" role="dialog" aria-modal="true" aria-label="Template drawer">
          <div className="modal-card drawer-card">
            <TemplateStudio
              templateName={templateName}
              templateLabels={TEMPLATE_LABELS}
              subject={subject}
              templateKey={templateKey}
              templateText={templateText}
              onTemplateKeyChange={setTemplateKey}
              onCopyTemplate={() => copyText(fullEmailText).catch((error) => setMessage(error.message))}
              onCopySubject={() => copyText(subject).catch((error) => setMessage(error.message))}
              onCopyBody={() => copyText(templateText).catch((error) => setMessage(error.message))}
              onClose={closeTemplateStudio}
            />
          </div>
        </div>
      ) : null}

      {showProfilePicker ? (
        <div className="modal-overlay" role="dialog" aria-modal="true" aria-label="Choose profile">
          <div className="modal-card profile-picker-card">
            <div className="record-header">
              <h2 className="section-title" style={{ marginBottom: 0 }}>Select your profile</h2>
            </div>
            <p className="meta" style={{ marginTop: 0 }}>
              This sets sender information for templates and status activity.
            </p>
            <div className="profile-picker-grid">
              <button className="button-secondary profile-picker-button" type="button" onClick={() => handleSelectProfile('bailey')}>
                <strong>Bailey</strong>
                <span>Research Intern</span>
                <span>{String(storedSender?.email || 'No default email set')}</span>
              </button>
              <button className="button-secondary profile-picker-button" type="button" onClick={() => handleSelectProfile('eugenia')}>
                <strong>Eugenia Quintanilla</strong>
                <span>Postdoctoral Research Fellow</span>
                <span>eugenia.quintanilla@austin.utexas.edu</span>
              </button>
            </div>
          </div>
        </div>
      ) : null}

      {showSenderModal ? (
        <div className="modal-overlay" role="dialog" aria-modal="true" aria-label="Edit sender profile">
          <div className="modal-card">
            <div className="record-header">
              <h2 className="section-title" style={{ marginBottom: 0 }}>Sender profile</h2>
              <div className="record-actions sender-modal-actions">
                <button
                  className="button-secondary"
                  type="button"
                  onClick={() => {
                    setShowSenderModal(false);
                    setShowProfilePicker(true);
                  }}
                >
                  Switch profile
                </button>
                <button className="button-secondary" type="button" onClick={() => setShowSenderModal(false)}>Close</button>
              </div>
            </div>
            <form
              className="form-grid"
              onSubmit={(event) => {
                event.preventDefault();
                handleSenderUpdate(new FormData(event.currentTarget));
              }}
              key={`sender-${sender.email || 'empty'}`}
            >
              <div><label className="label">Name</label><input name="name" className="field" defaultValue={sender.name || ''} /></div>
              <div><label className="label">Title</label><input name="title" className="field" defaultValue={sender.title || ''} /></div>
              <div><label className="label">Organization</label><input name="org" className="field" defaultValue={sender.org || ''} /></div>
              <div><label className="label">Email</label><input name="email" className="field" defaultValue={sender.email || ''} /></div>
              <div><label className="label">Phone</label><input name="phone" className="field" defaultValue={sender.phone || ''} /></div>
              <div><label className="label">Address</label><input name="address" className="field" defaultValue={sender.address || ''} /></div>
              <div className="form-actions" style={{ gridColumn: '1 / -1' }}>
                <button className="button" type="submit">Save sender profile</button>
              </div>
            </form>
          </div>
        </div>
      ) : null}

      {showAddModal ? (
        <div className="modal-overlay" role="dialog" aria-modal="true" aria-label="Add institution">
          <div className="modal-card">
            <div className="record-header">
              <h2 className="section-title" style={{ marginBottom: 0 }}>Add a new institution</h2>
              <button className="button-secondary" type="button" onClick={() => setShowAddModal(false)}>Close</button>
            </div>
            <form
              className="form-grid"
              onSubmit={(event) => {
                event.preventDefault();
                handleAddCollege(new FormData(event.currentTarget));
              }}
            >
              <div><label className="label">Institution</label><input name="institution" className="field" placeholder="New college name" /></div>
              <div><label className="label">Type</label><select name="type" className="select" defaultValue="4yr"><option value="4yr">4yr</option><option value="2yr">2yr</option></select></div>
              <div><label className="label">City</label><input name="city" className="field" placeholder="Austin" /></div>
              <div><label className="label">System / District</label><input name="system_district" className="field" placeholder="Texas State University System" /></div>
              <div><label className="label">Verified</label><select name="verified" className="select" defaultValue="incomplete"><option value="confirmed">Confirmed</option><option value="incomplete">Incomplete</option></select></div>
              <div><label className="label">Email</label><input name="public_records_email" className="field" placeholder="publicinfo@school.edu" /></div>
              <div><label className="label">Portal</label><input name="public_records_portal" className="field" placeholder="https://..." /></div>
              <div style={{ gridColumn: '1 / -1' }}><label className="label">Notes</label><textarea name="notes" className="textarea" placeholder="Why this record matters" /></div>
              <div className="form-actions" style={{ gridColumn: '1 / -1' }}>
                <button className="button" type="submit">Create college and draft request</button>
              </div>
            </form>
          </div>
        </div>
      ) : null}

      {showCommandPalette ? (
        <div className="modal-overlay" role="dialog" aria-modal="true" aria-label="Command palette">
          <div className="modal-card palette-card">
            <div className="record-header">
              <h2 className="section-title" style={{ marginBottom: 0 }}>Jump to institution</h2>
              <button className="button-secondary" type="button" onClick={() => setShowCommandPalette(false)}>Close</button>
            </div>
            <input
              className="field"
              autoFocus
              placeholder="Type institution name..."
              value={commandSearch}
              onChange={(event) => setCommandSearch(event.target.value)}
            />
            <div className="palette-results">
              {commandResults.map((record) => (
                <button
                  key={getRowKey(record)}
                  className="palette-item"
                  type="button"
                  onClick={() => {
                    setSelectedId(getRowKey(record));
                    setShowCommandPalette(false);
                    setCommandSearch('');
                  }}
                >
                  <strong>{record.institution}</strong>
                  <span className="meta">{record.city} · {STATUS_META[record.status]?.label || record.status}</span>
                </button>
              ))}
            </div>
          </div>
        </div>
      ) : null}

      {message ? (
        <div
          className="toast"
          role="button"
          tabIndex={0}
          onClick={() => setMessage('')}
          onKeyDown={(event) => {
            if (event.key === 'Enter' || event.key === ' ') {
              event.preventDefault();
              setMessage('');
            }
          }}
        >
          {message}
        </div>
      ) : null}
    </>
  );
}
