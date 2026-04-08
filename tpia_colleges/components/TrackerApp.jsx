'use client';

import { useDeferredValue, useEffect, useMemo, useState, useTransition } from 'react';
import { STATUS_KEYS, STATUS_META, SUBJECT_LINES, TEMPLATE_LABELS, SUGGESTED_TEMPLATE, TEXAS_COUNTIES } from './constants';

function statusStyle(status) {
  return STATUS_META[status] || STATUS_META.draft;
}

function verificationIcon(verified) {
  const status = String(verified || 'no').toLowerCase();
  if (status === 'yes') return '✓';
  if (status === 'partial') return '?';
  return '✗';
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

function buildVariables(record, sender) {
  const today = new Date().toISOString().slice(0, 10);
  const eventDate = record?.last_updated || record?.date_sent || today;
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
    DATE_SENT: eventDate,
    DEADLINE_DATE: record?.deadline_10day || today,
    BUSINESS_DAYS_ELAPSED: '0',
    AG_LETTER_NUMBER: '[AG LETTER NO. — check AG notice]',
    FEE_AMOUNT: '[FEE AMOUNT — from agency notice]',
    PAYMENT_METHOD: '[CHECK / CREDIT CARD / ONLINE PORTAL]',
    DENIAL_BASIS: '[CITED EXCEPTION — from denial letter]',
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
  };
}

function getSenderLabel(sender) {
  return String(sender?.name || sender?.org || sender?.email || 'Sender').trim() || 'Sender';
}

function parseNoteEntry(noteStr) {
  if (!noteStr) {
    return { date: '', author: '', body: '' };
  }
  
  const text = String(noteStr).trim();
  if (!text) {
    return { date: '', author: '', body: '' };
  }

  // Try to match: [DATE] AUTHOR: BODY or [DATE] BODY
  const match = text.match(/^\[([^\]]+)\]\s*(.*)$/);
  if (!match) {
    // No date bracket found; return as body with no date
    return { date: '', author: '', body: text };
  }

  const [, date, remainder] = match;
  if (!remainder.trim()) {
    // Just a date, no content after it
    return { date, author: '', body: '' };
  }

  // Try to extract author: AUTHOR: BODY
  const authorMatch = remainder.match(/^([^:]+):\s*(.*)$/);
  if (authorMatch) {
    const author = authorMatch[1].trim();
    const body = authorMatch[2].trim();
    return { date: date.trim(), author, body };
  }

  // No author colon found; entire remainder is body
  return { date: date.trim(), author: '', body: remainder.trim() };
}

export default function TrackerApp() {
  const [state, setState] = useState(null);
  const [selectedId, setSelectedId] = useState('');
  const [search, setSearch] = useState('');
  const [typeFilter, setTypeFilter] = useState([]);
  const [statusFilter, setStatusFilter] = useState([]);
  const [templateKey, setTemplateKey] = useState('01_initial_request');
  const [templateBody, setTemplateBody] = useState('');
  const [showSenderModal, setShowSenderModal] = useState(false);
  const [showAddModal, setShowAddModal] = useState(false);
  const [showTemplateDrawer, setShowTemplateDrawer] = useState(false);
  const [showCommandPalette, setShowCommandPalette] = useState(false);
  const [commandSearch, setCommandSearch] = useState('');
  const [actionsOpen, setActionsOpen] = useState(false);
  const [filtersOpen, setFiltersOpen] = useState(false);
  const [bulkMode, setBulkMode] = useState(false);
  const [bulkSelected, setBulkSelected] = useState([]);
  const [recentIds, setRecentIds] = useState([]);
  const [openPanel, setOpenPanel] = useState('record');
  const [statusDraft, setStatusDraft] = useState('draft');
  const [recordDraft, setRecordDraft] = useState(getRecordDraft(null));
  const [noteDraft, setNoteDraft] = useState('');
  const [isNarrow, setIsNarrow] = useState(false);
  const [message, setMessage] = useState('');
  const [, startTransition] = useTransition();
  const deferredSearch = useDeferredValue(search);

  async function loadState() {
    const response = await fetch('/api/state', { cache: 'no-store' });
    const payload = await response.json();
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

  useEffect(() => {
    loadState().catch((error) => setMessage(error.message));
  }, []);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    const media = window.matchMedia('(max-width: 1280px)');
    const sync = () => setIsNarrow(media.matches);
    sync();
    media.addEventListener('change', sync);
    return () => media.removeEventListener('change', sync);
  }, []);

  const records = state?.records || [];
  const sender = state?.sender || {};
  const selected = records.find((record) => getRowKey(record) === String(selectedId)) || records[0] || null;

  const filteredRecords = records.filter((record) => {
    const matchesType = typeFilter.length === 0 || typeFilter.includes(record.type);
    const matchesStatus = statusFilter.length === 0 || statusFilter.includes(record.status);
    const matchesText = !deferredSearch || String(record.institution).toLowerCase().includes(deferredSearch.toLowerCase());
    return matchesType && matchesStatus && matchesText;
  });

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

  useEffect(() => {
    if (selected && !filteredRecords.some((record) => getRowKey(record) === String(selectedId)) && filteredRecords[0]) {
      setSelectedId(getRowKey(filteredRecords[0]));
    }
  }, [search, typeFilter.join(','), statusFilter.join(','), selectedId, selected?.request_id, selected?.institution, selected?.city, filteredRecords]);

  useEffect(() => {
    if (!selected) return;
    setStatusDraft(selected.status || 'draft');
    setRecordDraft(getRecordDraft(selected));
    setRecentIds((current) => {
      const selectedKey = getRowKey(selected);
      const next = [selectedKey, ...current.filter((id) => id !== selectedKey)];
      return next.slice(0, 8);
    });
  }, [selected?.request_id, selected?.institution, selected?.city]);

  async function saveJson(url, payload, successText) {
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
  }

  async function patchJson(url, payload, successText) {
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
        };
      });

      return {
        ...current,
        records: nextRecords,
      };
    });
  }

  async function patchRequest(requestKey, payload, successText) {
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
  }

  async function postJson(url, payload, successText) {
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
    startTransition(async () => {
      await saveJson('/api/sender', payload, 'Saved sender profile');
      setShowSenderModal(false);
    });
  }

  async function handleStatusChange(nextStatus) {
    if (!selected) return;
    const previousStatus = currentStatus;
    setStatusDraft(nextStatus);

    const requestKey = getApiRecordId(selected, selectedId);
    startTransition(() => {
      patchRequest(
        requestKey,
        { status: nextStatus },
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
  const showEmailAction = activeContactType === 'email' || activeContactType === 'both';
  const showPortalAction = activeContactType === 'portal' || activeContactType === 'both';
  const noteAuthor = getSenderLabel(sender);
  const noteEntries = useMemo(
    () => String(selected?.notes || '').split(' | ').map((n) => n.trim()).filter(Boolean).map(parseNoteEntry),
    [selected?.notes],
  );
  const currentStatus = statusDraft || selected?.status || 'draft';

  const recordDirty = !!selected && (
    recordDraft.institution !== (selected.institution || '') ||
    recordDraft.type !== (selected.type || '4yr') ||
    recordDraft.city !== (selected.city || '') ||
    recordDraft.county !== (selected.county || '') ||
    recordDraft.system_district !== (selected.system_district || '') ||
    recordDraft.verified !== (selected.verified || 'partial') ||
    recordDraft.public_records_email !== (selected.public_records_email || '') ||
    recordDraft.public_records_portal !== (selected.public_records_portal || '')
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
        const emailField = document.getElementById('record-email');
        emailField?.focus();
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

  if (!state || !selected) {
    return (
      <div className="workspace">
        <div className="hero-card panel">
          <div className="hero-copy">
            <h1 className="page-title">Loading tracker...</h1>
            <p className="helper">Reading colleges.csv, requests.csv, and sender.json.</p>
          </div>
        </div>
      </div>
    );
  }

  const stats = {
    total: records.length,
    active: records.filter((record) => ['sent', 'acknowledged', 'in_progress', 'fee_pending', 'fee_paid', 'ag_opinion_requested', 'ag_opinion_pending'].includes(record.status)).length,
    complete: records.filter((record) => ['complete', 'partially_complete'].includes(record.status)).length,
    overdue: records.filter((record) => ['sent', 'acknowledged', 'in_progress', 'fee_pending', 'fee_paid', 'ag_opinion_requested', 'ag_opinion_pending'].includes(record.status) && (record.deadline_10day || record.deadline_ag_45day)).length,
  };

  return (
    <div className="workspace">
      <section className="hero-card panel compact-hero">
        <div className="compact-hero-grid">
          <div>
            <h1 className="page-title">Records Portal</h1>
          </div>
          <div className="stat-grid compact-stats">
            <div className="stat-card"><div className="stat-label">Institutions</div><div className="stat-value">{stats.total}</div></div>
            <div className="stat-card"><div className="stat-label">Active</div><div className="stat-value">{stats.active}</div></div>
            <div className="stat-card"><div className="stat-label">Complete</div><div className="stat-value">{stats.complete}</div></div>
            <div className="stat-card"><div className="stat-label">Overdue</div><div className="stat-value">{stats.overdue}</div></div>
          </div>
        </div>
      </section>

      <div style={{ height: 18 }} />

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
                    <button className="dropdown-item" type="button" onClick={() => { setShowCommandPalette(true); setActionsOpen(false); }}>Open command palette</button>
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
                      <p className="record-name" title={record.institution}>{record.institution} <span style={{ fontSize: '1.1em', marginLeft: '6px', fontWeight: 'bold', color: String(record.verified || 'no').toLowerCase() === 'yes' ? '#16a34a' : String(record.verified || 'no').toLowerCase() === 'partial' ? '#ea580c' : '#dc2626' }}>{verificationIcon(record.verified)}</span></p>
                      <p className="meta meta-wrap">{record.type} · {record.city} · {(record.enrollment_2025 ? `${Number(record.enrollment_2025).toLocaleString()} enrolled` : 'Enrollment n/a')} · {record.public_records_email || 'No email on file'}</p>
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
          <div className="record-header record-header-tight">
            <div>
              <div className="section-kicker">Selected institution</div>
              <h2 className="record-title" title={selected.institution}>{selected.institution}</h2>
              <p className="meta meta-wrap">{selected.type} · {selected.system_district} · {selected.city}</p>
              <p className="meta meta-wrap">Last updated: {selected.last_updated || 'N/A'}</p>
            </div>
            <span className="badge" style={{ color: statusStyle(selected.status).color, background: statusStyle(selected.status).bg }}>
              {statusStyle(selected.status).label}
            </span>
          </div>

          <div className="sticky-actions">
            {showPortalAction && selectedPortal ? <a className="button" href={selectedPortal} target="_blank" rel="noreferrer">Open portal</a> : null}
            {showEmailAction && selectedEmail ? <button className="button-secondary" type="button" onClick={() => copyText(selectedEmail).catch((error) => setMessage(error.message))}>Copy email</button> : null}
            <button className="button-secondary" type="button" onClick={() => copyText(fullEmailText).catch((error) => setMessage(error.message))}>Copy template</button>
            {isNarrow ? <button className="button-secondary" type="button" onClick={() => setShowTemplateDrawer(true)}>Open template</button> : null}
          </div>

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

          <div className="workflow-stack">
            <section className="workflow-section">
              <button className="section-toggle" type="button" onClick={() => setOpenPanel((current) => current === 'record' ? '' : 'record')}>
                <div>
                  <h3 className="section-title">Edit record</h3>
                  <p className="subtle">Maintain institution and contact metadata.</p>
                </div>
                <div className="toggle-meta">
                  {recordDirty ? <span className="dirty-dot" /> : null}
                  <span className="badge">{openPanel === 'record' ? 'Collapse' : 'Expand'}</span>
                </div>
              </button>

              {openPanel === 'record' ? (
                <div className="accordion-body">
                  <div className="form-grid narrow-form">
                    <div><label className="label">Institution</label><input name="institution" className="field" value={recordDraft.institution} onChange={(event) => setRecordDraft((draft) => ({ ...draft, institution: event.target.value }))} /></div>
                    <div><label className="label">Type</label><select name="type" className="select" value={recordDraft.type} onChange={(event) => setRecordDraft((draft) => ({ ...draft, type: event.target.value }))}><option value="4yr">4yr</option><option value="2yr">2yr</option></select></div>
                    <div><label className="label">City</label><input name="city" className="field" value={recordDraft.city} onChange={(event) => setRecordDraft((draft) => ({ ...draft, city: event.target.value }))} /></div>
                    <div><label className="label">System / District</label><input name="system_district" className="field" value={recordDraft.system_district} onChange={(event) => setRecordDraft((draft) => ({ ...draft, system_district: event.target.value }))} /></div>
                    <div><label className="label">Verification</label><select name="verified" className="select" value={recordDraft.verified} onChange={(event) => setRecordDraft((draft) => ({ ...draft, verified: event.target.value }))}><option value="yes">yes</option><option value="partial">partial</option><option value="no">no</option></select></div>
                    <div><label className="label">County</label><select name="county" className="select" value={recordDraft.county || selected.county || ''} onChange={(event) => setRecordDraft((draft) => ({ ...draft, county: event.target.value }))}><option value="">Select a county</option>{TEXAS_COUNTIES.map((county) => <option key={county} value={county}>{county}</option>)}</select></div>
                    <div><label className="label">Email</label><input id="record-email" name="public_records_email" className="field" value={recordDraft.public_records_email} onChange={(event) => setRecordDraft((draft) => ({ ...draft, public_records_email: event.target.value }))} /></div>
                    <div><label className="label">Portal</label><input name="public_records_portal" className="field" value={recordDraft.public_records_portal} onChange={(event) => setRecordDraft((draft) => ({ ...draft, public_records_portal: event.target.value }))} /></div>
                    <div className="form-actions" style={{ gridColumn: '1 / -1' }}>
                      <button className="button" type="button" onClick={() => handleSaveRecord().catch((error) => setMessage(error.message))}>Save college record</button>
                    </div>
                  </div>
                </div>
              ) : null}
            </section>

            <section className="workflow-section">
              <button className="section-toggle" type="button" onClick={() => setOpenPanel((current) => current === 'notes' ? '' : 'notes')}>
                <div>
                  <h3 className="section-title">Notes timeline</h3>
                  <p className="subtle">Chronological notes attached to this institution.</p>
                </div>
                <span className="badge">{openPanel === 'notes' ? 'Collapse' : 'Expand'}</span>
              </button>
              {openPanel === 'notes' ? (
                <div className="accordion-body">
                  <div className="note-composer">
                    <label className="label" htmlFor="note-draft">Add note</label>
                    <textarea
                      id="note-draft"
                      className="textarea"
                      value={noteDraft}
                      onChange={(event) => setNoteDraft(event.target.value)}
                      placeholder={`Write a note that will be attributed to ${noteAuthor}`}
                    />
                    <div className="form-actions">
                      <button className="button" type="button" onClick={() => handleAddNote().catch((error) => setMessage(error.message))}>Add note</button>
                      <span className="subtle">Saved as {noteAuthor}</span>
                    </div>
                  </div>
                  <div className="timeline">
                    {noteEntries.length
                      ? noteEntries.map((entry, index) => (
                        <div className="timeline-item" key={`${entry.date || 'note'}-${index}`}>
                          <div className="timeline-meta">
                            <strong>{entry.date || 'No date'}</strong>
                            {entry.author ? <span>{entry.author}</span> : null}
                          </div>
                          <div className="timeline-body">{entry.body}</div>
                        </div>
                      ))
                      : <div className="empty-state">No notes yet.</div>}
                  </div>
                </div>
              ) : null}
            </section>
          </div>
        </section>

        {!isNarrow ? (
          <section className="main-card workflow-panel template-panel">
            <div className="record-header record-header-tight">
              <div>
                <div className="section-kicker">Template studio</div>
                <h2 className="section-title" style={{ marginBottom: 4 }}>{TEMPLATE_LABELS[templateName] || 'Template'}</h2>
                <p className="meta meta-wrap">{subject}</p>
              </div>
              <div className="template-tools">
                <label className="label">Template stage</label>
                <select className="select" value={templateKey} onChange={(event) => setTemplateKey(event.target.value)}>
                  {Object.keys(TEMPLATE_LABELS).map((key) => <option key={key} value={key}>{TEMPLATE_LABELS[key]}</option>)}
                </select>
              </div>
            </div>

            <div className="record-actions" style={{ margin: '12px 0 12px' }}>
              <button className="button" onClick={() => copyText(fullEmailText).catch((error) => setMessage(error.message))}>Copy template</button>
              <button className="button-secondary" onClick={() => copyText(subject).catch((error) => setMessage(error.message))}>Copy subject</button>
              <button className="button-secondary" onClick={() => copyText(templateText).catch((error) => setMessage(error.message))}>Copy body</button>
            </div>
            <textarea className="textarea template-preview" readOnly value={templateText} onFocus={(event) => event.currentTarget.select()} />
          </section>
        ) : null}
      </div>

      {isNarrow && showTemplateDrawer ? (
        <div className="modal-overlay" role="dialog" aria-modal="true" aria-label="Template drawer">
          <div className="modal-card drawer-card">
            <div className="record-header record-header-tight">
              <h2 className="section-title" style={{ marginBottom: 0 }}>Template studio</h2>
              <button className="button-secondary" type="button" onClick={() => setShowTemplateDrawer(false)}>Close</button>
            </div>
            <div className="template-tools" style={{ marginBottom: 10 }}>
              <label className="label">Template stage</label>
              <select className="select" value={templateKey} onChange={(event) => setTemplateKey(event.target.value)}>
                {Object.keys(TEMPLATE_LABELS).map((key) => <option key={key} value={key}>{TEMPLATE_LABELS[key]}</option>)}
              </select>
            </div>
            <div className="record-actions" style={{ marginBottom: 10 }}>
              <button className="button" onClick={() => copyText(fullEmailText).catch((error) => setMessage(error.message))}>Copy template</button>
              <button className="button-secondary" onClick={() => copyText(subject).catch((error) => setMessage(error.message))}>Copy subject</button>
              <button className="button-secondary" onClick={() => copyText(templateText).catch((error) => setMessage(error.message))}>Copy body</button>
            </div>
            <textarea className="textarea template-preview" readOnly value={templateText} onFocus={(event) => event.currentTarget.select()} />
          </div>
        </div>
      ) : null}

      {showSenderModal ? (
        <div className="modal-overlay" role="dialog" aria-modal="true" aria-label="Edit sender profile">
          <div className="modal-card">
            <div className="record-header">
              <h2 className="section-title" style={{ marginBottom: 0 }}>Sender profile</h2>
              <button className="button-secondary" type="button" onClick={() => setShowSenderModal(false)}>Close</button>
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
              <div><label className="label">Verification</label><select name="verified" className="select" defaultValue="partial"><option value="yes">yes</option><option value="partial">partial</option><option value="no">no</option></select></div>
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

      {message ? <div className="toast">{message}</div> : null}
    </div>
  );
}
