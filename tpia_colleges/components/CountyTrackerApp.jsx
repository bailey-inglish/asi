'use client';

import { useEffect, useMemo, useState } from 'react';
import ActionBar from './tracker/ActionBar';
import CountyMobilePortraitShell from './tracker/CountyMobilePortraitShell';
import NotesTimeline from './tracker/NotesTimeline';
import TemplateStudio from './tracker/TemplateStudio';
import { STATUS_KEYS, STATUS_META } from './constants';

function applyTemplate(template, variables) {
  return Object.entries(variables || {}).reduce(
    (text, [key, value]) => text.replaceAll(`{{${key}}}`, String(value ?? '')),
    template,
  );
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
        return {
          from: entry.from == null ? null : String(entry.from),
          to: toStatus,
          user: String(entry.user || 'System'),
          at: Number.isNaN(atDate.getTime()) ? new Date().toISOString() : atDate.toISOString(),
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

function verificationIcon(verified) {
  const status = String(verified || 'no').toLowerCase();
  if (status === 'yes') return '';
  return '?';
}

function getVerifiedDisplayLabel(verified) {
  return String(verified || '').toLowerCase() === 'yes' ? 'Confirmed' : 'Incomplete';
}

function parseNoteEntry(noteStr) {
  if (!noteStr) {
    return { date: '', author: '', body: '', at: '' };
  }

  const text = String(noteStr).trim();
  if (!text) {
    return { date: '', author: '', body: '', at: '' };
  }

  const match = text.match(/^\[([^\]]+)\]\s*(.*)$/);
  if (!match) {
    return { date: '', author: '', body: text, at: '' };
  }

  const [, date, remainder] = match;
  if (!remainder.trim()) {
    return { date, author: '', body: '', at: '' };
  }

  const authorMatch = remainder.match(/^([^:]+):\s*(.*)$/);
  if (authorMatch) {
    const author = authorMatch[1].trim();
    const body = authorMatch[2].trim();
    const at = new Date(date);
    return { date: date.trim(), author, body, at: Number.isNaN(at.getTime()) ? '' : at.toISOString() };
  }

  const at = new Date(date);
  return {
    date: date.trim(),
    author: '',
    body: remainder.trim(),
    at: Number.isNaN(at.getTime()) ? '' : at.toISOString(),
  };
}

function getSenderLabel(sender) {
  return String(sender?.name || sender?.org || sender?.email || 'Sender').trim() || 'Sender';
}

function inferContactType(email, portal) {
  const hasEmail = String(email || '').trim().length > 0;
  const hasPortal = String(portal || '').trim().length > 0;
  if (hasEmail && hasPortal) return 'both';
  if (hasPortal) return 'portal';
  return 'email';
}

function buildTemplateVariables(record, sender) {
  const today = new Date().toISOString().slice(0, 10);
  return {
    COUNTY_NAME: String(record?.county_name || ''),
    CONTACT_NAME: String(record?.contact_name || '[PUBLIC INFORMATION OFFICER]'),
    RECIPIENT_EMAIL: String(record?.email || '[RECIPIENT EMAIL]'),
    TODAY: today,
    DATE_SENT: String(record?.date_sent || ''),
    DEADLINE_DATE: String(record?.deadline_10day || ''),
    SENDER_NAME: String(sender?.name || '[YOUR NAME]'),
    SENDER_TITLE: String(sender?.title || '[YOUR TITLE]'),
    SENDER_ORG: String(sender?.org || '[YOUR ORGANIZATION]'),
    SENDER_EMAIL: String(sender?.email || '[YOUR EMAIL]'),
    SENDER_PHONE: String(sender?.phone || '[YOUR PHONE]'),
    SENDER_ADDRESS: String(sender?.address || '[YOUR ADDRESS]'),
  };
}

export default function CountyTrackerApp({ preloadedState = null, onOpenScreenMenu = null } = {}) {
  const [state, setState] = useState(null);
  const [selectedId, setSelectedId] = useState('');
  const [countyDraft, setCountyDraft] = useState(null);
  const [statusDraft, setStatusDraft] = useState('draft');
  const [sortOrder, setSortOrder] = useState('alpha');
  const [noteDraft, setNoteDraft] = useState('');
  const [templateKey, setTemplateKey] = useState('01_county_initial_request');
  const [templateBody, setTemplateBody] = useState('');
  const [templateBodies, setTemplateBodies] = useState({});
  const [showNotesModal, setShowNotesModal] = useState(false);
  const [showTemplateDrawer, setShowTemplateDrawer] = useState(false);
  const [filtersOpen, setFiltersOpen] = useState(false);
  const [search, setSearch] = useState('');
  const [message, setMessage] = useState('');
  const [savingCount, setSavingCount] = useState(0);

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
    const selectable = payload.selectableCountyRecords || [];
    setSelectedId((current) => {
      const currentId = String(current || '');
      if (currentId && selectable.some((row) => String(row.county_id) === currentId)) {
        return currentId;
      }
      return String(selectable[0]?.county_id || '');
    });
  }

  async function loadState() {
    const response = await fetch('/api/county-state', { cache: 'no-store' });
    const payload = await response.json();
    if (!response.ok) {
      throw new Error(payload?.error || 'Failed to load county state');
    }

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
    if (!message) return undefined;
    const timeoutId = window.setTimeout(() => setMessage(''), 5000);
    return () => window.clearTimeout(timeoutId);
  }, [message]);

  const selectableRecords = state?.selectableCountyRecords || [];
  const selected = selectableRecords.find((row) => String(row.county_id) === String(selectedId)) || selectableRecords[0] || null;
  const sender = state?.sender || {};
  const templateLabels = state?.countyTemplateLabels || {};
  const subjectLines = state?.countySubjectLines || {};
  const suggestedTemplates = state?.countySuggestedTemplate || {};

  const filteredRecords = useMemo(() => {
    const q = String(search || '').trim().toLowerCase();
    const filtered = !q ? selectableRecords : selectableRecords.filter((row) => String(row.county_name || '').toLowerCase().includes(q));
    const collator = new Intl.Collator('en', { sensitivity: 'base' });
    return [...filtered].sort((left, right) => {
      if (sortOrder === 'enrollment') {
        const leftEnrollment = Number(left.primary_county_enrollment_total || 0);
        const rightEnrollment = Number(right.primary_county_enrollment_total || 0);
        if (leftEnrollment !== rightEnrollment) return rightEnrollment - leftEnrollment;
        return collator.compare(String(left.county_name || ''), String(right.county_name || ''));
      }

      if (sortOrder === 'recent') {
        const leftUpdated = new Date(left.last_updated || left.status_changed_at || 0).getTime() || 0;
        const rightUpdated = new Date(right.last_updated || right.status_changed_at || 0).getTime() || 0;
        if (leftUpdated !== rightUpdated) return rightUpdated - leftUpdated;
        return collator.compare(String(left.county_name || ''), String(right.county_name || ''));
      }

      if (sortOrder === 'staleness') {
        const leftTerminal = ['complete', 'partially_complete', 'denied', 'closed'].includes(String(left.status || '').trim());
        const rightTerminal = ['complete', 'partially_complete', 'denied', 'closed'].includes(String(right.status || '').trim());
        if (leftTerminal !== rightTerminal) return leftTerminal ? 1 : -1;

        const leftUpdated = new Date(left.last_updated || left.status_changed_at || 0).getTime() || 0;
        const rightUpdated = new Date(right.last_updated || right.status_changed_at || 0).getTime() || 0;
        if (leftUpdated !== rightUpdated) return leftUpdated - rightUpdated;
        return collator.compare(String(left.county_name || ''), String(right.county_name || ''));
      }

      return collator.compare(String(left.county_name || ''), String(right.county_name || ''));
    });
  }, [search, selectableRecords, sortOrder]);

  useEffect(() => {
    if (!selected) return;
    setCountyDraft({
      county_name: String(selected.county_name || ''),
      contact_name: String(selected.contact_name || ''),
      email: String(selected.email || ''),
      portal: String(selected.portal || ''),
      phone: String(selected.phone || ''),
      verified: String(selected.verified || 'no'),
    });
    setStatusDraft(String(selected.status || 'draft'));
    setTemplateKey(String(suggestedTemplates[selected.status] || '01_county_initial_request'));
  }, [selected?.county_id, selected?.status, suggestedTemplates]);

  useEffect(() => {
    let active = true;
    fetch(`/api/county-templates/${templateKey}`, { cache: 'no-store' })
      .then(async (response) => {
        if (!response.ok) {
          throw new Error(`Template ${templateKey} could not be loaded`);
        }
        return response.text();
      })
      .then((text) => {
        if (!active) return;
        setTemplateBody(text);
        setTemplateBodies((current) => ({ ...current, [templateKey]: text }));
      })
      .catch((error) => {
        if (!active) return;
        setTemplateBody('');
        setMessage(error.message);
      });

    return () => {
      active = false;
    };
  }, [templateKey]);

  async function loadTemplateText(key) {
    if (templateBodies[key]) return templateBodies[key];
    const response = await fetch(`/api/county-templates/${key}`, { cache: 'no-store' });
    if (!response.ok) throw new Error(`Template ${key} could not be loaded`);
    const text = await response.text();
    setTemplateBodies((current) => ({ ...current, [key]: text }));
    return text;
  }

  async function patchCounty(payload, successText) {
    if (!selected) return;

    return withSaving(async () => {
      const response = await fetch(`/api/counties/${encodeURIComponent(String(selected.county_id))}`, {
        method: 'PATCH',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload),
      });

      const data = await response.json().catch(() => ({}));
      if (!response.ok) {
        throw new Error(data?.error || 'Failed to save county request');
      }

      if (successText) {
        setMessage(successText);
      }

      await loadState();
    });
  }

  async function handleSaveCounty() {
    if (!countyDraft || !selected) return;
    await patchCounty({ ...countyDraft }, `Saved county contact info for ${selected.county_name}`);
  }

  async function handleStatusChange(nextStatus) {
    if (!selected) return;
    setStatusDraft(nextStatus);
    await patchCounty({ status: nextStatus, status_user: getSenderLabel(sender) }, `Saved county status for ${selected.county_name}`);
  }

  async function handleAddNote() {
    if (!selected) return;
    const note = String(noteDraft || '').trim();
    if (!note) {
      setMessage('Enter a note before saving.');
      return;
    }
    await patchCounty({ append_note: note, note_author: getSenderLabel(sender) }, `Added note for ${selected.county_name}`);
    setNoteDraft('');
  }

  async function handleSendEmail() {
    if (!selected) return;
    const targetEmail = String(countyDraft?.email || selected.email || '').trim();
    if (!targetEmail) {
      setMessage('No county email is available for this record.');
      return;
    }

    const bestTemplateKey = String(suggestedTemplates[selected.status] || '01_county_initial_request');
    const bestTemplateBody = await loadTemplateText(bestTemplateKey);
    const subject = String(subjectLines[bestTemplateKey] || 'TPIA Voter Records Request - {county}')
      .replace('{county}', String(selected.county_name || 'County'));
    const body = applyTemplate(bestTemplateBody, buildTemplateVariables({ ...selected, ...countyDraft }, sender));

    window.location.href = `mailto:${encodeURIComponent(targetEmail)}?subject=${encodeURIComponent(subject)}&body=${encodeURIComponent(body)}`;
  }

  async function handleVerifiedChange(nextValue) {
    if (!selected) return;
    const verified = nextValue === 'confirmed' ? 'yes' : 'partial';
    setCountyDraft((draft) => ({ ...draft, verified }));
    await patchCounty({ verified }, `Saved county verification for ${selected.county_name}`);
  }

  async function handleQuickCopyTemplate() {
    if (!selected) return;
    const bestTemplateKey = String(suggestedTemplates[selected.status] || '01_county_initial_request');
    const bestTemplateBody = await loadTemplateText(bestTemplateKey);
    const subject = String(subjectLines[bestTemplateKey] || 'TPIA Voter Records Request - {county}')
      .replace('{county}', String(selected.county_name || 'County'));
    const body = applyTemplate(bestTemplateBody, buildTemplateVariables({ ...selected, ...countyDraft }, sender));
    const fullText = `${subject}\n\n${body}`;
    await copyText(fullText);
  }

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
      date: entry.at,
      author: entry.user,
      body: formatStatusTransition(entry.from, entry.to),
      at: entry.at,
      sortOrder: index,
    })),
    [selected?.status_log],
  );

  const timelineEntries = useMemo(() => [...statusEntries, ...noteEntries].sort((a, b) => {
    const aTime = new Date(a.at || 0).getTime() || 0;
    const bTime = new Date(b.at || 0).getTime() || 0;
    if (aTime === bTime) return a.sortOrder - b.sortOrder;
    return aTime - bTime;
  }), [statusEntries, noteEntries]);

  const currentTemplateText = applyTemplate(templateBody, buildTemplateVariables({ ...selected, ...countyDraft }, sender));
  const currentSubject = String(subjectLines[templateKey] || 'TPIA Voter Records Request - {county}')
    .replace('{county}', String(selected?.county_name || 'County'));
  const selectedEmail = String(countyDraft?.email || selected?.email || '').trim();
  const selectedPortal = String(countyDraft?.portal || selected?.portal || '').trim();
  const activeContactType = inferContactType(selectedEmail, selectedPortal);
  const showEmailAction = activeContactType === 'email' || activeContactType === 'both';
  const showPortalAction = activeContactType === 'portal' || activeContactType === 'both';
  const emailPrimary = activeContactType === 'email';
  const templateName = templateKey || '01_county_initial_request';
  const fullEmailText = `${currentSubject}\n\n${currentTemplateText}`;

  async function copyText(text) {
    await navigator.clipboard.writeText(text);
    setMessage('Copied to clipboard');
  }

  if (!state || !selected || !countyDraft) {
    return (
      <div className="workspace">
        <section className="hero-card panel skeleton-shell desktop-skeleton-shell">
          <div className="skeleton-line skeleton-title" />
          <div className="skeleton-line skeleton-subtitle" />
          <div className="skeleton-grid" style={{ marginTop: 14 }}>
            <div className="skeleton-card" />
            <div className="skeleton-card" />
            <div className="skeleton-card" />
            <div className="skeleton-card" />
          </div>
        </section>

        <section className="hero-card panel skeleton-shell mobile-skeleton-shell">
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
        </section>
      </div>
    );
  }

  return (
    <>
      <CountyMobilePortraitShell
        selected={selected}
        selectedId={selectedId}
        records={selectableRecords}
        onSelectRecord={(record) => setSelectedId(String(record?.county_id || ''))}
        selectedEmail={selectedEmail}
        selectedPortal={selectedPortal}
        currentStatus={statusDraft}
        statusKeys={STATUS_KEYS}
        statusMetaMap={STATUS_META}
        selectedStatusMeta={STATUS_META[selected.status] || STATUS_META.draft}
        noteDraft={noteDraft}
        onChangeNote={setNoteDraft}
        noteEntries={noteEntries}
        timelineEntries={timelineEntries}
        onAddNote={() => handleAddNote().catch((error) => setMessage(error.message))}
        countyDraft={countyDraft}
        onCountyDraftChange={(field, value) => {
          if (field === 'county_name') {
            const next = selectableRecords.find((row) => String(row.county_name || '') === String(value || ''));
            if (next) {
              setSelectedId(String(next.county_id));
              return;
            }
          }
          setCountyDraft((draft) => ({ ...draft, [field]: value }));
        }}
        onSaveCounty={() => handleSaveCounty().catch((error) => setMessage(error.message))}
        onStatusChange={(nextStatus) => handleStatusChange(nextStatus).catch((error) => setMessage(error.message))}
        templateName={templateName}
        templateLabels={templateLabels}
        templateKey={templateKey}
        templateText={currentTemplateText}
        subject={currentSubject}
        onTemplateKeyChange={setTemplateKey}
        onCopyTemplate={() => handleQuickCopyTemplate().catch((error) => setMessage(error.message))}
        onCopySubject={() => copyText(currentSubject).catch((error) => setMessage(error.message))}
        onCopyBody={() => copyText(currentTemplateText).catch((error) => setMessage(error.message))}
        onSendEmail={() => handleSendEmail().catch((error) => setMessage(error.message))}
        onOpenScreenMenu={onOpenScreenMenu}
      />

    <div className="workspace workspace-fixed county-workspace desktop-layout">
      <section className="hero-card panel compact-hero">
        <div className="compact-hero-grid">
          <div>
            <h1 className="page-title">County Elections</h1>
            <div className="saving-status" aria-live="polite" aria-atomic="true" style={{ marginTop: 10 }}>
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
            <div className="stat-card"><div className="stat-label">All Counties</div><div className="stat-value">{(state.countyRecords || []).length}</div></div>
            <div className="stat-card"><div className="stat-label">Selectable</div><div className="stat-value">{selectableRecords.length}</div></div>
            <div className="stat-card"><div className="stat-label">Sent+</div><div className="stat-value">{selectableRecords.filter((row) => row.status !== 'draft').length}</div></div>
            <div className="stat-card"><div className="stat-label">Complete</div><div className="stat-value">{selectableRecords.filter((row) => ['complete', 'partially_complete'].includes(row.status)).length}</div></div>
          </div>
        </div>
      </section>

      <div className="workspace-gap" />

      <div className="workflow-grid">
        <section className="main-card list-panel">
          <div className="record-header" style={{ marginBottom: 10 }}>
            <div>
              <h2 className="section-title" style={{ marginBottom: 4 }}>Counties</h2>
              <p className="subtle">{filteredRecords.length} of {selectableRecords.length} shown</p>
            </div>
            <div className="record-actions">
              <button className="button-secondary" type="button" onClick={() => setFiltersOpen((value) => !value)}>
                {filtersOpen ? 'Hide filters' : 'Show filters'}
              </button>
            </div>
          </div>

          {filtersOpen ? (
            <div className="search-stack" style={{ marginTop: 0 }}>
              <div className="search-row" style={{ marginBottom: 12 }}>
                <label className="label" htmlFor="county-search">Search county</label>
                <input
                  id="county-search"
                  className="field"
                  value={search}
                  onChange={(event) => setSearch(event.target.value)}
                  placeholder="Travis, Harris, Bexar..."
                />
              </div>

              <div className="search-row" style={{ marginBottom: 0 }}>
                <label className="label" htmlFor="county-sort">Sort by</label>
                <select id="county-sort" className="select" value={sortOrder} onChange={(event) => setSortOrder(event.target.value)}>
                  <option value="alpha">A-Z</option>
                  <option value="enrollment">Enrollment</option>
                  <option value="recent">Most recently changed</option>
                  <option value="staleness">Longest since status update</option>
                </select>
              </div>
            </div>
          ) : null}

          <div className="record-list">
            {filteredRecords.map((row) => {
              const meta = STATUS_META[row.status] || STATUS_META.draft;
              const active = String(row.county_id) === String(selectedId);
              return (
                <button
                  key={row.county_id}
                  type="button"
                  className={`record-row ${active ? 'active' : ''}`}
                  style={{ borderLeftColor: meta.color }}
                  onClick={() => setSelectedId(String(row.county_id))}
                >
                  <div className="record-top">
                    <div>
                      <p className="record-name" title={row.county_name}>
                        {row.county_name} County{' '}
                        {verificationIcon(row.verified) ? (
                          <span
                            className="record-status-icon verification"
                            style={{ color: '#ea580c' }}
                            title={`Verified: ${getVerifiedDisplayLabel(row.verified)}`}
                            aria-label={`Verified ${getVerifiedDisplayLabel(row.verified)}`}
                          >
                            {verificationIcon(row.verified)}
                          </span>
                        ) : null}
                      </p>
                      <p className="meta meta-wrap">
                        {row.associated_institutions_count} institution{Number(row.associated_institutions_count || 0) === 1 ? '' : 's'} tracked · {Number(row.primary_county_enrollment_total || 0).toLocaleString()} enrolled
                      </p>
                    </div>
                    <span className="badge" style={{ color: meta.color, background: meta.bg }}>{meta.label}</span>
                  </div>
                </button>
              );
            })}
          </div>
        </section>

        <section className="main-card workflow-panel county-tab-panel">
          <div className="record-header">
            <div>
              <h2 className="section-title" style={{ marginBottom: 0 }}>{selected.county_name} County</h2>
            </div>
          </div>

          <ActionBar
            selectedPortal={showPortalAction && selectedPortal ? selectedPortal : ''}
            selectedEmail={showEmailAction && selectedEmail ? selectedEmail : ''}
            emailPrimary={emailPrimary}
            onSendEmail={() => handleSendEmail().catch((error) => setMessage(error.message))}
            onCopyTemplate={() => handleQuickCopyTemplate().catch((error) => setMessage(error.message))}
            onOpenNotes={() => setShowNotesModal(true)}
            onOpenTemplate={() => setShowTemplateDrawer(true)}
          />

          <div className="kv-grid" style={{ marginTop: 12 }}>
            <div className="kv">
              <strong>Status</strong>
              <select className="select" value={statusDraft} onChange={(event) => handleStatusChange(event.target.value).catch((error) => setMessage(error.message))}>
                {STATUS_KEYS.map((key) => <option key={key} value={key}>{STATUS_META[key]?.label || key}</option>)}
              </select>
            </div>
            <div className="kv">
              <strong>Verified</strong>
              <select className="select" value={String(countyDraft.verified || '').toLowerCase() === 'yes' ? 'confirmed' : 'incomplete'} onChange={(event) => handleVerifiedChange(event.target.value).catch((error) => setMessage(error.message))}>
                <option value="confirmed">Confirmed</option>
                <option value="incomplete">Incomplete</option>
              </select>
            </div>
          </div>

          <div className="form-grid" style={{ marginTop: 12 }}>
            <div><label className="label">Contact name</label><input className="field" value={countyDraft.contact_name} onChange={(event) => setCountyDraft((draft) => ({ ...draft, contact_name: event.target.value }))} /></div>
            <div><label className="label">Phone</label><input className="field" value={countyDraft.phone} onChange={(event) => setCountyDraft((draft) => ({ ...draft, phone: event.target.value }))} /></div>
            <div><label className="label">Email</label><input className="field" value={countyDraft.email} onChange={(event) => setCountyDraft((draft) => ({ ...draft, email: event.target.value }))} /></div>
            <div><label className="label">Portal</label><input className="field" value={countyDraft.portal} onChange={(event) => setCountyDraft((draft) => ({ ...draft, portal: event.target.value }))} /></div>
            <div className="form-actions" style={{ gridColumn: '1 / -1' }}>
              <button className="button-secondary" type="button" onClick={() => handleSaveCounty().catch((error) => setMessage(error.message))}>Save county contact details</button>
            </div>
          </div>

          <div className="workflow-section" style={{ marginTop: 12 }}>
            <div className="record-header" style={{ marginBottom: 8 }}>
              <h3 className="section-title" style={{ margin: 0 }}>Associated institutions</h3>
              <span className="badge">{selected.associated_institutions_count}</span>
            </div>
            <div className="county-associated-list">
              {(selected.associated_institutions || []).map((institution) => {
                const statusMeta = STATUS_META[institution.status] || STATUS_META.draft;
                return (
                  <div key={`${institution.id}-${institution.institution}`} className="county-associated-item">
                    <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'flex-start', gap: '8px' }}>
                      <strong>{institution.institution}</strong>
                      <span className="badge" style={{ color: statusMeta.color, background: statusMeta.bg, flexShrink: 0 }}>{statusMeta.label}</span>
                    </div>
                    <span className="meta">{institution.type || 'Type n/a'} · {institution.city || 'Unknown city'} · {institution.enrollment_2025 ? `${Number(institution.enrollment_2025).toLocaleString()} enrolled` : 'Enrollment n/a'}</span>
                  </div>
                );
              })}
            </div>
          </div>
        </section>
      </div>

      {showNotesModal ? (
        <div className="modal-overlay" role="dialog" aria-modal="true" aria-label="County notes">
          <div className="modal-card">
            <NotesTimeline
              noteDraft={noteDraft}
              noteAuthor={getSenderLabel(sender)}
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
        <div className="modal-overlay" role="dialog" aria-modal="true" aria-label="County templates">
          <div className="modal-card drawer-card">
            <TemplateStudio
              templateName={templateName}
              templateLabels={templateLabels}
              subject={currentSubject}
              templateKey={templateKey}
              templateText={currentTemplateText}
              onTemplateKeyChange={setTemplateKey}
              onCopyTemplate={() => copyText(fullEmailText).catch((error) => setMessage(error.message))}
              onCopySubject={() => copyText(currentSubject).catch((error) => setMessage(error.message))}
              onCopyBody={() => copyText(currentTemplateText).catch((error) => setMessage(error.message))}
              onClose={() => setShowTemplateDrawer(false)}
            />
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
    </div>
    </>
  );
}
