import { useEffect, useMemo, useState } from 'react';
import CountyMobileBrowseSheet from './CountyMobileBrowseSheet';
import NotesTimeline from './NotesTimeline';
import TemplateStudio from './TemplateStudio';

export default function CountyMobilePortraitShell({
  selected,
  selectedId,
  records,
  onSelectRecord,
  selectedEmail,
  selectedPortal,
  currentStatus,
  statusKeys,
  statusMetaMap,
  selectedStatusMeta,
  noteDraft,
  onChangeNote,
  noteEntries,
  timelineEntries,
  onAddNote,
  countyDraft,
  onCountyDraftChange,
  onSaveCounty,
  onStatusChange,
  templateName,
  templateLabels,
  templateKey,
  templateText,
  subject,
  onTemplateKeyChange,
  onCopyTemplate,
  onCopySubject,
  onCopyBody,
  onSendEmail,
  onOpenScreenMenu,
}) {
  const [mobileTab, setMobileTab] = useState('details');
  const [browseOpen, setBrowseOpen] = useState(false);

  useEffect(() => {
    if (!selected) return;
    setMobileTab('details');
  }, [selectedId, selected]);

  const selectedFacts = useMemo(() => {
    if (!selected) return [];

    return [
      { label: 'Status', value: statusMetaMap[selected.status]?.label || selected.status || 'draft' },
      { label: 'Verification', value: countyDraft?.verified || selected.verified || 'no' },
      { label: 'Institutions', value: String(selected.associated_institutions_count || 0) },
      { label: 'Enrollment', value: Number(selected.primary_county_enrollment_total || 0).toLocaleString() },
    ];
  }, [countyDraft?.verified, selected, statusMetaMap]);

  if (!selected || !countyDraft) return null;

  return (
    <>
      <CountyMobileBrowseSheet
        open={browseOpen}
        records={records}
        recentRecords={records.slice(0, 6)}
        selectedId={selectedId}
        statusKeys={statusKeys}
        statusMetaMap={statusMetaMap}
        onClose={() => setBrowseOpen(false)}
        onSelectRecord={(record) => {
          onSelectRecord(record);
          setBrowseOpen(false);
          setMobileTab('details');
        }}
      />

      <div className="mobile-shell county-mobile-shell">
        <div className="mobile-top-toolbar" role="region" aria-label="County actions">
          <button className="button-secondary mobile-browser-button" type="button" onClick={() => setBrowseOpen(true)}>
            Select County
          </button>
          {onOpenScreenMenu ? (
            <button
              className="button-secondary mobile-switch-button"
              type="button"
              onClick={onOpenScreenMenu}
              aria-label="Switch screens"
              title="Switch screens"
            >
              ⇄
            </button>
          ) : null}
        </div>

        <section className="main-card mobile-focus-card">
          <div className="record-header-tight">
            <h2 className="record-title">{selected.county_name} County</h2>
            <div className="meta">County elections/open records workflow</div>
          </div>

          <div className="mobile-meta-row">
            <div className="kv mobile-status-card">
              <strong>Status</strong>
              <select
                name="status"
                className="select"
                value={currentStatus}
                onChange={(event) => onStatusChange(event.target.value)}
              >
                {statusKeys.map((key) => (
                  <option key={key} value={key}>
                    {statusMetaMap[key]?.label || key}
                  </option>
                ))}
              </select>
            </div>
          </div>

          <div className="mobile-summary-grid">
            <div className="kv"><strong>County email</strong>{selectedEmail ? <a className="kv-value kv-link" href={`mailto:${selectedEmail}`}>{selectedEmail}</a> : <span className="kv-value">None</span>}</div>
            <div className="kv"><strong>Portal</strong>{selectedPortal ? <a className="kv-value kv-url kv-link" href={selectedPortal} target="_blank" rel="noreferrer">{selectedPortal}</a> : <span className="kv-value">None</span>}</div>
          </div>

          <div className="tabs mobile-tabs">
            <button type="button" className={`tab ${mobileTab === 'details' ? 'active' : ''}`} onClick={() => setMobileTab('details')}>Details</button>
            <button type="button" className={`tab ${mobileTab === 'timeline' ? 'active' : ''}`} onClick={() => setMobileTab('timeline')}>Timeline</button>
            <button type="button" className={`tab ${mobileTab === 'edit' ? 'active' : ''}`} onClick={() => setMobileTab('edit')}>Edit County</button>
            <button type="button" className={`tab ${mobileTab === 'templates' ? 'active' : ''}`} onClick={() => setMobileTab('templates')}>Template Studio</button>
          </div>

          <div className="mobile-panel-stack">
            {mobileTab === 'details' ? (
              <section className="mobile-tab-panel">
                <div className="mobile-facts-grid">
                  {selectedFacts.map((fact) => (
                    <div key={fact.label} className="kv">
                      <strong>{fact.label}</strong>
                      <span className="kv-value">{fact.value}</span>
                    </div>
                  ))}
                </div>
              </section>
            ) : null}

            {mobileTab === 'timeline' ? (
              <section className="mobile-tab-panel">
                <NotesTimeline
                  noteDraft={noteDraft}
                  noteAuthor=""
                  noteEntries={noteEntries}
                  timelineEntries={timelineEntries}
                  onChangeNote={onChangeNote}
                  onAddNote={onAddNote}
                />
              </section>
            ) : null}

            {mobileTab === 'edit' ? (
              <section className="mobile-tab-panel">
                <div className="form-grid">
                  <div><label className="label">Contact name</label><input className="field" value={countyDraft.contact_name} onChange={(event) => onCountyDraftChange('contact_name', event.target.value)} /></div>
                  <div><label className="label">Phone</label><input className="field" value={countyDraft.phone} onChange={(event) => onCountyDraftChange('phone', event.target.value)} /></div>
                  <div><label className="label">Email</label><input className="field" value={countyDraft.email} onChange={(event) => onCountyDraftChange('email', event.target.value)} /></div>
                  <div><label className="label">Portal</label><input className="field" value={countyDraft.portal} onChange={(event) => onCountyDraftChange('portal', event.target.value)} /></div>
                </div>
                <div className="form-actions" style={{ marginTop: 10 }}>
                  <button className="button-secondary" type="button" onClick={onSaveCounty}>Save county contact details</button>
                </div>
              </section>
            ) : null}

            {mobileTab === 'templates' ? (
              <section className="mobile-tab-panel">
                <TemplateStudio
                  templateName={templateName}
                  templateLabels={templateLabels}
                  subject={subject}
                  templateKey={templateKey}
                  templateText={templateText}
                  onTemplateKeyChange={onTemplateKeyChange}
                  onCopyTemplate={onCopyTemplate}
                  onCopySubject={onCopySubject}
                  onCopyBody={onCopyBody}
                />
              </section>
            ) : null}
          </div>
        </section>
      </div>
    </>
  );
}
