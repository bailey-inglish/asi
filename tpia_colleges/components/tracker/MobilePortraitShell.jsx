import { useEffect, useMemo, useState } from 'react';
import EditRecordForm from './EditRecordForm';
import MobileBrowseSheet from './MobileBrowseSheet';
import NotesTimeline from './NotesTimeline';
import RecordHeader from './RecordHeader';
import TemplateStudio from './TemplateStudio';

export default function MobilePortraitShell({
  selected,
  selectedId,
  records,
  selectedEmail,
  selectedPortal,
  currentStatus,
  statusKeys,
  statusMetaMap,
  selectedStatusMeta,
  lastUpdated,
  recentRecords,
  noteAuthor,
  noteEntries,
  timelineEntries,
  recordDraft,
  recordDirty,
  selectedCounty,
  texasCounties,
  onFieldChange,
  onSaveRecord,
  noteDraft,
  onChangeNote,
  onAddNote,
  onSelectRecord,
  onCopyTemplate,
  onStatusChange,
  templateName,
  templateLabels,
  templateKey,
  templateText,
  onTemplateKeyChange,
  subject,
  onOpenSenderModal,
  onOpenAddModal,
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
      { label: 'City', value: selected.city || 'N/A' },
      { label: 'Type', value: selected.type || 'N/A' },
      { label: 'County', value: selected.county || 'Unassigned' },
      { label: 'System', value: selected.system_district || 'N/A' },
      { label: 'Verification', value: selected.verified || 'N/A' },
      { label: 'Updated', value: lastUpdated },
    ];
  }, [lastUpdated, selected]);

  const onBrowseSelect = (record) => {
    onSelectRecord(record);
    setBrowseOpen(false);
    setMobileTab('details');
  };

  if (!selected) return null;

  return (
    <>
      <MobileBrowseSheet
        open={browseOpen}
        records={records}
        recentRecords={recentRecords}
        selectedId={selectedId}
        statusKeys={statusKeys}
        statusMetaMap={statusMetaMap}
        onClose={() => setBrowseOpen(false)}
        onSelectRecord={onBrowseSelect}
      />

      <div className="mobile-shell">
        <div className="mobile-top-toolbar" role="region" aria-label="Browse actions">
          <button className="button-secondary mobile-browser-button" type="button" onClick={() => setBrowseOpen(true)}>
            Select Institution
          </button>
          <button className="button mobile-add-button" type="button" onClick={onOpenAddModal} aria-label="Add institution" title="Add institution">
            +
          </button>
        </div>

        <section className="main-card mobile-focus-card">
          <RecordHeader selected={selected} statusMeta={selectedStatusMeta} lastUpdated={lastUpdated} />

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
            <div className="kv"><strong>Public records email</strong>{selectedEmail ? <a className="kv-value kv-link" href={`mailto:${selectedEmail}`}>{selectedEmail}</a> : <span className="kv-value">None</span>}</div>
            <div className="kv"><strong>Portal</strong>{selectedPortal ? <a className="kv-value kv-url kv-link" href={selectedPortal} target="_blank" rel="noreferrer">{selectedPortal}</a> : <span className="kv-value">None</span>}</div>
          </div>

          <div className="tabs mobile-tabs">
            <button type="button" className={`tab ${mobileTab === 'details' ? 'active' : ''}`} onClick={() => setMobileTab('details')}>Details</button>
            <button type="button" className={`tab ${mobileTab === 'timeline' ? 'active' : ''}`} onClick={() => setMobileTab('timeline')}>Timeline</button>
            <button type="button" className={`tab ${mobileTab === 'edit' ? 'active' : ''}`} onClick={() => setMobileTab('edit')}>Edit Institution</button>
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
                  noteAuthor={noteAuthor}
                  noteEntries={noteEntries}
                  timelineEntries={timelineEntries}
                  onChangeNote={onChangeNote}
                  onAddNote={onAddNote}
                />
              </section>
            ) : null}

            {mobileTab === 'edit' ? (
              <section className="mobile-tab-panel">
                <EditRecordForm
                  recordDraft={recordDraft}
                  selectedCounty={selectedCounty}
                  texasCounties={texasCounties}
                  onFieldChange={onFieldChange}
                  onSave={onSaveRecord}
                  recordDirty={recordDirty}
                />
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
                  onCopySubject={() => navigator.clipboard.writeText(subject)}
                  onCopyBody={() => navigator.clipboard.writeText(templateText)}
                />

                <div className="mobile-template-tools">
                  <button className="button-secondary" type="button" onClick={onOpenSenderModal}>
                    Edit sender
                  </button>
                </div>
              </section>
            ) : null}
          </div>
        </section>
      </div>
    </>
  );
}
