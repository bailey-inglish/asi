export default function RecordHeader({ selected, statusMeta, lastUpdated, showEditButton = false, onEditInstitution }) {
  if (!selected) return null;

  return (
    <div className="record-header record-header-tight">
      <div>
        <div className="record-title-row">
          <h2 className="record-title" title={selected.institution}>{selected.institution}</h2>
          {showEditButton ? (
            <button
              className="icon-button"
              type="button"
              aria-label="Edit institution"
              title="Edit institution"
              onClick={onEditInstitution}
            >
              ✎
            </button>
          ) : null}
        </div>
        <p className="meta meta-wrap record-header-meta">{selected.type} · {selected.system_district} · {selected.city}</p>
        <p className="meta meta-wrap record-header-meta">Last updated: {lastUpdated}</p>
      </div>
      <span className="badge" style={{ color: statusMeta.color, background: statusMeta.bg }}>
        {statusMeta.label}
      </span>
    </div>
  );
}