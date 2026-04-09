import { useMemo, useState } from 'react';

function getRecordKey(record) {
  const institution = String(record?.institution || 'institution').trim();
  const city = String(record?.city || 'city').trim();
  return String(record?.request_id || record?.id || `${institution}::${city}`);
}

export default function MobileBrowseSheet({
  open,
  records,
  recentRecords,
  selectedId,
  statusKeys,
  statusMetaMap,
  onClose,
  onSelectRecord,
}) {
  const [query, setQuery] = useState('');
  const [statusFilters, setStatusFilters] = useState([]);

  const filteredRecords = useMemo(() => {
    const normalizedQuery = query.trim().toLowerCase();

    return records.filter((record) => {
      const matchesQuery = !normalizedQuery
        || String(record.institution || '').toLowerCase().includes(normalizedQuery)
        || String(record.city || '').toLowerCase().includes(normalizedQuery)
        || String(record.system_district || '').toLowerCase().includes(normalizedQuery);

      const matchesStatus = statusFilters.length === 0 || statusFilters.includes(String(record.status || 'draft'));

      return matchesQuery && matchesStatus;
    });
  }, [query, records, statusFilters]);

  function toggleStatus(status) {
    setStatusFilters((current) => (current.includes(status) ? current.filter((value) => value !== status) : [...current, status]));
  }

  if (!open) return null;

  return (
    <div className="mobile-sheet-overlay" role="dialog" aria-modal="true" aria-label="Browse records">
      <div className="mobile-sheet">
        <div className="mobile-sheet-header">
          <div>
            <div className="section-kicker">Browse records</div>
            <h2 className="section-title" style={{ marginBottom: 0 }}>Find an institution</h2>
          </div>
          <button className="button-secondary" type="button" onClick={onClose}>Close</button>
        </div>

        <div className="mobile-sheet-search">
          <input
            className="field"
            value={query}
            onChange={(event) => setQuery(event.target.value)}
            placeholder="Search name, city, or district"
            autoComplete="off"
            autoFocus
          />

          <div className="mobile-chip-row mobile-filter-row" aria-label="Status filters">
            {statusKeys.map((status) => (
              <button
                key={status}
                type="button"
                className={statusFilters.includes(status) ? 'chip-active' : 'chip'}
                onClick={() => toggleStatus(status)}
              >
                {statusMetaMap[status]?.label || status}
              </button>
            ))}
          </div>

          {recentRecords.length ? (
            <div className="mobile-recent-block">
              <div className="label" style={{ marginBottom: 8 }}>Recent</div>
              <div className="mobile-chip-row">
                {recentRecords.map((record) => (
                  <button
                    key={getRecordKey(record)}
                    type="button"
                    className="chip"
                    onClick={() => {
                      onSelectRecord(record);
                      onClose();
                    }}
                  >
                    {record.institution}
                  </button>
                ))}
              </div>
            </div>
          ) : null}
        </div>

        <div className="mobile-record-list" role="list" aria-label="Institution results">
          {filteredRecords.map((record) => {
            const rowKey = getRecordKey(record);
            const meta = statusMetaMap[record.status] || statusMetaMap.draft;
            const active = String(rowKey) === String(selectedId);

            return (
              <button
                key={rowKey}
                type="button"
                className={`mobile-record-card ${active ? 'active' : ''}`}
                onClick={() => {
                  onSelectRecord(record);
                  onClose();
                }}
              >
                <div className="mobile-record-card-top">
                  <div>
                    <div className="mobile-record-title">{record.institution}</div>
                    <div className="mobile-record-subtitle">{record.city} · {record.type}</div>
                  </div>
                  <span className="badge" style={{ color: meta.color, background: meta.bg }}>{meta.label}</span>
                </div>
                <div className="mobile-record-meta">
                  <span>{record.system_district || 'No district'}</span>
                  <span>{record.public_records_email || 'No email'}</span>
                </div>
              </button>
            );
          })}
        </div>
      </div>
    </div>
  );
}