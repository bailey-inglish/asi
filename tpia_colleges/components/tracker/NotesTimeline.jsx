export default function NotesTimeline({
  noteDraft,
  noteAuthor,
  noteEntries,
  timelineEntries,
  onChangeNote,
  onAddNote,
  onClose,
  closePillLabel = 'Close',
}) {
  const items = Array.isArray(timelineEntries) && timelineEntries.length ? timelineEntries : noteEntries;

  return (
    <section className="workflow-section">
      <div className="section-toggle" role="presentation">
        <div>
          <h3 className="section-title">Notes timeline</h3>
          <p className="subtle">Chronological notes attached to this institution.</p>
        </div>
        {onClose ? (
          <button className="button-secondary" type="button" onClick={onClose}>{closePillLabel}</button>
        ) : (
          <span className="badge">Open</span>
        )}
      </div>

      <div className="accordion-body">
        <div className="note-composer">
          <label className="label" htmlFor="note-draft">Add note</label>
          <textarea
            id="note-draft"
            className="textarea"
            value={noteDraft}
            onChange={(event) => onChangeNote(event.target.value)}
            placeholder={`Write a note that will be attributed to ${noteAuthor}`}
          />
          <div className="form-actions">
            <button className="button" type="button" onClick={onAddNote}>Add note</button>
            <span className="subtle">Saved as {noteAuthor}</span>
          </div>
        </div>
        <div className="timeline">
          {items.length
            ? items.map((entry, index) => (
              entry.kind === 'status' ? (
                <div className="timeline-status-divider" key={`${entry.at || entry.date || 'status'}-${index}`}>
                  <div className="timeline-status-line" aria-hidden="true" />
                  <div className="timeline-status-pill">
                    <strong>{entry.body}</strong>
                    <span>{entry.author ? `${entry.author} · ${entry.date || ''}` : entry.date || ''}</span>
                  </div>
                  <div className="timeline-status-line" aria-hidden="true" />
                </div>
              ) : (
                <div className="timeline-item" key={`${entry.date || 'note'}-${index}`}>
                  <div className="timeline-meta">
                    <strong>{entry.date || 'No date'}</strong>
                    {entry.author ? <span>{entry.author}</span> : null}
                  </div>
                  <div className="timeline-body">{entry.body}</div>
                </div>
              )
            ))
            : <div className="empty-state">No notes or status changes yet.</div>}
        </div>
      </div>
    </section>
  );
}