export default function ActionBar({
  selectedPortal,
  selectedEmail,
  emailPrimary,
  onSendEmail,
  onCopyTemplate,
  onOpenPortal,
  onOpenNotes,
  onOpenTemplate,
}) {
  return (
    <div className="sticky-actions">
      {selectedPortal ? <a className="button" href={selectedPortal} target="_blank" rel="noreferrer" onClick={onOpenPortal}>Open portal</a> : null}
      {selectedEmail ? (
        <button className={emailPrimary ? 'button' : 'button-secondary'} type="button" onClick={onSendEmail}>
          Send email
        </button>
      ) : null}
      <button className="button-secondary" type="button" onClick={onOpenNotes}>Notes</button>
      <button className="button-secondary" type="button" onClick={onOpenTemplate}>View all templates</button>
      <button className="button-secondary" type="button" onClick={onCopyTemplate}>Copy template</button>
    </div>
  );
}