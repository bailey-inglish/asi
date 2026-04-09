export default function TemplateStudio({
  templateName,
  templateLabels,
  subject,
  templateKey,
  templateText,
  onTemplateKeyChange,
  onCopyTemplate,
  onCopySubject,
  onCopyBody,
  onClose,
}) {
  return (
    <div className="template-sheet">
      <div className="record-header record-header-tight">
        <div>
          <div className="section-kicker">Template studio</div>
          <h2 className="section-title" style={{ marginBottom: 4 }}>{templateLabels[templateName] || 'Template'}</h2>
          <p className="meta meta-wrap">{subject}</p>
        </div>
        {onClose ? <button className="button-secondary" type="button" onClick={onClose}>Close</button> : null}
      </div>

      <div className="template-tools">
        <label className="label">Template stage</label>
        <select className="select" value={templateKey} onChange={(event) => onTemplateKeyChange(event.target.value)}>
          {Object.keys(templateLabels).map((key) => <option key={key} value={key}>{templateLabels[key]}</option>)}
        </select>
      </div>

      <div className="record-actions">
        <button className="button" type="button" onClick={onCopyTemplate}>Copy template</button>
        <button className="button-secondary" type="button" onClick={onCopySubject}>Copy subject</button>
        <button className="button-secondary" type="button" onClick={onCopyBody}>Copy body</button>
      </div>

      <textarea className="textarea template-preview" readOnly value={templateText} onFocus={(event) => event.currentTarget.select()} />
    </div>
  );
}