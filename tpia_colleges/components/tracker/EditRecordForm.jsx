export default function EditRecordForm({
  recordDraft,
  currentStatus,
  selectedCounty,
  texasCounties,
  onFieldChange,
  onSave,
  recordDirty,
  onClose,
  closePillLabel = 'Close',
}) {
  const feeAmountValue = String(recordDraft.fee_amount || '0').trim();
  const verifiedValue = String(recordDraft.verified || '').toLowerCase() === 'yes' ? 'confirmed' : 'incomplete';
  const showFeeAmount = ['fee_pending', 'fee_paid'].includes(String(currentStatus || '').trim())
    || Number.parseFloat(feeAmountValue || '0') !== 0;

  return (
    <section className="workflow-section">
      <div className="section-toggle" role="presentation">
        <div>
          <h3 className="section-title">Edit record</h3>
          <p className="subtle">Maintain institution and contact metadata.</p>
        </div>
        <div className="toggle-meta">
          {recordDirty ? <span className="dirty-dot" /> : null}
          {onClose ? (
            <button className="button-secondary" type="button" onClick={onClose}>{closePillLabel}</button>
          ) : (
            <span className="badge">Open</span>
          )}
        </div>
      </div>

      <div className="accordion-body">
        <div className="form-grid narrow-form">
          <div><label className="label">Institution</label><input name="institution" className="field" value={recordDraft.institution} onChange={(event) => onFieldChange('institution', event.target.value)} /></div>
          <div><label className="label">Type</label><select name="type" className="select" value={recordDraft.type} onChange={(event) => onFieldChange('type', event.target.value)}><option value="4yr">4yr</option><option value="2yr">2yr</option></select></div>
          <div><label className="label">City</label><input name="city" className="field" value={recordDraft.city} onChange={(event) => onFieldChange('city', event.target.value)} /></div>
          <div><label className="label">System / District</label><input name="system_district" className="field" value={recordDraft.system_district} onChange={(event) => onFieldChange('system_district', event.target.value)} /></div>
          <div><label className="label">Verified</label><select name="verified" className="select" value={verifiedValue} onChange={(event) => onFieldChange('verified', event.target.value === 'confirmed' ? 'yes' : 'partial')}><option value="confirmed">Confirmed</option><option value="incomplete">Incomplete</option></select></div>
          <div><label className="label">County</label><select name="county" className="select" value={recordDraft.county || selectedCounty || ''} onChange={(event) => onFieldChange('county', event.target.value)}><option value="">Select a county</option>{texasCounties.map((county) => <option key={county} value={county}>{county}</option>)}</select></div>
          <div><label className="label">Email</label><input id="record-email" name="public_records_email" className="field" value={recordDraft.public_records_email} onChange={(event) => onFieldChange('public_records_email', event.target.value)} /></div>
          <div><label className="label">Portal</label><input name="public_records_portal" className="field" value={recordDraft.public_records_portal} onChange={(event) => onFieldChange('public_records_portal', event.target.value)} /></div>
          {showFeeAmount ? (
            <div><label className="label">Fee amount</label><input name="fee_amount" className="field" inputMode="decimal" type="number" min="0" step="0.01" value={feeAmountValue} onChange={(event) => onFieldChange('fee_amount', event.target.value)} /></div>
          ) : null}
          <div className="form-actions" style={{ gridColumn: '1 / -1' }}>
            <button className="button" type="button" onClick={onSave}>Save college record</button>
          </div>
        </div>
      </div>
    </section>
  );
}