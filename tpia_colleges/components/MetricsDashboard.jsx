'use client';

import { useEffect, useMemo, useRef, useState } from 'react';
import * as d3 from 'd3';
import { STATUS_KEYS, STATUS_META } from './constants';

function parseStatusDates(value) {
  if (!value) return {};
  try {
    const parsed = typeof value === 'string' ? JSON.parse(value) : value;
    if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) return {};
    return parsed;
  } catch {
    return {};
  }
}

function getOverdueDeadline(record) {
  const status = String(record?.status || 'draft').trim();
  const statusDates = parseStatusDates(record?.status_dates);

  if (['ag_opinion_requested', 'ag_opinion_pending'].includes(status)) {
    return record?.deadline_ag_45day || (statusDates.ag_opinion_requested || '');
  }

  if (['sent', 'acknowledged', 'in_progress', 'fee_pending', 'fee_paid'].includes(status)) {
    return record?.deadline_10day || (statusDates.sent || '');
  }

  return '';
}

function isRecordOverdue(record) {
  const deadlineRaw = getOverdueDeadline(record);
  if (!deadlineRaw) return false;
  const parsedDeadline = new Date(deadlineRaw);
  if (Number.isNaN(parsedDeadline.getTime())) return false;

  const today = new Date();
  const todayUtc = new Date(Date.UTC(today.getUTCFullYear(), today.getUTCMonth(), today.getUTCDate()));
  const deadlineUtc = new Date(Date.UTC(parsedDeadline.getUTCFullYear(), parsedDeadline.getUTCMonth(), parsedDeadline.getUTCDate()));
  return deadlineUtc < todayUtc;
}

function daysSince(value) {
  if (!value) return null;
  const parsed = new Date(value);
  if (Number.isNaN(parsed.getTime())) return null;
  return Math.max(0, Math.floor((Date.now() - parsed.getTime()) / (24 * 60 * 60 * 1000)));
}

function noteCount(notes) {
  const parts = String(notes || '').split(' | ').map((entry) => entry.trim()).filter(Boolean);
  return parts.length;
}

function useD3(render, deps) {
  const ref = useRef(null);
  useEffect(() => {
    if (!ref.current) return;
    const root = d3.select(ref.current);
    root.selectAll('*').remove();
    render(root);
  }, deps); // eslint-disable-line react-hooks/exhaustive-deps
  return ref;
}

function FlowChart({ rows }) {
  const ref = useD3((root) => {
    const width = 760;
    const rowHeight = 34;
    const margin = { top: 16, right: 24, bottom: 34, left: 190 };
    const statuses = STATUS_KEYS;
    const height = margin.top + margin.bottom + statuses.length * rowHeight;

    const byStatus = statuses.map((status) => {
      const institutions = rows.institutions.filter((row) => String(row.status || 'draft') === status).length;
      const counties = rows.counties.filter((row) => String(row.status || 'draft') === status).length;
      return { status, institutions, counties };
    });

    const maxValue = d3.max(byStatus.flatMap((row) => [row.institutions, row.counties])) || 1;
    const x = d3.scaleLinear().domain([0, maxValue]).nice().range([margin.left, width - margin.right]);
    const y = d3.scaleBand().domain(statuses).range([margin.top, height - margin.bottom]).paddingInner(0.22);

    const svg = root.append('svg').attr('viewBox', `0 0 ${width} ${height}`).attr('class', 'metric-svg');

    svg.append('g')
      .attr('transform', `translate(0,${height - margin.bottom})`)
      .call(d3.axisBottom(x).ticks(6).tickSizeOuter(0));

    svg.append('g')
      .attr('transform', `translate(${margin.left},0)`)
      .call(d3.axisLeft(y).tickFormat((status) => STATUS_META[status]?.label || status).tickSize(0))
      .call((g) => g.select('.domain').remove());

    const group = svg.append('g');
    const halfBand = y.bandwidth() / 2;

    group.selectAll('rect.inst')
      .data(byStatus)
      .join('rect')
      .attr('x', x(0))
      .attr('y', (d) => y(d.status) + 2)
      .attr('width', (d) => Math.max(0, x(d.institutions) - x(0)))
      .attr('height', Math.max(4, halfBand - 4))
      .attr('rx', 6)
      .attr('fill', '#155eef');

    group.selectAll('rect.county')
      .data(byStatus)
      .join('rect')
      .attr('x', x(0))
      .attr('y', (d) => y(d.status) + halfBand + 2)
      .attr('width', (d) => Math.max(0, x(d.counties) - x(0)))
      .attr('height', Math.max(4, halfBand - 4))
      .attr('rx', 6)
      .attr('fill', '#0ea5a4');

    const legend = svg.append('g').attr('transform', `translate(${margin.left},${margin.top - 8})`);
    legend.append('circle').attr('r', 5).attr('fill', '#155eef');
    legend.append('text').attr('x', 10).attr('y', 4).text('Institutions').attr('fill', '#364152').attr('font-size', 12);
    legend.append('circle').attr('cx', 110).attr('r', 5).attr('fill', '#0ea5a4');
    legend.append('text').attr('x', 120).attr('y', 4).text('Counties').attr('fill', '#364152').attr('font-size', 12);
  }, [rows]);

  return <div ref={ref} />;
}

function DualMetricChart({ title, aLabel, bLabel, aValue, bValue, colorA = '#155eef', colorB = '#0ea5a4', suffix = '' }) {
  const ref = useD3((root) => {
    const width = 360;
    const height = 220;
    const margin = { top: 22, right: 18, bottom: 44, left: 52 };

    const data = [
      { key: aLabel, value: aValue, color: colorA },
      { key: bLabel, value: bValue, color: colorB },
    ];

    const maxValue = Math.max(1, d3.max(data, (d) => d.value) || 1);
    const x = d3.scaleBand().domain(data.map((d) => d.key)).range([margin.left, width - margin.right]).padding(0.32);
    const y = d3.scaleLinear().domain([0, maxValue]).nice().range([height - margin.bottom, margin.top]);

    const svg = root.append('svg').attr('viewBox', `0 0 ${width} ${height}`).attr('class', 'metric-svg');

    svg.append('g')
      .attr('transform', `translate(0,${height - margin.bottom})`)
      .call(d3.axisBottom(x).tickSizeOuter(0));

    svg.append('g')
      .attr('transform', `translate(${margin.left},0)`)
      .call(d3.axisLeft(y).ticks(4).tickSizeOuter(0));

    svg.selectAll('rect')
      .data(data)
      .join('rect')
      .attr('x', (d) => x(d.key))
      .attr('y', (d) => y(d.value))
      .attr('width', x.bandwidth())
      .attr('height', (d) => Math.max(0, y(0) - y(d.value)))
      .attr('rx', 8)
      .attr('fill', (d) => d.color);

    svg.selectAll('text.value')
      .data(data)
      .join('text')
      .attr('class', 'value')
      .attr('x', (d) => x(d.key) + x.bandwidth() / 2)
      .attr('y', (d) => y(d.value) - 8)
      .attr('text-anchor', 'middle')
      .attr('font-size', 12)
      .attr('fill', '#172033')
      .text((d) => `${d.value}${suffix}`);
  }, [aLabel, bLabel, aValue, bValue, colorA, colorB, suffix]);

  return (
    <div className="metric-card panel">
      <h3 className="section-title" style={{ marginBottom: 6 }}>{title}</h3>
      <div ref={ref} />
    </div>
  );
}

function GroupedCategoryChart({ title, categories, institutions, counties }) {
  const ref = useD3((root) => {
    const width = 760;
    const height = 260;
    const margin = { top: 24, right: 18, bottom: 52, left: 52 };

    const data = categories.flatMap((category) => [
      { category, type: 'Institutions', value: institutions[category] || 0, color: '#155eef' },
      { category, type: 'Counties', value: counties[category] || 0, color: '#0ea5a4' },
    ]);

    const x0 = d3.scaleBand().domain(categories).range([margin.left, width - margin.right]).paddingInner(0.26);
    const x1 = d3.scaleBand().domain(['Institutions', 'Counties']).range([0, x0.bandwidth()]).padding(0.18);
    const y = d3.scaleLinear().domain([0, d3.max(data, (d) => d.value) || 1]).nice().range([height - margin.bottom, margin.top]);

    const svg = root.append('svg').attr('viewBox', `0 0 ${width} ${height}`).attr('class', 'metric-svg');

    svg.append('g')
      .attr('transform', `translate(0,${height - margin.bottom})`)
      .call(d3.axisBottom(x0).tickSizeOuter(0));

    svg.append('g')
      .attr('transform', `translate(${margin.left},0)`)
      .call(d3.axisLeft(y).ticks(5).tickSizeOuter(0));

    const groups = svg.selectAll('g.group').data(categories).join('g').attr('class', 'group').attr('transform', (d) => `translate(${x0(d)},0)`);

    groups.selectAll('rect')
      .data((category) => data.filter((entry) => entry.category === category))
      .join('rect')
      .attr('x', (d) => x1(d.type))
      .attr('y', (d) => y(d.value))
      .attr('width', x1.bandwidth())
      .attr('height', (d) => Math.max(0, y(0) - y(d.value)))
      .attr('rx', 7)
      .attr('fill', (d) => d.color);
  }, [categories, institutions, counties]);

  return (
    <div className="metric-card panel">
      <h3 className="section-title" style={{ marginBottom: 6 }}>{title}</h3>
      <div ref={ref} />
    </div>
  );
}

export default function MetricsDashboard() {
  const [collegeState, setCollegeState] = useState(null);
  const [countyState, setCountyState] = useState(null);
  const [error, setError] = useState('');

  useEffect(() => {
    Promise.all([
      fetch('/api/state', { cache: 'no-store' }).then(async (response) => {
        const payload = await response.json();
        if (!response.ok) throw new Error(payload?.error || 'Failed to load institution state');
        return payload;
      }),
      fetch('/api/county-state', { cache: 'no-store' }).then(async (response) => {
        const payload = await response.json();
        if (!response.ok) throw new Error(payload?.error || 'Failed to load county state');
        return payload;
      }),
    ])
      .then(([collegePayload, countyPayload]) => {
        setCollegeState(collegePayload);
        setCountyState(countyPayload);
      })
      .catch((loadError) => setError(loadError.message));
  }, []);

  const metrics = useMemo(() => {
    const institutions = collegeState?.records || [];
    const counties = countyState?.selectableCountyRecords || [];

    const completionStatuses = new Set(['complete', 'partially_complete']);
    const activeStatuses = new Set(['sent', 'acknowledged', 'in_progress', 'fee_pending', 'fee_paid', 'ag_opinion_requested', 'ag_opinion_pending']);

    const totalInstitutions = institutions.length;
    const totalCounties = counties.length;

    const completionRateInstitutions = totalInstitutions ? Math.round((institutions.filter((row) => completionStatuses.has(String(row.status || 'draft'))).length / totalInstitutions) * 100) : 0;
    const completionRateCounties = totalCounties ? Math.round((counties.filter((row) => completionStatuses.has(String(row.status || 'draft'))).length / totalCounties) * 100) : 0;

    const overdueInstitutions = institutions.filter(isRecordOverdue).length;
    const overdueCounties = counties.filter(isRecordOverdue).length;

    const avgOpenInstitutionDays = Math.round(
      d3.mean(
        institutions
          .filter((row) => activeStatuses.has(String(row.status || 'draft')))
          .map((row) => daysSince(row.date_sent || row.status_dates?.sent))
          .filter((value) => value != null),
      ) || 0,
    );

    const avgOpenCountyDays = Math.round(
      d3.mean(
        counties
          .filter((row) => activeStatuses.has(String(row.status || 'draft')))
          .map((row) => daysSince(row.date_sent || row.status_dates?.sent))
          .filter((value) => value != null),
      ) || 0,
    );

    const updatedWithin7Institutions = institutions.filter((row) => {
      const days = daysSince(row.last_updated);
      return days != null && days <= 7;
    }).length;

    const updatedWithin7Counties = counties.filter((row) => {
      const days = daysSince(row.last_updated);
      return days != null && days <= 7;
    }).length;

    const avgNotesInstitutions = Number((d3.mean(institutions.map((row) => noteCount(row.notes))) || 0).toFixed(2));
    const avgNotesCounties = Number((d3.mean(counties.map((row) => noteCount(row.notes))) || 0).toFixed(2));

    const verificationCategories = ['yes', 'partial', 'no'];
    const verificationInstitution = Object.fromEntries(verificationCategories.map((key) => [key, 0]));
    const verificationCounty = Object.fromEntries(verificationCategories.map((key) => [key, 0]));

    for (const row of institutions) {
      const key = String(row.verified || 'partial').toLowerCase();
      if (verificationInstitution[key] != null) verificationInstitution[key] += 1;
    }

    for (const row of counties) {
      const key = String(row.verified || 'partial').toLowerCase();
      if (verificationCounty[key] != null) verificationCounty[key] += 1;
    }

    const contactCategories = ['email_only', 'portal_only', 'both', 'none'];
    const contactInstitution = Object.fromEntries(contactCategories.map((key) => [key, 0]));
    const contactCounty = Object.fromEntries(contactCategories.map((key) => [key, 0]));

    const classifyContact = (emailValue, portalValue) => {
      const hasEmail = String(emailValue || '').trim().length > 0;
      const hasPortal = String(portalValue || '').trim().length > 0;
      if (hasEmail && hasPortal) return 'both';
      if (hasEmail) return 'email_only';
      if (hasPortal) return 'portal_only';
      return 'none';
    };

    for (const row of institutions) {
      contactInstitution[classifyContact(row.public_records_email, row.public_records_portal)] += 1;
    }

    for (const row of counties) {
      contactCounty[classifyContact(row.email, row.portal)] += 1;
    }

    return {
      rows: { institutions, counties },
      completionRateInstitutions,
      completionRateCounties,
      overdueInstitutions,
      overdueCounties,
      avgOpenInstitutionDays,
      avgOpenCountyDays,
      updatedWithin7Institutions,
      updatedWithin7Counties,
      avgNotesInstitutions,
      avgNotesCounties,
      verificationInstitution,
      verificationCounty,
      contactInstitution,
      contactCounty,
    };
  }, [collegeState, countyState]);

  if (error) {
    return (
      <div className="workspace">
        <section className="hero-card panel">
          <h1 className="page-title">Metrics</h1>
          <p className="helper">{error}</p>
        </section>
      </div>
    );
  }

  if (!collegeState || !countyState) {
    return (
      <div className="workspace">
        <section className="hero-card panel">
          <h1 className="page-title">Loading metrics...</h1>
          <p className="helper">Crunching request statistics across institution and county workflows.</p>
        </section>
      </div>
    );
  }

  return (
    <div className="workspace metrics-layout">
      <section className="hero-card panel compact-hero">
        <h1 className="page-title">Metrics Dashboard</h1>
        <p className="helper" style={{ marginTop: 6 }}>D3 visualizations across institution and county request pipelines.</p>
      </section>

      <div className="metric-card panel" style={{ marginTop: 12 }}>
        <h2 className="section-title" style={{ marginBottom: 8 }}>Flow Map by Status Stage</h2>
        <FlowChart rows={metrics.rows} />
      </div>

      <div className="metrics-grid-two" style={{ marginTop: 12 }}>
        <DualMetricChart
          title="Completion Rate"
          aLabel="Institutions"
          bLabel="Counties"
          aValue={metrics.completionRateInstitutions}
          bValue={metrics.completionRateCounties}
          suffix="%"
        />
        <DualMetricChart
          title="Overdue Requests"
          aLabel="Institutions"
          bLabel="Counties"
          aValue={metrics.overdueInstitutions}
          bValue={metrics.overdueCounties}
        />
        <DualMetricChart
          title="Avg Open Days (Active)"
          aLabel="Institutions"
          bLabel="Counties"
          aValue={metrics.avgOpenInstitutionDays}
          bValue={metrics.avgOpenCountyDays}
        />
        <DualMetricChart
          title="Updated in Last 7 Days"
          aLabel="Institutions"
          bLabel="Counties"
          aValue={metrics.updatedWithin7Institutions}
          bValue={metrics.updatedWithin7Counties}
        />
        <DualMetricChart
          title="Average Notes per Request"
          aLabel="Institutions"
          bLabel="Counties"
          aValue={metrics.avgNotesInstitutions}
          bValue={metrics.avgNotesCounties}
        />
      </div>

      <div style={{ marginTop: 12 }}>
        <GroupedCategoryChart
          title="Verification Quality Distribution"
          categories={['yes', 'partial', 'no']}
          institutions={metrics.verificationInstitution}
          counties={metrics.verificationCounty}
        />
      </div>

      <div style={{ marginTop: 12, marginBottom: 20 }}>
        <GroupedCategoryChart
          title="Contact Channel Coverage"
          categories={['email_only', 'portal_only', 'both', 'none']}
          institutions={metrics.contactInstitution}
          counties={metrics.contactCounty}
        />
      </div>
    </div>
  );
}
