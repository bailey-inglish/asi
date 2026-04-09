'use client';

import { useEffect, useMemo, useState } from 'react';
import TrackerApp from './TrackerApp';
import CountyTrackerApp from './CountyTrackerApp';

const TAB_DEFS = [
  { id: 'institutions', label: 'Institutions' },
  { id: 'counties', label: 'Counties' },
];

const MOBILE_SWITCHER_TABS = TAB_DEFS.filter((tab) => tab.id !== 'metrics');

export default function WorkspaceTabs() {
  const [activeTab, setActiveTab] = useState('institutions');
  const [preloadedInstitutionState, setPreloadedInstitutionState] = useState(null);
  const [preloadedCountyState, setPreloadedCountyState] = useState(null);
  const [showMobileScreenMenu, setShowMobileScreenMenu] = useState(false);

  useEffect(() => {
    let active = true;

    Promise.all([
      fetch('/api/state', { cache: 'no-store' }).then(async (response) => {
        const payload = await response.json();
        if (!response.ok) {
          throw new Error(payload?.error || 'Failed to preload institution data');
        }
        return payload;
      }),
      fetch('/api/county-state', { cache: 'no-store' }).then(async (response) => {
        const payload = await response.json();
        if (!response.ok) {
          throw new Error(payload?.error || 'Failed to preload county data');
        }
        return payload;
      }),
    ])
      .then(([institutionPayload, countyPayload]) => {
        if (!active) return;
        setPreloadedInstitutionState(institutionPayload);
        setPreloadedCountyState(countyPayload);
      })
      .catch(() => {
        // Keep lazy loading fallback inside each tab component.
      });

    return () => {
      active = false;
    };
  }, []);

  const activeContent = useMemo(() => {
    if (activeTab === 'counties') {
      return (
        <CountyTrackerApp
          preloadedState={preloadedCountyState}
          onOpenScreenMenu={() => setShowMobileScreenMenu(true)}
        />
      );
    }
    return <TrackerApp preloadedState={preloadedInstitutionState} onOpenScreenMenu={() => setShowMobileScreenMenu(true)} />;
  }, [activeTab, preloadedCountyState, preloadedInstitutionState]);

  return (
    <div className="tabs-shell">
      <div className="floating-tabs-dock">
        <div className="top-tabs" role="tablist" aria-label="Tracker sections">
          {TAB_DEFS.map((tab) => {
            const isActive = activeTab === tab.id;
            return (
              <button
                key={tab.id}
                type="button"
                role="tab"
                aria-selected={isActive}
                className={`top-tab ${isActive ? 'active' : ''}`}
                onClick={() => setActiveTab(tab.id)}
              >
                {tab.label}
              </button>
            );
          })}
        </div>
      </div>

      {showMobileScreenMenu ? (
        <div className="mobile-sheet-overlay" role="dialog" aria-modal="true" aria-label="Switch screens">
          <div className="mobile-sheet">
            <div className="mobile-sheet-header mobile-sheet-header-inline">
              <div>
                <div className="section-kicker">Page Switcher</div>
                <h2 className="section-title" style={{ marginBottom: 0 }}>Select screen</h2>
              </div>
              <button className="mobile-close-icon mobile-switcher-close" type="button" onClick={() => setShowMobileScreenMenu(false)} aria-label="Close switcher">
                ×
              </button>
            </div>
            <div className="profile-picker-grid" style={{ marginTop: 6 }}>
              {MOBILE_SWITCHER_TABS.map((tab) => {
                const isActive = activeTab === tab.id;
                return (
                  <button
                    key={tab.id}
                    className={isActive ? 'button' : 'button-secondary'}
                    type="button"
                    onClick={() => {
                      setActiveTab(tab.id);
                      setShowMobileScreenMenu(false);
                    }}
                  >
                    {tab.label}
                  </button>
                );
              })}
            </div>
          </div>
        </div>
      ) : null}

      {activeContent}
    </div>
  );
}
