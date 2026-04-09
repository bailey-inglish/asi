import { NextResponse } from 'next/server';
import { loadCountyState } from '../../../lib/countyData';

export async function GET() {
  try {
    const state = await loadCountyState();
    return NextResponse.json(state);
  } catch (error) {
    console.error('Error in GET /api/county-state:', error);
    return NextResponse.json(
      { error: error?.message || 'Failed to load county state.' },
      { status: 500 },
    );
  }
}
