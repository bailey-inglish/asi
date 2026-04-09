import { NextResponse } from 'next/server';
import { loadAppState } from '../../../lib/data';

export async function GET() {
  try {
    const state = await loadAppState();
    return NextResponse.json(state);
  } catch (error) {
    console.error('Error in GET /api/state:', error);
    return NextResponse.json(
      { error: error?.message || 'Failed to load application state.' },
      { status: 500 },
    );
  }
}
