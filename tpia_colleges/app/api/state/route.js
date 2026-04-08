import { NextResponse } from 'next/server';
import { loadAppState } from '../../../lib/data';

export async function GET() {
  const state = await loadAppState();
  return NextResponse.json(state);
}
