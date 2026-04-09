import { NextResponse } from 'next/server';
import { loadSender, saveSender } from '../../../lib/data';

const SENDER_CACHE_TTL_MS = 60_000;
let senderCache = null;
let senderCacheAt = 0;

export async function GET() {
  const now = Date.now();
  if (senderCache && (now - senderCacheAt) < SENDER_CACHE_TTL_MS) {
    return NextResponse.json(senderCache);
  }

  const sender = await loadSender();
  senderCache = sender;
  senderCacheAt = now;
  return NextResponse.json(sender);
}

export async function PUT(request) {
  const payload = await request.json();
  const sender = {
    name: String(payload.name || ''),
    title: String(payload.title || ''),
    org: String(payload.org || ''),
    email: String(payload.email || ''),
    phone: String(payload.phone || ''),
    address: String(payload.address || ''),
  };
  await saveSender(sender);
  senderCache = sender;
  senderCacheAt = Date.now();
  return NextResponse.json({ ok: true, sender });
}
