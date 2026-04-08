import { NextResponse } from 'next/server';
import { loadSender, saveSender } from '../../../lib/data';

export async function GET() {
  return NextResponse.json(await loadSender());
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
  return NextResponse.json({ ok: true, sender });
}
