import { NextResponse } from 'next/server';
import fs from 'node:fs/promises';
import path from 'node:path';

export async function GET(_request, { params }) {
  const { name } = await params;
  const filePath = path.join(process.cwd(), 'templates', `${name}.txt`);
  const text = await fs.readFile(filePath, 'utf8');
  return new NextResponse(text, { headers: { 'Content-Type': 'text/plain; charset=utf-8' } });
}
