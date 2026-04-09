import { NextResponse } from 'next/server';
import fs from 'node:fs/promises';
import path from 'node:path';

const TEMPLATE_ROOT_CANDIDATES = [
  path.join(process.cwd(), 'templates'),
  path.join(process.cwd(), 'tpia_colleges', 'templates'),
];

async function resolveTemplatePath(templateName) {
  for (const rootDir of TEMPLATE_ROOT_CANDIDATES) {
    const candidate = path.join(rootDir, `${templateName}.txt`);
    try {
      await fs.access(candidate);
      return candidate;
    } catch {
      // Keep searching other candidate roots.
    }
  }
  return null;
}

export async function GET(_request, { params }) {
  const { name } = await params;
  const templateName = String(name || '').trim();

  if (!/^[a-zA-Z0-9_-]+$/.test(templateName)) {
    return NextResponse.json({ error: 'Invalid template name' }, { status: 400 });
  }

  const filePath = await resolveTemplatePath(templateName);
  if (!filePath) {
    return NextResponse.json({ error: `Template not found: ${templateName}` }, { status: 404 });
  }

  try {
    const text = await fs.readFile(filePath, 'utf8');
    return new NextResponse(text, { headers: { 'Content-Type': 'text/plain; charset=utf-8' } });
  } catch (error) {
    if (error && typeof error === 'object' && 'code' in error && error.code === 'ENOENT') {
      return NextResponse.json({ error: `Template not found: ${templateName}` }, { status: 404 });
    }

    return NextResponse.json({ error: 'Failed to load template' }, { status: 500 });
  }
}
