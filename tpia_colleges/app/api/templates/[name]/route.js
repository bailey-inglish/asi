import { NextResponse } from 'next/server';
import fs from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const routeDir = path.dirname(fileURLToPath(import.meta.url));
const templatesDir = path.resolve(routeDir, '../../../../templates');

export async function GET(_request, { params }) {
  const { name } = await params;
  const templateName = String(name || '').trim();

  if (!/^[a-zA-Z0-9_-]+$/.test(templateName)) {
    return NextResponse.json({ error: 'Invalid template name' }, { status: 400 });
  }

  const filePath = path.join(templatesDir, `${templateName}.txt`);

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
