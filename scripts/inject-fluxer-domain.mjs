#!/usr/bin/env node
/**
 * Injects FLUXER_PUBLIC_DOMAIN into fluxer_app/dist/index.html at build time
 * so the Vercel-deployed app can reach the backend (e.g. ngrok) without relying
 * on the runtime /api/fluxer-config. Run after build:vercel when deploying to Vercel.
 */
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(__dirname, '..');
const indexPath = path.join(root, 'fluxer_app', 'dist', 'index.html');

const domain = process.env.FLUXER_PUBLIC_DOMAIN?.trim();
if (!domain) {
	process.exit(0);
}

if (!fs.existsSync(indexPath)) {
	console.warn('inject-fluxer-domain: index.html not found, skipping');
	process.exit(0);
}

let html = fs.readFileSync(indexPath, 'utf8');
const script = `<script>window.__FLUXER_PUBLIC_DOMAIN__="${domain.replace(/"/g, '\\"')}";</script>`;
if (html.includes('__FLUXER_PUBLIC_DOMAIN__')) {
	html = html.replace(/<script>window\.__FLUXER_PUBLIC_DOMAIN__="[^"]*";<\/script>/, script);
} else {
	html = html.replace('</head>', `${script}\n</head>`);
}
fs.writeFileSync(indexPath, html);
console.log('inject-fluxer-domain: injected FLUXER_PUBLIC_DOMAIN into index.html');
