/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * Extensive smoke tests: key pages must load on Vercel and (optionally) localhost.
 * Run: pnpm --filter fluxer_app exec vitest run src/test/pageLoad.e2e.test.ts
 * - Vercel: default base https://fluxor-rust.vercel.app (set VERCEL_BASE_URL to override).
 * - Localhost: run only when LOCALHOST_BASE_URL is set (e.g. http://localhost:48763).
 */
// @vitest-environment node
import { describe, expect, test } from 'vitest';

const VERCEL_BASE = process.env.VERCEL_BASE_URL ?? 'https://fluxor-rust.vercel.app';
const LOCALHOST_BASE = process.env.LOCALHOST_BASE_URL; // e.g. http://localhost:48763

function expectAppShell(html: string) {
	expect(html).toContain('<title>Pegasus</title>');
	expect(html).toContain('id="root"');
}

function expectNoCloudflareError(html: string) {
	expect(html).not.toContain('Error 1033');
	expect(html).not.toContain('Cloudflare Tunnel error');
	expect(html).not.toContain('home.auroraplayer.com');
}

const APP_PAGES = [
	{ path: '/', name: 'home' },
	{ path: '/login', name: 'login' },
	{ path: '/register', name: 'register' },
	{ path: '/download', name: 'download' },
] as const;

describe('Vercel – app pages return 200 and HTML shell', () => {
	for (const { path, name } of APP_PAGES) {
		test(`GET ${path} (${name})`, async () => {
			const res = await fetch(`${VERCEL_BASE}${path}`, {
				redirect: 'follow',
				headers: { Accept: 'text/html' },
			});
			expect(res.status, `${path} status`).toBe(200);
			expect(res.headers.get('content-type')).toMatch(/text\/html/);
			const html = await res.text();
			expectAppShell(html);
			expectNoCloudflareError(html);
		});
	}
});

describe('Vercel – app pages are not Cloudflare/error pages', () => {
	for (const { path } of APP_PAGES) {
		test(`${path} does not show Error 1033 or tunnel error`, async () => {
			const res = await fetch(`${VERCEL_BASE}${path}`, {
				redirect: 'follow',
				headers: { Accept: 'text/html' },
			});
			expect(res.status).toBe(200);
			expectNoCloudflareError(await res.text());
		});
	}
});

describe('Vercel – config endpoints', () => {
	test('GET /api/fluxer-config returns 200 and JSON with backend config', async () => {
		const res = await fetch(`${VERCEL_BASE}/api/fluxer-config`, {
			redirect: 'follow',
			headers: { Accept: 'application/json' },
		});
		expect(res.status).toBe(200);
		const contentType = res.headers.get('content-type') ?? '';
		expect(contentType).toMatch(/application\/json/);
		const data = (await res.json()) as Record<string, unknown>;
		expect(data && typeof data === 'object').toBe(true);
		const hasBackend =
			(typeof data?.base_domain === 'string' && data.base_domain.length > 0) ||
			(typeof data?.api === 'string' && data.api.length > 0);
		expect(hasBackend, 'config must have base_domain or api').toBe(true);
	});

	test('GET /.well-known/fluxer returns 200', async () => {
		const res = await fetch(`${VERCEL_BASE}/.well-known/fluxer`, {
			redirect: 'follow',
			headers: { Accept: 'application/json' },
		});
		expect(res.status).toBe(200);
	});
});

describe('Vercel – channels route serves app shell (SPA)', () => {
	test('GET /channels/@me returns 200 and app shell', async () => {
		const res = await fetch(`${VERCEL_BASE}/channels/@me`, {
			redirect: 'follow',
			headers: { Accept: 'text/html' },
		});
		expect(res.status).toBe(200);
		expect(res.headers.get('content-type')).toMatch(/text\/html/);
		const html = await res.text();
		expectAppShell(html);
		expectNoCloudflareError(html);
	});
});

// Localhost tests: only when LOCALHOST_BASE_URL is set (dev server assumed running)
describe.skipIf(!LOCALHOST_BASE)('Localhost – app pages load', () => {
	for (const { path, name } of APP_PAGES) {
		test(`GET ${path} (${name})`, async () => {
			const res = await fetch(`${LOCALHOST_BASE}${path}`, {
				redirect: 'follow',
				headers: { Accept: 'text/html' },
			});
			expect(res.status, `${path} status`).toBe(200);
			expect(res.headers.get('content-type')).toMatch(/text\/html/);
			const html = await res.text();
			expectAppShell(html);
		});
	}
});

describe.skipIf(!LOCALHOST_BASE)('Localhost – config endpoints', () => {
	test('GET /api/fluxer-config returns 200 and JSON with backend config', async () => {
		const res = await fetch(`${LOCALHOST_BASE}/api/fluxer-config`, {
			redirect: 'follow',
			headers: { Accept: 'application/json' },
		});
		expect(res.status).toBe(200);
		const contentType = res.headers.get('content-type') ?? '';
		expect(contentType).toMatch(/application\/json/);
		const data = (await res.json()) as Record<string, unknown> & { endpoints?: { api?: string } };
		expect(data && typeof data === 'object').toBe(true);
		const hasBackend =
			(typeof data?.base_domain === 'string' && data.base_domain.length > 0) ||
			(typeof data?.api === 'string' && data.api.length > 0) ||
			(typeof data?.endpoints?.api === 'string' && data.endpoints.api.length > 0);
		expect(hasBackend, 'config must have base_domain, api, or endpoints.api').toBe(true);
	});

	test('GET /.well-known/fluxer returns 200', async () => {
		const res = await fetch(`${LOCALHOST_BASE}/.well-known/fluxer`, {
			redirect: 'follow',
			headers: { Accept: 'application/json' },
		});
		expect(res.status).toBe(200);
	});
});

describe.skipIf(!LOCALHOST_BASE)('Localhost – channels route', () => {
	test('GET /channels/@me returns 200 and app shell', async () => {
		const res = await fetch(`${LOCALHOST_BASE}/channels/@me`, {
			redirect: 'follow',
			headers: { Accept: 'text/html' },
		});
		expect(res.status).toBe(200);
		expect(res.headers.get('content-type')).toMatch(/text\/html/);
		const html = await res.text();
		expectAppShell(html);
	});
});
