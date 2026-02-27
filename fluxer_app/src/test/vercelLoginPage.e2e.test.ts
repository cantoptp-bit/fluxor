/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * Smoke test: Vercel-deployed login page and config endpoint.
 * Run with: pnpm --filter fluxer_app exec vitest run src/test/vercelLoginPage.e2e.test.ts
 * Set VERCEL_BASE_URL to test a different deployment (default: https://fluxor-rust.vercel.app).
 * Uses Node environment so fetch can reach the live Vercel URL (no CORS in Node).
 */
// @vitest-environment node
import {describe, expect, test} from 'vitest';

const VERCEL_BASE = process.env.VERCEL_BASE_URL ?? 'https://fluxor-rust.vercel.app';

describe('Vercel login page (smoke)', () => {
	test('GET /login returns 200 and HTML with app shell', async () => {
		const res = await fetch(`${VERCEL_BASE}/login`, {
			redirect: 'follow',
			headers: {Accept: 'text/html'},
		});
		expect(res.status).toBe(200);
		expect(res.headers.get('content-type')).toMatch(/text\/html/);
		const html = await res.text();
		expect(html).toContain('<title>Fluxer</title>');
		expect(html).toContain('id="root"');
	});

	test('GET /api/fluxer-config returns 200 and JSON with backend config', async () => {
		const res = await fetch(`${VERCEL_BASE}/api/fluxer-config`, {
			redirect: 'follow',
			headers: {Accept: 'application/json'},
		});
		expect(res.status).toBe(200);
		const contentType = res.headers.get('content-type') ?? '';
		expect(contentType).toMatch(/application\/json/);
		const data = (await res.json()) as Record<string, unknown>;
		expect(data && typeof data === 'object').toBe(true);
		// Backend URL should be set (base_domain or api) so the app can connect
		const hasBackend =
			(typeof data?.base_domain === 'string' && data.base_domain.length > 0) ||
			(typeof data?.api === 'string' && data.api.length > 0);
		expect(hasBackend).toBe(true);
	});
});
