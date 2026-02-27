/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * Tests that the desktop EXE loads the correct app URL (no Cloudflare tunnel error).
 */

import { describe, expect, test } from 'vitest';
import { CANARY_APP_URL, STABLE_APP_URL } from './Constants';

describe('Constants (EXE app URL)', () => {
	test('STABLE_APP_URL is fluxor-rust.vercel.app so EXE does not hit Cloudflare tunnel', () => {
		expect(STABLE_APP_URL).toBe('https://fluxor-rust.vercel.app');
	});

	test('CANARY_APP_URL is fluxor-rust.vercel.app', () => {
		expect(CANARY_APP_URL).toBe('https://fluxor-rust.vercel.app');
	});

	test('default URLs do not point at deprecated tunnel (home.auroraplayer.com)', () => {
		expect(STABLE_APP_URL).not.toContain('home.auroraplayer.com');
		expect(CANARY_APP_URL).not.toContain('home.auroraplayer.com');
	});
});
