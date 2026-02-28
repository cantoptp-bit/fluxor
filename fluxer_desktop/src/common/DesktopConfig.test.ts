/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * Tests that getAppUrl never returns the deprecated tunnel URL (avoids Error 1033 when launching EXE).
 */

import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest';
import { getAppUrl, loadDesktopConfig } from './DesktopConfig';

vi.mock('electron-log', () => ({
	default: {
		info: vi.fn(),
		warn: vi.fn(),
		error: vi.fn(),
	},
}));

describe('DesktopConfig getAppUrl (EXE launch URL)', () => {
	const EXPECTED_APP_URL = 'https://fluxor-rust.vercel.app';
	const DEPRECATED_URL = 'https://home.auroraplayer.com';

	let tempDir: string;

	beforeEach(() => {
		tempDir = fs.mkdtempSync(path.join(os.tmpdir(), 'fluxer-desktop-test-'));
	});

	afterEach(() => {
		try {
			fs.rmSync(tempDir, { recursive: true, force: true });
		} catch {
			// ignore
		}
	});

	test('returns fluxor-rust.vercel.app when no settings file exists', () => {
		loadDesktopConfig(tempDir);
		expect(getAppUrl()).toBe(EXPECTED_APP_URL);
	});

	test('returns fluxor-rust.vercel.app when settings has app_url = deprecated tunnel (no Error 1033)', () => {
		fs.writeFileSync(
			path.join(tempDir, 'settings.json'),
			JSON.stringify({ app_url: DEPRECATED_URL }, null, 2),
			'utf-8',
		);
		loadDesktopConfig(tempDir);
		expect(getAppUrl()).toBe(EXPECTED_APP_URL);
	});

	test('returns fluxor-rust.vercel.app when settings has app_url = deprecated with path', () => {
		fs.writeFileSync(
			path.join(tempDir, 'settings.json'),
			JSON.stringify({ app_url: `${DEPRECATED_URL}/channels/@me` }, null, 2),
			'utf-8',
		);
		loadDesktopConfig(tempDir);
		expect(getAppUrl()).toBe(EXPECTED_APP_URL);
	});

	test('returns custom URL when settings has app_url = fluxor-rust.vercel.app', () => {
		fs.writeFileSync(
			path.join(tempDir, 'settings.json'),
			JSON.stringify({ app_url: EXPECTED_APP_URL }, null, 2),
			'utf-8',
		);
		loadDesktopConfig(tempDir);
		expect(getAppUrl()).toBe(EXPECTED_APP_URL);
	});

	test('returns other custom URL when set to a non-deprecated host', () => {
		const other = 'https://my-tunnel.ngrok-free.app';
		fs.writeFileSync(
			path.join(tempDir, 'settings.json'),
			JSON.stringify({ app_url: other }, null, 2),
			'utf-8',
		);
		loadDesktopConfig(tempDir);
		expect(getAppUrl()).toBe(other);
	});
});
