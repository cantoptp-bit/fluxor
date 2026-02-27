/*
 * Copyright (C) 2026 Fluxer Contributors
 * Tests for Vercel/backend config parsing (backend-config.json, fluxer-config).
 */

import {describe, expect, test} from 'vitest';
import {
	isLocalhostOrigin,
	parseBackendConfigApiUrl,
	type BackendConfigData,
} from './backendConfigUtils';

describe('parseBackendConfigApiUrl', () => {
	test('returns api when present', () => {
		expect(
			parseBackendConfigApiUrl({api: 'https://ngrok.example.com/api', base_domain: 'ngrok.example.com'}),
		).toBe('https://ngrok.example.com/api');
		expect(parseBackendConfigApiUrl({api: 'https://x.com/api'})).toBe('https://x.com/api');
	});

	test('builds api from base_domain when api missing', () => {
		expect(parseBackendConfigApiUrl({base_domain: 'abc.ngrok-free.app'})).toBe(
			'https://abc.ngrok-free.app/api',
		);
		expect(parseBackendConfigApiUrl({base_domain: 'mybackend.com'})).toBe('https://mybackend.com/api');
	});

	test('accepts .well-known/fluxer shape (endpoints.api)', () => {
		expect(
			parseBackendConfigApiUrl({
				endpoints: { api: 'https://localhost:49319/api', api_client: 'https://localhost:49319/api' },
			}),
		).toBe('https://localhost:49319/api');
		expect(parseBackendConfigApiUrl({ endpoints: { api_client: 'https://x.com/api' } })).toBe(
			'https://x.com/api',
		);
	});

	test('returns null when empty or no usable data', () => {
		expect(parseBackendConfigApiUrl({})).toBeNull();
		expect(parseBackendConfigApiUrl({api: ''})).toBeNull();
		expect(parseBackendConfigApiUrl({base_domain: ''})).toBeNull();
		expect(parseBackendConfigApiUrl(null)).toBeNull();
		expect(parseBackendConfigApiUrl(undefined)).toBeNull();
	});

	test('trims and rejects whitespace-only api', () => {
		expect(parseBackendConfigApiUrl({api: '  '})).toBeNull();
	});
});

describe('isLocalhostOrigin', () => {
	test('returns true for localhost URLs', () => {
		expect(isLocalhostOrigin('http://localhost/api')).toBe(true);
		expect(isLocalhostOrigin('http://localhost:48763/api')).toBe(true);
		expect(isLocalhostOrigin('https://127.0.0.1/api')).toBe(true);
		expect(isLocalhostOrigin('https://127.0.0.1:8080/')).toBe(true);
	});

	test('returns false for non-localhost', () => {
		expect(isLocalhostOrigin('https://fluxor-rust.vercel.app')).toBe(false);
		expect(isLocalhostOrigin('https://abc.ngrok-free.app/api')).toBe(false);
		expect(isLocalhostOrigin('/api')).toBe(false);
	});

	test('returns false for invalid URLs', () => {
		expect(isLocalhostOrigin('')).toBe(false);
		expect(isLocalhostOrigin('not-a-url')).toBe(false);
	});
});
