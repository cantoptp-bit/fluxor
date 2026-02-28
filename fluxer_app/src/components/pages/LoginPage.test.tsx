/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * Tests that the login page route and component exist and the login path is correct.
 */
import {Routes} from '@app/Routes';
import {describe, expect, test} from 'vitest';

describe('Login page', () => {
	test('LOGIN route is /login', () => {
		expect(Routes.LOGIN).toBe('/login');
	});

	test('inviteLogin builds path with /login suffix', () => {
		expect(Routes.inviteLogin('abc')).toBe('/invite/abc/login');
	});

	test('giftLogin builds path with /login suffix', () => {
		expect(Routes.giftLogin('xyz')).toBe('/gift/xyz/login');
	});
});
