/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * Ensures the compiled Lingui catalog includes strings used on the login/patch-notes UI
 * so we don't get "Uncompiled message detected!" in the console. Run `pnpm lingui:extract`
 * and `pnpm lingui:compile` when adding new Trans strings.
 */
import {messages as messagesEnUS} from '@app/locales/en-US/messages.mjs';
import {describe, expect, test} from 'vitest';

function catalogContains(catalog: Record<string, unknown>, text: string): boolean {
	for (const v of Object.values(catalog)) {
		if (typeof v === 'string' && v === text) return true;
		if (Array.isArray(v) && v.length > 0 && v[0] === text) return true;
	}
	return false;
}

describe('Lingui compiled catalog (en-US)', () => {
	test('contains AuthPatchNotes / changelog messages to avoid uncompiled warnings', () => {
		expect(catalogContains(messagesEnUS as Record<string, unknown>, 'Revamped UI design')).toBe(true);
		expect(catalogContains(messagesEnUS as Record<string, unknown>, 'Favorites modal')).toBe(true);
		expect(catalogContains(messagesEnUS as Record<string, unknown>, 'View all changes')).toBe(true);
		expect(catalogContains(messagesEnUS as Record<string, unknown>, 'Login reliability')).toBe(true);
	});
});
