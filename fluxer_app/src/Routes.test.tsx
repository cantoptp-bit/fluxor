/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License or (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Routes} from '@app/Routes';
import {describe, expect, test} from 'vitest';

describe('Routes (temp chat)', () => {
	describe('tempChat', () => {
		test('returns path with temp segment and id', () => {
			expect(Routes.tempChat('123_456')).toBe('/channels/@me/temp/123_456');
		});
	});

	describe('isTempChatRoute', () => {
		test('returns true for /channels/@me/temp/:id', () => {
			expect(Routes.isTempChatRoute('/channels/@me/temp/123_456')).toBe(true);
		});
		test('returns false for /channels/@me (no temp)', () => {
			expect(Routes.isTempChatRoute('/channels/@me')).toBe(false);
		});
		test('returns false for /channels/@me/123 (DM channel)', () => {
			expect(Routes.isTempChatRoute('/channels/@me/123')).toBe(false);
		});
	});
});
