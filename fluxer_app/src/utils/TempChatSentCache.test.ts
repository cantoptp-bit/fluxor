/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';
import {
	getSentPlaintext,
	getSentPlaintextKey,
	processTempChatMessages,
	setSentPlaintext,
} from './TempChatSentCache';

describe('TempChatSentCache', () => {
	describe('getSentPlaintextKey', () => {
		test('returns stable key format for temp chat and message id', () => {
			expect(getSentPlaintextKey('user1_user2', 'msg-123')).toBe(
				'fluxer_temp_sent:user1_user2:msg-123',
			);
		});
	});

	describe('getSentPlaintext / setSentPlaintext', () => {
		const storage: Record<string, string> = {};

		beforeEach(() => {
			vi.stubGlobal('sessionStorage', {
				getItem: (key: string) => storage[key] ?? null,
				setItem: (key: string, value: string) => {
					storage[key] = value;
				},
				removeItem: (key: string) => {
					delete storage[key];
				},
				clear: () => {
					Object.keys(storage).forEach((k) => delete storage[k]);
				},
				length: 0,
				key: () => null,
			});
		});

		afterEach(() => {
			vi.unstubAllGlobals();
		});

		test('set then get returns the same text', () => {
			setSentPlaintext('chat_1', 'msg_1', 'Hello world');
			expect(getSentPlaintext('chat_1', 'msg_1')).toBe('Hello world');
		});

		test('get with no stored value returns null', () => {
			expect(getSentPlaintext('chat_1', 'msg_999')).toBe(null);
		});

		test('different chat or message id does not collide', () => {
			setSentPlaintext('chat_1', 'msg_1', 'Text 1');
			setSentPlaintext('chat_2', 'msg_1', 'Text 2');
			setSentPlaintext('chat_1', 'msg_2', 'Text 3');
			expect(getSentPlaintext('chat_1', 'msg_1')).toBe('Text 1');
			expect(getSentPlaintext('chat_2', 'msg_1')).toBe('Text 2');
			expect(getSentPlaintext('chat_1', 'msg_2')).toBe('Text 3');
		});
	});

	describe('processTempChatMessages (decrypt bug regression)', () => {
		const placeholder = '[Your message]';

		test('own message without cache uses placeholder and never calls decrypt (0 decrypt errors)', async () => {
			const decrypt = vi.fn().mockRejectedValue(new Error('decrypt would fail'));
			const raw = [
				{
					id: 'm1',
					sender_id: 'me',
					ciphertext: 'x',
					iv: 'y',
					ephemeral_public_key: 'z',
					created_at: '2026-01-01T00:00:00Z',
				},
			];
			const {messages, decryptErrorCount} = await processTempChatMessages(
				raw,
				'me',
				'chat_1',
				placeholder,
				decrypt,
			);
			expect(decryptErrorCount).toBe(0);
			expect(decrypt).not.toHaveBeenCalled();
			expect(messages).toHaveLength(1);
			expect(messages[0].text).toBe(placeholder);
			expect(messages[0].senderId).toBe('me');
		});

		test('own message with cache uses cached text and 0 decrypt errors', async () => {
			const storage: Record<string, string> = {};
			vi.stubGlobal('sessionStorage', {
				getItem: (key: string) => storage[key] ?? null,
				setItem: (key: string, value: string) => {
					storage[key] = value;
				},
				removeItem: () => {},
				clear: () => {},
				length: 0,
				key: () => null,
			});
			const key = getSentPlaintextKey('chat_1', 'm1');
			storage[key] = 'My secret message';
			const decrypt = vi.fn().mockRejectedValue(new Error('decrypt would fail'));
			const raw = [
				{
					id: 'm1',
					sender_id: 'me',
					ciphertext: 'x',
					iv: 'y',
					ephemeral_public_key: 'z',
					created_at: '2026-01-01T00:00:00Z',
				},
			];
			const {messages, decryptErrorCount} = await processTempChatMessages(
				raw,
				'me',
				'chat_1',
				placeholder,
				decrypt,
			);
			vi.unstubAllGlobals();
			expect(decryptErrorCount).toBe(0);
			expect(decrypt).not.toHaveBeenCalled();
			expect(messages).toHaveLength(1);
			expect(messages[0].text).toBe('My secret message');
		});

		test('other user message: decrypt success adds message and 0 errors', async () => {
			const decrypt = vi.fn().mockResolvedValue('Decrypted text');
			const raw = [
				{
					id: 'm1',
					sender_id: 'other',
					ciphertext: 'x',
					iv: 'y',
					ephemeral_public_key: 'z',
					created_at: '2026-01-01T00:00:00Z',
				},
			];
			const {messages, decryptErrorCount} = await processTempChatMessages(
				raw,
				'me',
				'chat_1',
				placeholder,
				decrypt,
			);
			expect(decryptErrorCount).toBe(0);
			expect(decrypt).toHaveBeenCalledTimes(1);
			expect(messages).toHaveLength(1);
			expect(messages[0].text).toBe('Decrypted text');
			expect(messages[0].senderId).toBe('other');
		});

		test('other user message: decrypt throws yields 1 decrypt error and message is skipped', async () => {
			const decrypt = vi.fn().mockRejectedValue(new Error('decrypt failed'));
			const raw = [
				{
					id: 'm1',
					sender_id: 'other',
					ciphertext: 'x',
					iv: 'y',
					ephemeral_public_key: 'z',
					created_at: '2026-01-01T00:00:00Z',
				},
			];
			const {messages, decryptErrorCount} = await processTempChatMessages(
				raw,
				'me',
				'chat_1',
				placeholder,
				decrypt,
			);
			expect(decryptErrorCount).toBe(1);
			expect(messages).toHaveLength(0);
		});

		test('mix: own (cached) + other (decrypt throws) = 1 error, own message has cached text', async () => {
			const storage: Record<string, string> = {};
			vi.stubGlobal('sessionStorage', {
				getItem: (key: string) => storage[key] ?? null,
				setItem: (key: string, value: string) => {
					storage[key] = value;
				},
				removeItem: () => {},
				clear: () => {},
				length: 0,
				key: () => null,
			});
			storage[getSentPlaintextKey('chat_1', 'm_own')] = 'I said hello';
			const decrypt = vi.fn().mockRejectedValue(new Error('decrypt failed'));
			const raw = [
				{
					id: 'm_own',
					sender_id: 'me',
					ciphertext: 'a',
					iv: 'b',
					ephemeral_public_key: 'c',
					created_at: '2026-01-01T00:00:00Z',
				},
				{
					id: 'm_other',
					sender_id: 'other',
					ciphertext: 'x',
					iv: 'y',
					ephemeral_public_key: 'z',
					created_at: '2026-01-01T00:00:01Z',
				},
			];
			const {messages, decryptErrorCount} = await processTempChatMessages(
				raw,
				'me',
				'chat_1',
				placeholder,
				decrypt,
			);
			vi.unstubAllGlobals();
			expect(decryptErrorCount).toBe(1);
			expect(decrypt).toHaveBeenCalledTimes(1);
			expect(messages).toHaveLength(1);
			expect(messages[0].id).toBe('m_own');
			expect(messages[0].text).toBe('I said hello');
		});
	});
});
