/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {createTestAccount} from '@fluxer/api/src/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {createBuilder} from '@fluxer/api/src/test/TestRequestBuilder';
import {HTTP_STATUS} from '@fluxer/api/src/test/TestConstants';
import {acceptFriendRequest, sendFriendRequest} from '@fluxer/api/src/user/tests/RelationshipTestUtils';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

interface TempChatSummary {
	id: string;
	participant_ids: [string, string];
	created_at: string;
}

interface TempChatMessageResponse {
	id: string;
	sender_id: string;
	ciphertext: string;
	iv: string;
	ephemeral_public_key: string;
	created_at: string;
}

describe('Temp chat send message', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	test('participant can send a message and it appears in list', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);

		await sendFriendRequest(harness, alice.token, bob.userId);
		await acceptFriendRequest(harness, bob.token, alice.userId);

		const chat = await createBuilder<TempChatSummary>(harness, alice.token)
			.post('/users/@me/temp-chats')
			.body({recipient_id: bob.userId})
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(chat.id).toBeDefined();

		const sendPayload = {
			ciphertext: 'base64encryptedpayload',
			iv: 'base64iv',
			ephemeral_public_key: 'base64ephemeralpubkey',
		};

		const sendRes = await createBuilder<{id: string; created_at: string}>(harness, alice.token)
			.post(`/users/@me/temp-chats/${chat.id}/messages`)
			.body(sendPayload)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(sendRes.id).toBeDefined();
		expect(typeof sendRes.created_at).toBe('string');

		const messages = await createBuilder<TempChatMessageResponse[]>(harness, alice.token)
			.get(`/users/@me/temp-chats/${chat.id}/messages`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(messages).toHaveLength(1);
		expect(messages[0].id).toBe(sendRes.id);
		expect(messages[0].sender_id).toBe(alice.userId);
		expect(messages[0].ciphertext).toBe(sendPayload.ciphertext);
		expect(messages[0].iv).toBe(sendPayload.iv);
		expect(messages[0].ephemeral_public_key).toBe(sendPayload.ephemeral_public_key);
	});

	test('second participant can send and both see two messages', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);

		await sendFriendRequest(harness, alice.token, bob.userId);
		await acceptFriendRequest(harness, bob.token, alice.userId);

		const chat = await createBuilder<TempChatSummary>(harness, alice.token)
			.post('/users/@me/temp-chats')
			.body({recipient_id: bob.userId})
			.expect(HTTP_STATUS.OK)
			.execute();

		await createBuilder(harness, alice.token)
			.post(`/users/@me/temp-chats/${chat.id}/messages`)
			.body({
				ciphertext: 'alice_msg',
				iv: 'iv1',
				ephemeral_public_key: 'key1',
			})
			.expect(HTTP_STATUS.OK)
			.execute();

		const bobSend = await createBuilder<{id: string; created_at: string}>(harness, bob.token)
			.post(`/users/@me/temp-chats/${chat.id}/messages`)
			.body({
				ciphertext: 'bob_msg',
				iv: 'iv2',
				ephemeral_public_key: 'key2',
			})
			.expect(HTTP_STATUS.OK)
			.execute();

		const aliceMessages = await createBuilder<TempChatMessageResponse[]>(harness, alice.token)
			.get(`/users/@me/temp-chats/${chat.id}/messages`)
			.expect(HTTP_STATUS.OK)
			.execute();

		const bobMessages = await createBuilder<TempChatMessageResponse[]>(harness, bob.token)
			.get(`/users/@me/temp-chats/${chat.id}/messages`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(aliceMessages).toHaveLength(2);
		expect(bobMessages).toHaveLength(2);

		const bobMsg = aliceMessages.find((m) => m.id === bobSend.id);
		expect(bobMsg).toBeDefined();
		expect(bobMsg?.sender_id).toBe(bob.userId);
		expect(bobMsg?.ciphertext).toBe('bob_msg');
	});

	test('non-participant cannot send message', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);
		const eve = await createTestAccount(harness);

		await sendFriendRequest(harness, alice.token, bob.userId);
		await acceptFriendRequest(harness, bob.token, alice.userId);

		const chat = await createBuilder<TempChatSummary>(harness, alice.token)
			.post('/users/@me/temp-chats')
			.body({recipient_id: bob.userId})
			.expect(HTTP_STATUS.OK)
			.execute();

		await createBuilder(harness, eve.token)
			.post(`/users/@me/temp-chats/${chat.id}/messages`)
			.body({
				ciphertext: 'evil',
				iv: 'iv',
				ephemeral_public_key: 'key',
			})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();

		const messages = await createBuilder<TempChatMessageResponse[]>(harness, alice.token)
			.get(`/users/@me/temp-chats/${chat.id}/messages`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(messages).toHaveLength(0);
	});

	test('sending multiple messages in sequence returns correct count and order', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);

		await sendFriendRequest(harness, alice.token, bob.userId);
		await acceptFriendRequest(harness, bob.token, alice.userId);

		const chat = await createBuilder<TempChatSummary>(harness, alice.token)
			.post('/users/@me/temp-chats')
			.body({recipient_id: bob.userId})
			.expect(HTTP_STATUS.OK)
			.execute();

		const payload = {
			ciphertext: 'enc',
			iv: 'iv',
			ephemeral_public_key: 'key',
		};
		const sentIds: string[] = [];
		for (let i = 0; i < 5; i++) {
			const res = await createBuilder<{id: string; created_at: string}>(harness, alice.token)
				.post(`/users/@me/temp-chats/${chat.id}/messages`)
				.body(payload)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(res.id).toBeDefined();
			expect(res.created_at).toBeDefined();
			sentIds.push(res.id);
		}

		const messages = await createBuilder<TempChatMessageResponse[]>(harness, alice.token)
			.get(`/users/@me/temp-chats/${chat.id}/messages`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(messages).toHaveLength(5);
		const messageIds = messages.map((m) => m.id);
		expect(messageIds).toEqual(sentIds);
		for (let i = 1; i < messages.length; i++) {
			const prev = new Date(messages[i - 1].created_at).getTime();
			const curr = new Date(messages[i].created_at).getTime();
			expect(curr).toBeGreaterThanOrEqual(prev);
		}
	});

	test('sending to non-existent temp chat returns 404', async () => {
		const alice = await createTestAccount(harness);

		await createBuilder(harness, alice.token)
			.post('/users/@me/temp-chats/999999999999999999/messages')
			.body({
				ciphertext: 'x',
				iv: 'y',
				ephemeral_public_key: 'z',
			})
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});

	test('sending with missing ciphertext returns 400', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);

		await sendFriendRequest(harness, alice.token, bob.userId);
		await acceptFriendRequest(harness, bob.token, alice.userId);

		const chat = await createBuilder<TempChatSummary>(harness, alice.token)
			.post('/users/@me/temp-chats')
			.body({recipient_id: bob.userId})
			.expect(HTTP_STATUS.OK)
			.execute();

		await createBuilder(harness, alice.token)
			.post(`/users/@me/temp-chats/${chat.id}/messages`)
			.body({
				iv: 'iv',
				ephemeral_public_key: 'key',
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});

	test('sending with empty body returns 400', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);

		await sendFriendRequest(harness, alice.token, bob.userId);
		await acceptFriendRequest(harness, bob.token, alice.userId);

		const chat = await createBuilder<TempChatSummary>(harness, alice.token)
			.post('/users/@me/temp-chats')
			.body({recipient_id: bob.userId})
			.expect(HTTP_STATUS.OK)
			.execute();

		await createBuilder(harness, alice.token)
			.post(`/users/@me/temp-chats/${chat.id}/messages`)
			.body({})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});

	test('both participants can send and fetch messages in alternation', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);

		await sendFriendRequest(harness, alice.token, bob.userId);
		await acceptFriendRequest(harness, bob.token, alice.userId);

		const chat = await createBuilder<TempChatSummary>(harness, alice.token)
			.post('/users/@me/temp-chats')
			.body({recipient_id: bob.userId})
			.expect(HTTP_STATUS.OK)
			.execute();

		const base = {ciphertext: 'c', iv: 'i', ephemeral_public_key: 'k'};
		await createBuilder(harness, alice.token)
			.post(`/users/@me/temp-chats/${chat.id}/messages`)
			.body({...base, ciphertext: 'alice-1'})
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, bob.token)
			.post(`/users/@me/temp-chats/${chat.id}/messages`)
			.body({...base, ciphertext: 'bob-1'})
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, alice.token)
			.post(`/users/@me/temp-chats/${chat.id}/messages`)
			.body({...base, ciphertext: 'alice-2'})
			.expect(HTTP_STATUS.OK)
			.execute();

		const aliceView = await createBuilder<TempChatMessageResponse[]>(harness, alice.token)
			.get(`/users/@me/temp-chats/${chat.id}/messages`)
			.expect(HTTP_STATUS.OK)
			.execute();
		const bobView = await createBuilder<TempChatMessageResponse[]>(harness, bob.token)
			.get(`/users/@me/temp-chats/${chat.id}/messages`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(aliceView).toHaveLength(3);
		expect(bobView).toHaveLength(3);
		expect(aliceView.map((m) => m.sender_id)).toEqual([alice.userId, bob.userId, alice.userId]);
		expect(aliceView[0].ciphertext).toBe('alice-1');
		expect(aliceView[1].ciphertext).toBe('bob-1');
		expect(aliceView[2].ciphertext).toBe('alice-2');
	});
});
