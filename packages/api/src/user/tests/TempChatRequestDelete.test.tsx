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

describe('Temp chat request-delete (both parties must agree)', () => {
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

	test('first request returns deleted: false, second request returns deleted: true and chat is removed', async () => {
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
		expect(chat.participant_ids).toContain(alice.userId);
		expect(chat.participant_ids).toContain(bob.userId);

		const requestDeleteResponse = await createBuilder<{deleted: boolean}>(harness, alice.token)
			.post(`/users/@me/temp-chats/${chat.id}/request-delete`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(requestDeleteResponse.deleted).toBe(false);

		const listAfterFirst = await createBuilder<TempChatSummary[]>(harness, alice.token)
			.get('/users/@me/temp-chats')
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(listAfterFirst.some((c) => c.id === chat.id)).toBe(true);

		const secondRequestResponse = await createBuilder<{deleted: boolean}>(harness, bob.token)
			.post(`/users/@me/temp-chats/${chat.id}/request-delete`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(secondRequestResponse.deleted).toBe(true);

		const listAfterBoth = await createBuilder<TempChatSummary[]>(harness, alice.token)
			.get('/users/@me/temp-chats')
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(listAfterBoth.some((c) => c.id === chat.id)).toBe(false);
	});

	test('DELETE endpoint also requires both parties to agree', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);

		await sendFriendRequest(harness, alice.token, bob.userId);
		await acceptFriendRequest(harness, bob.token, alice.userId);

		const chat = await createBuilder<TempChatSummary>(harness, alice.token)
			.post('/users/@me/temp-chats')
			.body({recipient_id: bob.userId})
			.expect(HTTP_STATUS.OK)
			.execute();

		const firstDelete = await createBuilder<{requested?: boolean}>(harness, alice.token)
			.delete(`/users/@me/temp-chats/${chat.id}`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(firstDelete.requested).toBe(true);

		const listMid = await createBuilder<TempChatSummary[]>(harness, alice.token)
			.get('/users/@me/temp-chats')
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(listMid.some((c) => c.id === chat.id)).toBe(true);

		await createBuilder(harness, bob.token)
			.delete(`/users/@me/temp-chats/${chat.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		const listFinal = await createBuilder<TempChatSummary[]>(harness, alice.token)
			.get('/users/@me/temp-chats')
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(listFinal.some((c) => c.id === chat.id)).toBe(false);
	});

	test('non-participant cannot request delete', async () => {
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
			.post(`/users/@me/temp-chats/${chat.id}/request-delete`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();

		const list = await createBuilder<TempChatSummary[]>(harness, alice.token)
			.get('/users/@me/temp-chats')
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(list.some((c) => c.id === chat.id)).toBe(true);
	});
});
