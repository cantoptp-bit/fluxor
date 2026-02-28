/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * This source code is licensed under the GNU Affero General Public License v3.0
 * found in the LICENSE file in the root directory of this source tree.
 */

import {createTestAccount} from '@fluxer/api/src/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {createBuilder} from '@fluxer/api/src/test/TestRequestBuilder';
import {HTTP_STATUS} from '@fluxer/api/src/test/TestConstants';
import {acceptFriendRequest, sendFriendRequest} from '@fluxer/api/src/user/tests/RelationshipTestUtils';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('User E2E key', () => {
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

	test('GET other user e2e-key when they have no key returns 404', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);

		await sendFriendRequest(harness, alice.token, bob.userId);
		await acceptFriendRequest(harness, bob.token, alice.userId);

		await createBuilder(harness, alice.token)
			.get(`/users/${bob.userId}/e2e-key`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});

	test('GET other user e2e-key when friends and they have key returns 200', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);

		await sendFriendRequest(harness, alice.token, bob.userId);
		await acceptFriendRequest(harness, bob.token, alice.userId);

		const bobKey = 'dGVzdC1wdWJsaWMta2V5LWJhc2U2NA==';
		await createBuilder(harness, bob.token)
			.post('/users/@me/e2e-key')
			.body({public_key: bobKey})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		const res = await createBuilder<{public_key_base64: string}>(harness, alice.token)
			.get(`/users/${bob.userId}/e2e-key`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(res.public_key_base64).toBe(bobKey);
	});
});
