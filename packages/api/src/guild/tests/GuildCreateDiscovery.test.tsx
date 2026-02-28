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
import {
	DiscoveryCategories,
	DISCOVERY_DESCRIPTION_MAX_LENGTH,
	DISCOVERY_DESCRIPTION_MIN_LENGTH,
} from '@fluxer/constants/src/DiscoveryConstants';
import type {
	DiscoveryGuildListResponse,
	DiscoveryStatusResponse,
} from '@fluxer/schema/src/domains/guild/GuildDiscoverySchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Guild create with discovery', () => {
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

	it('creates community with list_in_discovery and creates pending discovery application', async () => {
		const account = await createTestAccount(harness);
		const guild = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Discovery Community',
				guild_type: 'community',
				list_in_discovery: true,
				discovery_description: 'A test community for the discovery explorer.',
				discovery_category: DiscoveryCategories.GAMING,
			})
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(guild.id).toBeDefined();
		expect(guild.guild_type).toBe('community');

		const status = await createBuilder<DiscoveryStatusResponse>(harness, account.token)
			.get(`/guilds/${guild.id}/discovery`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(status.application).not.toBeNull();
		expect(status.application?.status).toBe('pending');
		expect(status.application?.description).toBe('A test community for the discovery explorer.');
		expect(status.application?.category_type).toBe(DiscoveryCategories.GAMING);
	});

	it('creates community without list_in_discovery and does not create discovery application', async () => {
		const account = await createTestAccount(harness);
		const guild = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({
				name: 'No Discovery Community',
				guild_type: 'community',
			})
			.expect(HTTP_STATUS.OK)
			.execute();

		const status = await createBuilder<DiscoveryStatusResponse>(harness, account.token)
			.get(`/guilds/${guild.id}/discovery`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(status.application).toBeNull();
	});

	it('returns 400 when list_in_discovery is true but description and category omitted', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Bad Discovery',
				guild_type: 'community',
				list_in_discovery: true,
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});

	it('returns 400 when discovery_description is too short', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Short Desc',
				guild_type: 'community',
				list_in_discovery: true,
				discovery_description: 'short',
				discovery_category: DiscoveryCategories.OTHER,
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});

	it('returns 400 when discovery_category is out of range', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Bad Category',
				guild_type: 'community',
				list_in_discovery: true,
				discovery_description: 'Valid description with enough characters.',
				discovery_category: 9,
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});

	it('creates server with list_in_discovery and does not create discovery application', async () => {
		const account = await createTestAccount(harness);
		const guild = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Server With Discovery Fields',
				guild_type: 'server',
				list_in_discovery: true,
				discovery_description: 'Servers should ignore discovery fields.',
				discovery_category: DiscoveryCategories.GAMING,
			})
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(guild.guild_type).toBe('server');

		const status = await createBuilder<DiscoveryStatusResponse>(harness, account.token)
			.get(`/guilds/${guild.id}/discovery`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(status.application).toBeNull();
	});

	it('creates community with list_in_discovery false and extra discovery fields without applying', async () => {
		const account = await createTestAccount(harness);
		const guild = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Community No List',
				guild_type: 'community',
				list_in_discovery: false,
				discovery_description: 'Description should be ignored when not listing.',
				discovery_category: DiscoveryCategories.MUSIC,
			})
			.expect(HTTP_STATUS.OK)
			.execute();

		const status = await createBuilder<DiscoveryStatusResponse>(harness, account.token)
			.get(`/guilds/${guild.id}/discovery`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(status.application).toBeNull();
	});

	it('accepts discovery_description at minimum length (10 chars)', async () => {
		const account = await createTestAccount(harness);
		const tenChars = '0123456789';
		expect(tenChars.length).toBe(DISCOVERY_DESCRIPTION_MIN_LENGTH);

		const guild = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Min Desc Community',
				guild_type: 'community',
				list_in_discovery: true,
				discovery_description: tenChars,
				discovery_category: DiscoveryCategories.EDUCATION,
			})
			.expect(HTTP_STATUS.OK)
			.execute();

		const status = await createBuilder<DiscoveryStatusResponse>(harness, account.token)
			.get(`/guilds/${guild.id}/discovery`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(status.application?.description).toBe(tenChars);
		expect(status.application?.status).toBe('pending');
	});

	it('accepts discovery_description at maximum length (300 chars)', async () => {
		const account = await createTestAccount(harness);
		const maxDesc = 'a'.repeat(DISCOVERY_DESCRIPTION_MAX_LENGTH);
		expect(maxDesc.length).toBe(DISCOVERY_DESCRIPTION_MAX_LENGTH);

		const guild = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Max Desc Community',
				guild_type: 'community',
				list_in_discovery: true,
				discovery_description: maxDesc,
				discovery_category: DiscoveryCategories.OTHER,
			})
			.expect(HTTP_STATUS.OK)
			.execute();

		const status = await createBuilder<DiscoveryStatusResponse>(harness, account.token)
			.get(`/guilds/${guild.id}/discovery`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(status.application?.description).toBe(maxDesc);
		expect(status.application?.status).toBe('pending');
	});

	it('accepts discovery_category at boundaries (0 and 8)', async () => {
		const account = await createTestAccount(harness);
		const desc = 'Valid description for category test.';

		const guild0 = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Category Zero Community',
				guild_type: 'community',
				list_in_discovery: true,
				discovery_description: desc,
				discovery_category: 0,
			})
			.expect(HTTP_STATUS.OK)
			.execute();

		const guild8 = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Category Eight Community',
				guild_type: 'community',
				list_in_discovery: true,
				discovery_description: desc,
				discovery_category: 8,
			})
			.expect(HTTP_STATUS.OK)
			.execute();

		const status0 = await createBuilder<DiscoveryStatusResponse>(harness, account.token)
			.get(`/guilds/${guild0.id}/discovery`)
			.expect(HTTP_STATUS.OK)
			.execute();
		const status8 = await createBuilder<DiscoveryStatusResponse>(harness, account.token)
			.get(`/guilds/${guild8.id}/discovery`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(status0.application?.category_type).toBe(0);
		expect(status8.application?.category_type).toBe(8);
	});

	it('does not return pending community from create in public discovery list', async () => {
		const account = await createTestAccount(harness);
		const guild = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Pending Only Community',
				guild_type: 'community',
				list_in_discovery: true,
				discovery_description: 'This guild has only a pending application.',
				discovery_category: DiscoveryCategories.ENTERTAINMENT,
			})
			.expect(HTTP_STATUS.OK)
			.execute();

		const searcher = await createTestAccount(harness);
		const results = await createBuilder<DiscoveryGuildListResponse>(harness, searcher.token)
			.get('/discovery/guilds')
			.expect(HTTP_STATUS.OK)
			.execute();

		const found = results.guilds.find((g) => g.id === guild.id);
		expect(found).toBeUndefined();
	});
});
