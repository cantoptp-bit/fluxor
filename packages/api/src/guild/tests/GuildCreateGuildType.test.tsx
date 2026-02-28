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
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Guild create guild_type', () => {
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

	it('POST /guilds with guild_type server returns guild with guild_type server', async () => {
		const account = await createTestAccount(harness);
		const response = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({name: 'My Server', guild_type: 'server'})
			.execute();
		expect(response.guild_type).toBe('server');
		expect(response.name).toBe('My Server');
		expect(response.id).toBeDefined();
	});

	it('POST /guilds with guild_type community returns guild with guild_type community', async () => {
		const account = await createTestAccount(harness);
		const response = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({name: 'My Community', guild_type: 'community'})
			.execute();
		expect(response.guild_type).toBe('community');
		expect(response.name).toBe('My Community');
		expect(response.id).toBeDefined();
	});

	it('POST /guilds without guild_type defaults to server', async () => {
		const account = await createTestAccount(harness);
		const response = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({name: 'Default Type Guild'})
			.execute();
		expect(response.guild_type).toBe('server');
		expect(response.name).toBe('Default Type Guild');
	});

	it('GET /guilds/:id returns guild_type for server', async () => {
		const account = await createTestAccount(harness);
		const created = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({name: 'Fetch Server', guild_type: 'server'})
			.execute();
		const fetched = await createBuilder<GuildResponse>(harness, account.token)
			.get(`/guilds/${created.id}`)
			.execute();
		expect(fetched.guild_type).toBe('server');
		expect(fetched.id).toBe(created.id);
	});

	it('GET /guilds/:id returns guild_type for community', async () => {
		const account = await createTestAccount(harness);
		const created = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({name: 'Fetch Community', guild_type: 'community'})
			.execute();
		const fetched = await createBuilder<GuildResponse>(harness, account.token)
			.get(`/guilds/${created.id}`)
			.execute();
		expect(fetched.guild_type).toBe('community');
		expect(fetched.id).toBe(created.id);
	});

	it('GET /users/@me/guilds includes guild_type for each guild', async () => {
		const account = await createTestAccount(harness);
		await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({name: 'List Server', guild_type: 'server'})
			.execute();
		await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({name: 'List Community', guild_type: 'community'})
			.execute();
		const list = await createBuilder<Array<GuildResponse>>(harness, account.token).get('/users/@me/guilds').execute();
		expect(Array.isArray(list)).toBe(true);
		const serverGuild = list.find((g) => g.name === 'List Server');
		const communityGuild = list.find((g) => g.name === 'List Community');
		expect(serverGuild?.guild_type).toBe('server');
		expect(communityGuild?.guild_type).toBe('community');
	});
});
