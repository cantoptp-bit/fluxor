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

import {Endpoints} from '@app/Endpoints';
import {Routes} from '@app/Routes';
import {GuildRecord} from '@app/records/GuildRecord';
import {GuildRoleRecord} from '@app/records/GuildRoleRecord';
import type {GuildReadyData} from '@app/types/gateway/GatewayGuildTypes';
import * as RouterUtils from '@app/utils/RouterUtils';
import type {Guild} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {GuildRole} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';
import {DEFAULT_PERMISSIONS} from '@fluxer/constants/src/ChannelConstants';
import http from '@app/lib/HttpClient';
import {makeAutoObservable} from 'mobx';
import {runInAction} from 'mobx';

class GuildStore {
	guilds: Record<string, GuildRecord> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	getGuild(guildId: string): GuildRecord | undefined {
		return this.guilds[guildId];
	}

	getGuildIds(): Array<string> {
		return Object.keys(this.guilds);
	}

	getGuildRoles(guildId: string, includeEveryone = false): Array<GuildRoleRecord> {
		const guild = this.guilds[guildId];
		if (!guild) {
			return [];
		}
		return Object.values(guild.roles).filter((role) => includeEveryone || role.id !== guildId);
	}

	getGuilds(): Array<GuildRecord> {
		return Object.values(this.guilds);
	}

	getOwnedGuilds(userId: string): Array<GuildRecord> {
		return Object.values(this.guilds).filter((guild) => guild.ownerId === userId);
	}

	handleConnectionOpen({guilds}: {guilds: Array<GuildReadyData>}): void {
		const availableGuilds = guilds.filter((guild) => !guild.unavailable);

		if (availableGuilds.length === 0) {
			this.guilds = {};
			return;
		}

		this.guilds = availableGuilds.reduce<Record<string, GuildRecord>>((acc, guildData) => {
			acc[guildData.id] = GuildRecord.fromGuildReadyData(guildData);
			return acc;
		}, {});
	}

	handleGuildCreate(guild: GuildReadyData): void {
		if (guild.unavailable) {
			return;
		}

		this.guilds[guild.id] = GuildRecord.fromGuildReadyData(guild);
	}

	/**
	 * Fetches the user's guild list from the API (which includes guild_type) and
	 * patches each guild in the store so server vs community is correct. Call after
	 * READY so the sidebar shows only servers under Servers and only communities under Communities.
	 */
	async syncGuildTypesFromApi(): Promise<void> {
		try {
			const response = await http.get<Array<{id: string; guild_type?: 'server' | 'community'}>>({
				url: Endpoints.USER_GUILDS_LIST,
			});
			if (!response.ok || !Array.isArray(response.body)) {
				return;
			}
			runInAction(() => {
				for (const apiGuild of response.body) {
					const guildId = apiGuild.id;
					const existing = this.guilds[guildId];
					if (!existing) continue;
					const apiType = apiGuild.guild_type === 'community' ? 'community' : 'server';
					if (existing.guildType !== apiType) {
						this.guilds[guildId] = existing.withUpdates({guild_type: apiType});
					}
				}
			});
		} catch (_err) {
			// Non-fatal: gateway payload may omit guild_type; sidebar may show all under Servers until next load
		}
	}

	/**
	 * Add a guild from API create response so the client can navigate immediately
	 * without waiting for GUILD_CREATE (which is only sent in FLUXER_LITE_MODE).
	 */
	addGuildFromApiResponse(guild: Guild): void {
		const everyoneRole: GuildRole = {
			id: guild.id,
			name: '@everyone',
			color: 0,
			position: 0,
			hoist_position: null,
			permissions: String(DEFAULT_PERMISSIONS),
			hoist: false,
			mentionable: false,
		};
		const roles: Record<string, GuildRoleRecord> = {
			[guild.id]: new GuildRoleRecord(guild.id, everyoneRole),
		};
		this.guilds[guild.id] = new GuildRecord({...guild, roles});
	}

	handleGuildUpdate(guild: Guild): void {
		const existingGuild = this.guilds[guild.id];
		if (!existingGuild) {
			return;
		}

		this.guilds[guild.id] = new GuildRecord({
			...guild,
			roles: existingGuild.roles,
		});
	}

	handleGuildDelete({guildId, unavailable}: {guildId: string; unavailable?: boolean}): void {
		delete this.guilds[guildId];

		if (!unavailable) {
			const history = RouterUtils.getHistory();
			const currentPath = history?.location.pathname ?? '';
			const guildRoutePrefix = `/channels/${guildId}`;

			if (currentPath.startsWith(guildRoutePrefix)) {
				RouterUtils.transitionTo(Routes.ME);
			}
		}
	}

	private updateGuildWithRoles(
		guildId: string,
		roleUpdater: (roles: Record<string, GuildRoleRecord>) => Record<string, GuildRoleRecord>,
	): void {
		const guild = this.guilds[guildId];
		if (!guild) {
			return;
		}

		const updatedRoles = roleUpdater({...guild.roles});
		this.guilds[guildId] = new GuildRecord({
			...guild.toJSON(),
			roles: updatedRoles,
		});
	}

	handleGuildRoleCreate({guildId, role}: {guildId: string; role: GuildRole}): void {
		this.updateGuildWithRoles(guildId, (roles) => ({
			...roles,
			[role.id]: new GuildRoleRecord(guildId, role),
		}));
	}

	handleGuildRoleDelete({guildId, roleId}: {guildId: string; roleId: string}): void {
		this.updateGuildWithRoles(guildId, (roles) =>
			Object.fromEntries(Object.entries(roles).filter(([id]) => id !== roleId)),
		);
	}

	handleGuildRoleUpdate({guildId, role}: {guildId: string; role: GuildRole}): void {
		this.updateGuildWithRoles(guildId, (roles) => ({
			...roles,
			[role.id]: new GuildRoleRecord(guildId, role),
		}));
	}

	handleGuildRoleUpdateBulk({guildId, roles}: {guildId: string; roles: Array<GuildRole>}): void {
		this.updateGuildWithRoles(guildId, (existingRoles) => {
			const updatedRoles = {...existingRoles};
			for (const role of roles) {
				updatedRoles[role.id] = new GuildRoleRecord(guildId, role);
			}
			return updatedRoles;
		});
	}
}

export default new GuildStore();
