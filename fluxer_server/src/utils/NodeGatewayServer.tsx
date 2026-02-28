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

/**
 * Built-in gateway implementation (Node/TypeScript). Provides the same WebSocket
 * protocol as the Erlang gateway so clients can connect, identify, and send messages.
 * Used when the Erlang gateway binary is not available (e.g. Windows or no install).
 * Session init is notified to the API so HAS_SESSION_STARTED is set and messaging works.
 */

import { WebSocketServer, type WebSocket } from 'ws';
import { Logger } from '@app/Logger';
import { Config } from '@app/Config';
import { randomBytes } from 'node:crypto';
import { NatsConnectionManager } from '@fluxer/nats/src/NatsConnectionManager';
import { StringCodec } from 'nats';

const HELLO_OP = 10;
const HEARTBEAT_OP = 1;
const HEARTBEAT_ACK_OP = 11;
const IDENTIFY_OP = 2;
const RESUME_OP = 6;
const DISPATCH_OP = 0;
const INVALID_SESSION_OP = 9;

export interface NodeGatewayServer {
	start: () => void;
	stop: () => void;
}

export function createNodeGatewayServer(): NodeGatewayServer {
	const logger = Logger.child({ component: 'node-gateway' });
	let wss: WebSocketServer | null = null;
	let natsManager: NatsConnectionManager | null = null;
	const sessionByToken = new Map<string, { sessionId: string; user: Record<string, unknown> }>();

	const start = () => {
		if (wss) return;

		startNatsResponders().catch((error) => {
			logger.error({ error }, 'Failed to start NATS responders');
		});

		const port = Config.services.gateway.port;
		wss = new WebSocketServer({ port });

		logger.info({ port }, 'Built-in Node gateway started');

		wss.on('connection', (ws: WebSocket) => {
			logger.info('New connection to Node gateway');

			ws.send(
				JSON.stringify({
					op: HELLO_OP,
					d: { heartbeat_interval: 41250 },
				}),
			);

			ws.on('message', (data: Buffer | string | ArrayBuffer | ArrayBufferView | Array<Buffer | string | ArrayBuffer | ArrayBufferView>) => {
				try {
					const payload = JSON.parse(data.toString());
					void handlePayload(ws, payload).catch((err) => logger.error({ err }, 'handlePayload failed'));
				} catch (err) {
					logger.error({ err }, 'Failed to parse gateway payload');
				}
			});

			ws.on('error', (err: Error) => logger.error({ err }, 'Node gateway socket error'));
			ws.on('close', () => logger.info('Connection to Node gateway closed'));
		});
	};

	const apiBaseUrl = `http://127.0.0.1:${Config.services.server.port}`;

	function notifyApiSessionStarted(token: string): void {
		fetch(`${apiBaseUrl}/api/gateway/session-init`, {
			method: 'POST',
			headers: { 'Content-Type': 'application/json' },
			body: JSON.stringify({ type: 'session', token, version: 1 }),
		}).catch((err) => {
			logger.warn({ err }, 'Node gateway: failed to notify API session init');
		});
	}

	async function fetchUserByToken(token: string | undefined): Promise<Record<string, unknown> | null> {
		if (!token) return null;
		try {
			const res = await fetch(`${apiBaseUrl}/api/users/@me`, {
				headers: {
					Authorization: `Bearer ${token}`,
					'X-Forwarded-For': '127.0.0.1',
				},
			});
			if (!res.ok) {
				logger.warn({ status: res.status }, 'Node gateway: /api/users/@me failed');
				return null;
			}
			const data = (await res.json()) as Record<string, unknown>;
			const rawId = data.id;
			const id = rawId != null ? String(rawId) : '0';
			const rawDisc = data.discriminator;
			const discriminator =
				typeof rawDisc === 'number'
					? String(rawDisc).padStart(4, '0')
					: typeof rawDisc === 'string'
						? rawDisc
						: '0000';
			return {
				id,
				username: (data.username as string) ?? 'User',
				discriminator,
				avatar: data.avatar ?? null,
				bot: (data.bot as boolean) ?? false,
				system: (data.system as boolean) ?? false,
				mfa_enabled: (data.mfa_enabled as boolean) ?? false,
				locale: (data.locale as string) ?? 'en-US',
				verified: (data.verified as boolean) ?? false,
				email: (data.email as string | null) ?? null,
				flags: (data.flags as number) ?? 0,
				premium_type: (data.premium_type as number) ?? 0,
				public_flags: (data.flags as number) ?? 0,
			};
		} catch (err) {
			logger.warn({ err }, 'Node gateway: fetch /api/users/@me threw');
			return null;
		}
	}

	async function fetchUserGuilds(token: string | undefined): Promise<Array<Record<string, unknown>>> {
		if (!token) return [];
		try {
			const res = await fetch(`${apiBaseUrl}/api/users/@me/guilds`, {
				headers: {
					Authorization: `Bearer ${token}`,
					'X-Forwarded-For': '127.0.0.1',
				},
			});
			if (!res.ok) {
				logger.warn({ status: res.status }, 'Node gateway: /api/users/@me/guilds failed');
				return [];
			}
			const data = (await res.json()) as Array<Record<string, unknown>>;
			const guilds = Array.isArray(data) ? data : [];
			return guilds;
		} catch (err) {
			logger.warn({ err }, 'Node gateway: fetch /api/users/@me/guilds threw');
			return [];
		}
	}

	const defaultUser = {
		id: '0',
		username: 'User',
		discriminator: '0000',
		avatar: null,
		bot: false,
		system: false,
		mfa_enabled: false,
		locale: 'en-US',
		verified: false,
		email: null,
		flags: 0,
		premium_type: 0,
		public_flags: 0,
	};

	function buildReadyPayload(
		sessionId: string,
		user: Record<string, unknown>,
		guilds: Array<Record<string, unknown>>,
	) {
		return {
			op: DISPATCH_OP,
			t: 'READY',
			s: 1,
			d: {
				v: 9,
				user: {
					id: user.id,
					username: user.username,
					discriminator: user.discriminator,
					avatar: user.avatar,
					bot: user.bot,
					system: user.system,
					mfa_enabled: user.mfa_enabled,
					locale: user.locale,
					verified: user.verified,
					email: user.email,
					flags: user.flags,
					premium_type: user.premium_type,
					public_flags: user.public_flags,
				},
				guilds,
				private_channels: [],
				session_id: sessionId,
				analytics_token: '',
				country_code: 'US',
				friend_suggestion_count: 0,
				consents: { personalization: { consented: true } },
				experiments: [],
				guild_join_requests: [],
				merged_members: [],
				user_settings: {
					theme: 'dark',
					status: 'online',
					custom_status: null,
					default_guilds_restricted: false,
					inline_attachment_media: true,
					inline_embed_media: true,
					gif_auto_play: true,
					render_embeds: true,
					render_reactions: true,
					animate_emoji: true,
					enable_tts_command: true,
					message_display_compact: false,
					convert_emoticons: true,
					explicit_content_filter: 0,
					disable_games_tab: false,
					locale: 'en-US',
					developer_mode: false,
					detect_platform_accounts: true,
					guild_positions: [],
					friend_source_flags: 0,
					restricted_guilds: [],
					afk_timeout: 600,
					passwordless: false,
					contact_sync_enabled: false,
					native_phone_integration_enabled: false,
					guild_folders: [],
					bot_restricted_guilds: [],
					bot_default_guilds_restricted: false,
					animate_stickers: 0,
					render_spoilers: 0,
					incoming_call_flags: 0,
					group_dm_add_permission_flags: 0,
					trusted_domains: [],
					default_hide_muted_channels: false,
					status_resets_at: null,
					status_resets_to: null,
					time_format: 0,
					flags: 0,
				},
				read_states: [],
				user_guild_settings: [],
				presences: [],
				relationships: [],
				notes: {},
				geo_ordered_rtc_regions: [],
				auth_session_id_hash: sessionId,
			},
		};
	}

	async function sendReady(
		ws: WebSocket,
		sessionId: string,
		user: Record<string, unknown>,
		token: string | undefined,
	): Promise<void> {
		const guilds = await fetchUserGuilds(token);
		const payload = buildReadyPayload(sessionId, user, guilds);
		ws.send(JSON.stringify(payload));
		logger.info({ sessionId, guildCount: guilds.length }, 'Sent READY');
	}

	async function handlePayload(ws: WebSocket, payload: { op: number; d?: Record<string, unknown> }): Promise<void> {
		const { op, d } = payload;

		switch (op) {
			case HEARTBEAT_OP:
				ws.send(JSON.stringify({ op: HEARTBEAT_ACK_OP }));
				break;

			case IDENTIFY_OP: {
				const token = d?.token as string | undefined;
				logger.info({ hasToken: Boolean(token) }, 'Received IDENTIFY');
				if (token) {
					notifyApiSessionStarted(token);
				}
				const userFromApi = await fetchUserByToken(token);
				const user = userFromApi ?? defaultUser;
				const sessionId = randomBytes(16).toString('hex');
				if (token) {
					sessionByToken.set(token, { sessionId, user });
				}
				await sendReady(ws, sessionId, user, token);
				break;
			}

			case RESUME_OP: {
				const token = d?.token as string | undefined;
				const sessionId = d?.session_id as string | undefined;
				const stored = token && sessionId ? sessionByToken.get(token) : null;
				if (stored && stored.sessionId === sessionId) {
					logger.info({ sessionId }, 'RESUME accepted');
					await sendReady(ws, sessionId, stored.user, token);
				} else {
					logger.info('RESUME invalid, sending INVALID_SESSION');
					ws.send(JSON.stringify({ op: INVALID_SESSION_OP, d: false }));
				}
				break;
			}

			default:
				logger.debug({ op }, 'Unhandled opcode');
		}
	}

	const stop = () => {
		sessionByToken.clear();
		if (wss) {
			wss.close();
			wss = null;
		}
		if (natsManager) {
			natsManager.drain().catch(() => {});
			natsManager = null;
		}
		logger.info('Node gateway stopped');
	};

	async function startNatsResponders(): Promise<void> {
		logger.info({ pid: process.pid }, 'Node gateway starting NATS responders');
		const config = Config as { services: { nats?: { core_url?: string; auth_token?: string } } };
		natsManager = new NatsConnectionManager({
			url: config.services.nats?.core_url ?? 'nats://127.0.0.1:4222',
			token: config.services.nats?.auth_token,
			name: 'node-gateway-rpc',
		});

		await natsManager.connect();
		const nc = natsManager.getConnection();
		const sc = StringCodec();

		const subjects = [
			'rpc.gateway.push.invalidate_badge_count',
			'rpc.gateway.presence.terminate_sessions',
			'rpc.gateway.guild.check_permission',
			'rpc.gateway.guild.get_data',
			'rpc.gateway.guild.get_member',
			'rpc.gateway.guild.get_viewable_channels',
			'rpc.gateway.guild.start',
			'rpc.gateway.presence.join_guild',
			'rpc.gateway.guild.dispatch',
			'rpc.gateway.presence.dispatch',
			'rpc.gateway.guild.get_counts',
			'rpc.gateway.guild.get_channel_count',
			'rpc.gateway.guild.get_user_permissions',
			'rpc.gateway.guild.get_user_permissions_batch',
			'rpc.gateway.process.node_stats',
		];

		const handleRpc = async (subject: string, params: Record<string, unknown>): Promise<unknown> => {
			if (subject === 'rpc.gateway.push.invalidate_badge_count') return undefined;
			if (subject === 'rpc.gateway.guild.get_viewable_channels') {
				const guildId = params.guild_id as string | undefined;
				if (!guildId) return { channel_ids: [] };
				try {
					const res = await fetch(
						`${apiBaseUrl}/api/internal/guilds/${encodeURIComponent(guildId)}/viewable-channel-ids`,
						{ headers: { Host: '127.0.0.1', 'X-Forwarded-For': '127.0.0.1' } },
					);
					if (!res.ok) return { channel_ids: [] };
					const data = (await res.json()) as { channel_ids?: Array<string> };
					return { channel_ids: data.channel_ids ?? [] };
				} catch (err) {
					logger.warn({ err, guildId }, 'Node gateway: get_viewable_channels internal fetch failed');
					return { channel_ids: [] };
				}
			}
			if (subject === 'rpc.gateway.guild.check_permission') return { has_permission: true };
			if (subject === 'rpc.gateway.guild.get_data') {
				return {
					id: params.guild_id,
					name: 'Guild',
					owner_id: String(params.user_id ?? '1'),
					icon: null,
					banner: null,
					system_channel_id: null,
					system_channel_flags: 0,
					afk_channel_id: null,
					afk_timeout: 300,
					features: [],
					verification_level: 0,
					mfa_level: 0,
					nsfw_level: 0,
					explicit_content_filter: 0,
					default_message_notifications: 0,
					disabled_operations: 0,
					message_history_cutoff: null,
					permissions: null,
				};
			}
			if (subject === 'rpc.gateway.guild.get_member') {
				const userId = String(params.user_id ?? '1');
				return {
					success: true,
					member_data: {
						user: { id: userId, username: 'user', tag: 'user', discriminator: '0', avatar: null },
						nick: null,
						avatar: null,
						banner: null,
						accent_color: null,
						roles: [],
						joined_at: new Date().toISOString(),
						mute: false,
						deaf: false,
						communication_disabled_until: null,
						profile_flags: null,
					},
				};
			}
			if (subject === 'rpc.gateway.guild.start') return true;
			if (subject === 'rpc.gateway.process.node_stats') return { status: 'ok' };
			if (subject === 'rpc.gateway.guild.get_counts') return { member_count: 1, presence_count: 1 };
			if (subject === 'rpc.gateway.guild.get_channel_count') return { count: 5 };
			if (subject === 'rpc.gateway.guild.get_user_permissions')
				return { permissions: '0' };
			if (subject === 'rpc.gateway.guild.get_user_permissions_batch') return { permissions: [] };
			if (subject === 'rpc.gateway.guild.dispatch' || subject === 'rpc.gateway.presence.dispatch') {
				const eventData = params.data;
				const eventName = params.event;
				if (wss && eventName) {
					const clientPayload = JSON.stringify({
						op: DISPATCH_OP,
						t: eventName,
						s: Math.floor(Math.random() * 100000),
						d: eventData,
					});
					for (const client of wss.clients) {
						if (client.readyState === 1) client.send(clientPayload);
					}
				}
				return { ok: true };
			}
			return { ok: true };
		};

		for (const subject of subjects) {
			const sub = nc.subscribe(subject);
			logger.info(`Subscribed to NATS: ${subject}`);
			(async () => {
				for await (const msg of sub) {
					const raw = sc.decode(msg.data);
					const parsed = raw ? JSON.parse(raw) : {};
					const result = await handleRpc(subject, parsed);
					msg.respond(sc.encode(JSON.stringify({ ok: true, result })));
				}
			})().catch((err) => logger.error({ err, subject }, 'NATS responder error'));
		}
	}

	return { start, stop };
}
