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

import {createGuildID} from '@fluxer/api/src/BrandedTypes';
import {RateLimitMiddleware} from '@fluxer/api/src/middleware/RateLimitMiddleware';
import {OpenAPI} from '@fluxer/api/src/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@fluxer/api/src/RateLimitConfig';
import type {HonoApp} from '@fluxer/api/src/types/HonoEnv';
import {GatewayBotResponse} from '@fluxer/schema/src/domains/gateway/GatewaySchemas';
import {RpcRequest} from '@fluxer/schema/src/domains/rpc/RpcSchemas';

function isLocalhost(ctx: { req: { header: (name: string) => string | undefined } }): boolean {
	const host = ctx.req.header('host') ?? '';
	return host.startsWith('127.0.0.1') || host.startsWith('localhost');
}

export function GatewayController(app: HonoApp) {
	// Session init: used by real gateway and mock gateway so HAS_SESSION_STARTED is set and message sending works. Always registered (not gated by test harness).
	app.post('/gateway/session-init', async (ctx) => {
		let request: {type: 'session'; token: string; version: 1};
		try {
			const parsed = RpcRequest.parse(await ctx.req.json());
			if (parsed.type !== 'session') {
				return ctx.json({type: 'error', code: 'INVALID_REQUEST', message: 'Expected session init request'}, 400);
			}
			request = parsed;
		} catch {
			return ctx.json({type: 'error', code: 'INVALID_REQUEST', message: 'Invalid session init body'}, 400);
		}
		const response = await ctx.get('rpcService').handleRpcRequest({
			request,
			requestCache: ctx.get('requestCache'),
		});
		return ctx.json(response);
	});

	app.get(
		'/gateway/bot',
		RateLimitMiddleware(RateLimitConfigs.GATEWAY_BOT_INFO),
		OpenAPI({
			operationId: 'get_gateway_bot',
			summary: 'Get gateway information',
			responseSchema: GatewayBotResponse,
			statusCode: 200,
			security: [],
			tags: ['Gateway'],
			description:
				'Retrieves gateway connection information and recommended shard count for establishing WebSocket connections.',
		}),
		async (ctx) => {
			const gatewayRequestService = ctx.get('gatewayRequestService');
			return ctx.json(await gatewayRequestService.getBotGatewayInfo(ctx.req.header('Authorization') ?? null));
		},
	);

	// Internal: used by Node gateway for get_viewable_channels RPC when Erlang gateway is not used. Localhost only.
	app.get('/internal/guilds/:guild_id/viewable-channel-ids', async (ctx) => {
		if (!isLocalhost(ctx)) {
			return ctx.json({ error: 'Forbidden' }, 403);
		}
		const guildId = createGuildID(BigInt(ctx.req.param('guild_id')));
		const channels = await ctx.get('channelRepository').listGuildChannels(guildId);
		const channel_ids = channels.map((c) => c.id.toString());
		return ctx.json({ channel_ids });
	});
}
