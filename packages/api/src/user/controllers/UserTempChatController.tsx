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

import {createUserID} from '@fluxer/api/src/BrandedTypes';
import {Config} from '@fluxer/api/src/Config';
import {Logger} from '@fluxer/api/src/Logger';
import {DefaultUserOnly, LoginRequired} from '@fluxer/api/src/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@fluxer/api/src/middleware/RateLimitMiddleware';
import {OpenAPI} from '@fluxer/api/src/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@fluxer/api/src/RateLimitConfig';
import type {HonoApp} from '@fluxer/api/src/types/HonoEnv';
import {Validator} from '@fluxer/api/src/Validator';
import {UserIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const E2EKeyResponse = z.object({
	public_key_base64: z.string(),
});
const E2EKeySetRequest = z.object({
	public_key: z.string().describe('X25519 public key in base64'),
});
const CreateTempChatRequest = z.object({
	recipient_id: SnowflakeType.describe('User ID of the friend to start a temp chat with'),
});
const TempChatSummaryResponse = z.object({
	id: z.string(),
	participant_ids: z.tuple([z.string(), z.string()]).describe('User IDs as strings'),
	created_at: z.coerce.date(),
});
const TempChatMessageResponse = z.object({
	id: z.string(),
	sender_id: z.string(),
	ciphertext: z.string(),
	iv: z.string(),
	ephemeral_public_key: z.string(),
	created_at: z.coerce.date(),
});
const SendTempChatMessageRequest = z.object({
	ciphertext: z.string(),
	iv: z.string(),
	ephemeral_public_key: z.string(),
});
const TempChatIdParam = z.object({
	temp_chat_id: z.string().describe('Temp chat ID (legacy: user_id_1_user_id_2, or V2: numeric chat_id)'),
});

const SCHEMA_NOT_INITIALIZED_MESSAGE =
	'Database schema not initialized. Run: cd fluxer_api && pnpm exec tsx scripts/CassandraMigrate.tsx up';

function isCassandraSchemaError(err: unknown): boolean {
	const msg = err instanceof Error ? err.message : String(err);
	return (
		/unconfigured\s+table/i.test(msg) ||
		/Keyspace\s+.*\s+does\s+not\s+exist/i.test(msg) ||
		/InvalidQueryException/i.test(msg) ||
		/undefined\s+column/i.test(msg)
	);
}

function handleTempChatError(err: unknown): {
	status: 403 | 404 | 500;
	message: string;
	detail?: string;
} {
	if (err instanceof Error) {
		if (err.message === 'TEMP_CHAT_REQUIRES_FRIENDSHIP' || err.message === 'TEMP_CHAT_FORBIDDEN')
			return {status: 403, message: err.message};
		if (err.message === 'TEMP_CHAT_NOT_FOUND') return {status: 404, message: err.message};
	}
	const detail = isCassandraSchemaError(err)
		? SCHEMA_NOT_INITIALIZED_MESSAGE
		: (err instanceof Error ? err.message : String(err));
	return {status: 500, message: 'Internal server error', detail};
}

export function UserTempChatController(app: HonoApp) {
	app.get(
		'/users/@me/e2e-key',
		RateLimitMiddleware(RateLimitConfigs.USER_E2E_KEY),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'get_my_e2e_key',
			summary: 'Get current user E2E public key',
			responseSchema: E2EKeyResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description: 'Returns the current user\'s X25519 public key for temp chat encryption. 404 if not set.',
		}),
		async (ctx) => {
			const key = await ctx.get('tempChatRequestService').getMyPublicKey(ctx.get('user').id);
			if (key === null) return ctx.json({error: 'E2E key not set'}, 404);
			return ctx.json({public_key_base64: key});
		},
	);

	app.post(
		'/users/@me/e2e-key',
		RateLimitMiddleware(RateLimitConfigs.USER_E2E_KEY),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', E2EKeySetRequest),
		OpenAPI({
			operationId: 'set_my_e2e_key',
			summary: 'Set current user E2E public key',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description: 'Stores the current user\'s X25519 public key for temp chat. Client generates key pair and sends public key.',
		}),
		async (ctx) => {
			await ctx.get('tempChatRequestService').setMyPublicKey(
				ctx.get('user').id,
				ctx.req.valid('json').public_key,
			);
			return ctx.body(null, 204);
		},
	);

	app.get(
		'/users/:user_id/e2e-key',
		RateLimitMiddleware(RateLimitConfigs.USER_E2E_KEY),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'get_user_e2e_key',
			summary: 'Get another user\'s E2E public key',
			responseSchema: E2EKeyResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description: 'Returns a user\'s X25519 public key if the caller is friends with that user. 404 if not friend or key not set.',
		}),
		async (ctx) => {
			const otherUserId = createUserID(ctx.req.valid('param').user_id);
			const key = await ctx.get('tempChatRequestService').getOtherUserPublicKey(
				ctx.get('user').id,
				otherUserId,
			);
			if (key === null) return ctx.json({error: 'Not found'}, 404);
			return ctx.json({public_key_base64: key});
		},
	);

	app.post(
		'/users/@me/temp-chats',
		RateLimitMiddleware(RateLimitConfigs.USER_TEMP_CHATS),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', CreateTempChatRequest),
		OpenAPI({
			operationId: 'create_temp_chat',
			summary: 'Create temp chat',
			responseSchema: TempChatSummaryResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description: 'Creates a new temporary encrypted chat with a friend. Each call creates a distinct chat (supports multiple temp chats per pair). Requires friendship.',
		}),
		async (ctx) => {
			try {
				const response = await ctx.get('tempChatRequestService').createTempChat(
					ctx.get('user').id,
					createUserID(ctx.req.valid('json').recipient_id),
				);
				return ctx.json({
					id: response.id,
					participant_ids: response.participant_ids.map(String),
					created_at: response.created_at.toISOString(),
				});
			} catch (err) {
				Logger.error(
					{err, userId: String(ctx.get('user').id), recipientId: String(ctx.req.valid('json').recipient_id)},
					'Temp chat create failed',
				);
				const {status, message, detail} = handleTempChatError(err);
				const body =
					status === 500 && Config.nodeEnv === 'development' && detail
						? {error: message, detail}
						: {error: message};
				return ctx.json(body, status);
			}
		},
	);

	app.get(
		'/users/@me/temp-chats',
		RateLimitMiddleware(RateLimitConfigs.USER_TEMP_CHATS),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'list_temp_chats',
			summary: 'List temp chats',
			responseSchema: z.array(TempChatSummaryResponse),
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description: 'Returns all temporary encrypted chats for the current user.',
		}),
		async (ctx) => {
			const list = await ctx.get('tempChatRequestService').listTempChats(ctx.get('user').id);
			return ctx.json(
				list.map((c) => ({
					id: c.id,
					participant_ids: c.participant_ids.map(String),
					created_at: c.created_at.toISOString(),
				})),
			);
		},
	);

	app.get(
		'/users/@me/temp-chats/:temp_chat_id/messages',
		RateLimitMiddleware(RateLimitConfigs.USER_TEMP_CHATS),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', TempChatIdParam),
		OpenAPI({
			operationId: 'list_temp_chat_messages',
			summary: 'List temp chat messages',
			responseSchema: z.array(TempChatMessageResponse),
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description: 'Returns encrypted messages for a temp chat. Only participants can list.',
		}),
		async (ctx) => {
			try {
				const messages = await ctx.get('tempChatRequestService').getMessages(
					ctx.get('user').id,
					ctx.req.valid('param').temp_chat_id,
				);
				return ctx.json(
					messages.map((m) => ({
						id: m.id,
						sender_id: m.sender_id.toString(),
						ciphertext: m.ciphertext,
						iv: m.iv,
						ephemeral_public_key: m.ephemeral_public_key,
						created_at: m.created_at.toISOString(),
					})),
				);
			} catch (err) {
				Logger.error(
					{err, userId: String(ctx.get('user').id), tempChatId: ctx.req.valid('param').temp_chat_id},
					'Temp chat get messages failed',
				);
				const {status, message, detail} = handleTempChatError(err);
				const body =
					status === 500 && Config.nodeEnv === 'development' && detail
						? {error: message, detail}
						: {error: message};
				return ctx.json(body, status);
			}
		},
	);

	app.post(
		'/users/@me/temp-chats/:temp_chat_id/messages',
		RateLimitMiddleware(RateLimitConfigs.USER_TEMP_CHATS),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', TempChatIdParam),
		Validator('json', SendTempChatMessageRequest),
		OpenAPI({
			operationId: 'send_temp_chat_message',
			summary: 'Send temp chat message',
			responseSchema: z.object({id: z.string(), created_at: z.string()}),
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description: 'Sends an E2E-encrypted message to a temp chat. Body must contain ciphertext, iv, ephemeral_public_key.',
		}),
		async (ctx) => {
			try {
				const result = await ctx.get('tempChatRequestService').sendMessage(
					ctx.get('user').id,
					ctx.req.valid('param').temp_chat_id,
					ctx.req.valid('json'),
				);
				return ctx.json({
					id: result.id,
					created_at: result.created_at.toISOString(),
				});
			} catch (err) {
				Logger.error(
					{err, userId: String(ctx.get('user').id), tempChatId: ctx.req.valid('param').temp_chat_id},
					'Temp chat send message failed',
				);
				const {status, message, detail} = handleTempChatError(err);
				const body =
					status === 500 && Config.nodeEnv === 'development' && detail
						? {error: message, detail}
						: {error: message};
				return ctx.json(body, status);
			}
		},
	);

	app.post(
		'/users/@me/temp-chats/:temp_chat_id/request-delete',
		RateLimitMiddleware(RateLimitConfigs.USER_TEMP_CHATS),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', TempChatIdParam),
		OpenAPI({
			operationId: 'request_delete_temp_chat',
			summary: 'Request delete temp chat',
			responseSchema: z.object({deleted: z.boolean()}),
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Records your request to delete. The chat is only deleted when both participants have requested delete.',
		}),
		async (ctx) => {
			try {
				const {deleted} = await ctx.get('tempChatRequestService').requestDeleteTempChat(
					ctx.get('user').id,
					ctx.req.valid('param').temp_chat_id,
				);
				return ctx.json({deleted}, 200);
			} catch (err) {
				Logger.error(
					{err, userId: String(ctx.get('user').id), tempChatId: ctx.req.valid('param').temp_chat_id},
					'Temp chat request-delete failed',
				);
				const {status, message, detail} = handleTempChatError(err);
				const body =
					status === 500 && Config.nodeEnv === 'development' && detail
						? {error: message, detail}
						: {error: message};
				return ctx.json(body, status);
			}
		},
	);

	app.delete(
		'/users/@me/temp-chats/:temp_chat_id',
		RateLimitMiddleware(RateLimitConfigs.USER_TEMP_CHATS),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', TempChatIdParam),
		OpenAPI({
			operationId: 'delete_temp_chat',
			summary: 'Request delete temp chat (both parties must agree)',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Records your request to delete. The chat is only deleted when both participants have requested.',
		}),
		async (ctx) => {
			try {
				const {deleted} = await ctx.get('tempChatRequestService').requestDeleteTempChat(
					ctx.get('user').id,
					ctx.req.valid('param').temp_chat_id,
				);
				if (deleted) {
					return ctx.body(null, 204);
				}
				return ctx.json({requested: true}, 200);
			} catch (err) {
				Logger.error(
					{err, userId: String(ctx.get('user').id), tempChatId: ctx.req.valid('param').temp_chat_id},
					'Temp chat delete failed',
				);
				const {status, message, detail} = handleTempChatError(err);
				const body =
					status === 500 && Config.nodeEnv === 'development' && detail
						? {error: message, detail}
						: {error: message};
				return ctx.json(body, status);
			}
		},
	);
}
