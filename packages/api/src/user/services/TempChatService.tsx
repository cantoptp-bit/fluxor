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

import {createUserID, type UserID} from '@fluxer/api/src/BrandedTypes';
import type {TempChatMessageRow, TempChatMessageRowV2} from '@fluxer/api/src/database/types/TempChatTypes';
import type {IUserRepository} from '@fluxer/api/src/user/IUserRepository';
import type {ITempChatMessageRepository} from '@fluxer/api/src/user/repositories/ITempChatMessageRepository';
import type {ITempChatRepository} from '@fluxer/api/src/user/repositories/ITempChatRepository';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import type {SnowflakeService} from '@fluxer/api/src/infrastructure/SnowflakeService';

export function tempChatIdFromPair(userId1: UserID, userId2: UserID): string {
	const [lo, hi] = userId1 < userId2 ? [userId1, userId2] : [userId2, userId1];
	return `${lo}_${hi}`;
}

/** Parse legacy id "lo_hi" to participant pair, or null if invalid. */
export function parseTempChatId(id: string): [UserID, UserID] | null {
	const parts = id.split('_');
	if (parts.length !== 2) return null;
	const lo = BigInt(parts[0]);
	const hi = BigInt(parts[1]);
	if (lo >= hi) return null;
	return [createUserID(lo), createUserID(hi)];
}

/** True if id is V2 format (numeric string, no underscore). */
export function isV2TempChatId(id: string): boolean {
	return /^\d+$/.test(id) && !id.includes('_');
}

export class TempChatService {
	constructor(
		private readonly userRepository: IUserRepository,
		private readonly tempChatRepository: ITempChatRepository,
		private readonly tempChatMessageRepository: ITempChatMessageRepository,
		private readonly snowflakeService: SnowflakeService,
	) {}

	/** Ensure caller and recipient are friends; create or get temp chat. Returns chat row; id = tempChatIdFromPair(row.user_id_1, row.user_id_2). */
	async createOrGet(userId: UserID, recipientId: UserID): Promise<{id: string; user_id_1: UserID; user_id_2: UserID; created_at: Date}> {
		const friendship = await this.userRepository.getRelationship(userId, recipientId, RelationshipTypes.FRIEND);
		if (!friendship) {
			throw new Error('TEMP_CHAT_REQUIRES_FRIENDSHIP');
		}
		const row = await this.tempChatRepository.createOrGet(userId, recipientId);
		return {
			id: tempChatIdFromPair(row.user_id_1, row.user_id_2),
			user_id_1: row.user_id_1,
			user_id_2: row.user_id_2,
			created_at: row.created_at,
		};
	}

	/** Create a new temp chat (V2) with a unique id. Caller and recipient must be friends. Use this for multiple temp chats per pair. */
	async create(userId: UserID, recipientId: UserID): Promise<{id: string; user_id_1: UserID; user_id_2: UserID; created_at: Date}> {
		const friendship = await this.userRepository.getRelationship(userId, recipientId, RelationshipTypes.FRIEND);
		if (!friendship) {
			throw new Error('TEMP_CHAT_REQUIRES_FRIENDSHIP');
		}
		const chatId = await this.snowflakeService.generate();
		const row = await this.tempChatRepository.createV2(userId, recipientId, chatId);
		return {
			id: String(chatId),
			user_id_1: row.user_id_1,
			user_id_2: row.user_id_2,
			created_at: row.created_at,
		};
	}

	async listByUserId(userId: UserID): Promise<Array<{id: string; user_id_1: UserID; user_id_2: UserID; created_at: Date}>> {
		const [legacy, v2] = await Promise.all([
			this.tempChatRepository.listByUserId(userId),
			this.tempChatRepository.listByUserIdV2(userId),
		]);
		const legacyMapped = legacy.map((r) => ({
			id: tempChatIdFromPair(r.user_id_1, r.user_id_2),
			user_id_1: r.user_id_1,
			user_id_2: r.user_id_2,
			created_at: r.created_at,
		}));
		const v2Mapped = v2.map((r) => ({
			id: String(r.chat_id),
			user_id_1: r.user_id_1,
			user_id_2: r.user_id_2,
			created_at: r.created_at,
		}));
		return [...legacyMapped, ...v2Mapped];
	}

	/** Throws if tempChatId invalid or user is not a participant. Returns pair and optional V2 chat_id. */
	async assertParticipant(userId: UserID, tempChatId: string): Promise<{pair: [UserID, UserID]; chatId: bigint | null}> {
		if (isV2TempChatId(tempChatId)) {
			const chatId = BigInt(tempChatId);
			const chat = await this.tempChatRepository.getByChatId(chatId);
			if (!chat) throw new Error('TEMP_CHAT_NOT_FOUND');
			const pair: [UserID, UserID] = [chat.user_id_1, chat.user_id_2];
			if (userId !== chat.user_id_1 && userId !== chat.user_id_2) throw new Error('TEMP_CHAT_FORBIDDEN');
			return {pair, chatId};
		}
		const pair = parseTempChatId(tempChatId);
		if (!pair) throw new Error('TEMP_CHAT_NOT_FOUND');
		const [lo, hi] = pair;
		if (userId !== lo && userId !== hi) throw new Error('TEMP_CHAT_FORBIDDEN');
		return {pair, chatId: null};
	}

	async getMessages(userId: UserID, tempChatId: string): Promise<Array<TempChatMessageRow | TempChatMessageRowV2>> {
		const {pair, chatId} = await this.assertParticipant(userId, tempChatId);
		if (chatId !== null) {
			return this.tempChatMessageRepository.listByChatIdV2(chatId);
		}
		return this.tempChatMessageRepository.listByChat(pair[0], pair[1]);
	}

	async sendMessage(
		senderId: UserID,
		tempChatId: string,
		payload: {ciphertext: string; iv: string; ephemeral_public_key: string},
	): Promise<{message_id: bigint; created_at: Date}> {
		const {pair, chatId} = await this.assertParticipant(senderId, tempChatId);
		const messageId = await this.snowflakeService.generate();
		const now = new Date();
		if (chatId !== null) {
			const row: TempChatMessageRowV2 = {
				chat_id: chatId,
				message_id: messageId,
				sender_id: senderId,
				ciphertext: payload.ciphertext,
				iv: payload.iv,
				ephemeral_public_key: payload.ephemeral_public_key,
				created_at: now,
			};
			await this.tempChatMessageRepository.insertV2(row);
			return {message_id: messageId, created_at: now};
		}
		const row: TempChatMessageRow = {
			user_id_1: pair[0],
			user_id_2: pair[1],
			message_id: messageId,
			sender_id: senderId,
			ciphertext: payload.ciphertext,
			iv: payload.iv,
			ephemeral_public_key: payload.ephemeral_public_key,
			created_at: now,
		};
		await this.tempChatMessageRepository.insert(row);
		return {message_id: messageId, created_at: now};
	}

	async deleteChat(userId: UserID, tempChatId: string): Promise<void> {
		const {pair, chatId} = await this.assertParticipant(userId, tempChatId);
		if (chatId !== null) {
			await this.tempChatRepository.deleteByChatId(chatId);
			return;
		}
		await this.tempChatRepository.delete(pair[0], pair[1]);
	}

	/**
	 * Record this user's request to delete. If both participants have requested, deletes the chat.
	 * Returns true if the chat was actually deleted, false if only the request was recorded.
	 */
	async requestDelete(userId: UserID, tempChatId: string): Promise<{deleted: boolean}> {
		const {pair, chatId} = await this.assertParticipant(userId, tempChatId);
		if (chatId !== null) {
			await this.tempChatRepository.addDeleteRequestV2(chatId, userId);
			const requesters = await this.tempChatRepository.listDeleteRequestUserIdsV2(chatId);
			const chat = await this.tempChatRepository.getByChatId(chatId);
			const bothAgreed = chat && requesters.length >= 2 && requesters.includes(chat.user_id_1) && requesters.includes(chat.user_id_2);
			if (bothAgreed) {
				await this.tempChatRepository.clearDeleteRequestsV2(chatId);
				await this.tempChatRepository.deleteByChatId(chatId);
				return {deleted: true};
			}
			return {deleted: false};
		}
		await this.tempChatRepository.addDeleteRequest(pair[0], pair[1], userId);
		const requesters = await this.tempChatRepository.listDeleteRequestUserIds(pair[0], pair[1]);
		const bothAgreed = requesters.length >= 2 && requesters.includes(pair[0]) && requesters.includes(pair[1]);
		if (bothAgreed) {
			await this.tempChatRepository.clearDeleteRequests(pair[0], pair[1]);
			await this.tempChatRepository.delete(pair[0], pair[1]);
			return {deleted: true};
		}
		return {deleted: false};
	}
}
