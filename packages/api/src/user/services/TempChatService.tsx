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
import type {TempChatMessageRow} from '@fluxer/api/src/database/types/TempChatTypes';
import type {IUserRepository} from '@fluxer/api/src/user/IUserRepository';
import type {ITempChatMessageRepository} from '@fluxer/api/src/user/repositories/ITempChatMessageRepository';
import type {ITempChatRepository} from '@fluxer/api/src/user/repositories/ITempChatRepository';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import type {SnowflakeService} from '@fluxer/api/src/infrastructure/SnowflakeService';

export function tempChatIdFromPair(userId1: UserID, userId2: UserID): string {
	const [lo, hi] = userId1 < userId2 ? [userId1, userId2] : [userId2, userId1];
	return `${lo}_${hi}`;
}

export function parseTempChatId(id: string): [UserID, UserID] | null {
	const parts = id.split('_');
	if (parts.length !== 2) return null;
	const lo = BigInt(parts[0]);
	const hi = BigInt(parts[1]);
	if (lo >= hi) return null;
	return [createUserID(lo), createUserID(hi)];
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

	async listByUserId(userId: UserID): Promise<Array<{id: string; user_id_1: UserID; user_id_2: UserID; created_at: Date}>> {
		const rows = await this.tempChatRepository.listByUserId(userId);
		return rows.map((r) => ({
			id: tempChatIdFromPair(r.user_id_1, r.user_id_2),
			user_id_1: r.user_id_1,
			user_id_2: r.user_id_2,
			created_at: r.created_at,
		}));
	}

	/** Throws if tempChatId invalid or user is not a participant. */
	async assertParticipant(userId: UserID, tempChatId: string): Promise<[UserID, UserID]> {
		const pair = parseTempChatId(tempChatId);
		if (!pair) throw new Error('TEMP_CHAT_NOT_FOUND');
		const [lo, hi] = pair;
		if (userId !== lo && userId !== hi) throw new Error('TEMP_CHAT_FORBIDDEN');
		return pair;
	}

	async getMessages(userId: UserID, tempChatId: string): Promise<Array<TempChatMessageRow>> {
		const [userId1, userId2] = await this.assertParticipant(userId, tempChatId);
		return this.tempChatMessageRepository.listByChat(userId1, userId2);
	}

	async sendMessage(
		senderId: UserID,
		tempChatId: string,
		payload: {ciphertext: string; iv: string; ephemeral_public_key: string},
	): Promise<{message_id: bigint; created_at: Date}> {
		const [userId1, userId2] = await this.assertParticipant(senderId, tempChatId);
		const messageId = await this.snowflakeService.generate();
		const now = new Date();
		const row: TempChatMessageRow = {
			user_id_1: userId1,
			user_id_2: userId2,
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
		const [userId1, userId2] = await this.assertParticipant(userId, tempChatId);
		await this.tempChatRepository.delete(userId1, userId2);
	}

	/**
	 * Record this user's request to delete. If both participants have requested, deletes the chat.
	 * Returns true if the chat was actually deleted, false if only the request was recorded.
	 */
	async requestDelete(userId: UserID, tempChatId: string): Promise<{deleted: boolean}> {
		const [userId1, userId2] = await this.assertParticipant(userId, tempChatId);
		await this.tempChatRepository.addDeleteRequest(userId1, userId2, userId);
		const requesters = await this.tempChatRepository.listDeleteRequestUserIds(userId1, userId2);
		const bothAgreed = requesters.length >= 2 && requesters.includes(userId1) && requesters.includes(userId2);
		if (bothAgreed) {
			await this.tempChatRepository.clearDeleteRequests(userId1, userId2);
			await this.tempChatRepository.delete(userId1, userId2);
			return {deleted: true};
		}
		return {deleted: false};
	}
}
