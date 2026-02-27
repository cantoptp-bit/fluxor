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

import type {UserID} from '@fluxer/api/src/BrandedTypes';
import type {IUserRepository} from '@fluxer/api/src/user/IUserRepository';
import type {TempChatService} from '@fluxer/api/src/user/services/TempChatService';
import type {UserE2EKeyService} from '@fluxer/api/src/user/services/UserE2EKeyService';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';

export interface TempChatSummary {
	id: string;
	participant_ids: [UserID, UserID];
	created_at: Date;
}

export interface TempChatMessagePayload {
	id: string;
	sender_id: UserID;
	ciphertext: string;
	iv: string;
	ephemeral_public_key: string;
	created_at: Date;
}

export class TempChatRequestService {
	constructor(
		private readonly tempChatService: TempChatService,
		private readonly userE2EKeyService: UserE2EKeyService,
		private readonly userRepository: IUserRepository,
	) {}

	async getMyPublicKey(userId: UserID): Promise<string | null> {
		return this.userE2EKeyService.getPublicKey(userId);
	}

	async setMyPublicKey(userId: UserID, publicKeyBase64: string): Promise<void> {
		await this.userE2EKeyService.setPublicKey(userId, publicKeyBase64);
	}

	/** Get another user's public key; only if caller is friends with that user. */
	async getOtherUserPublicKey(callerUserId: UserID, otherUserId: UserID): Promise<string | null> {
		const friendship = await this.userRepository.getRelationship(callerUserId, otherUserId, RelationshipTypes.FRIEND);
		if (!friendship) return null;
		return this.userE2EKeyService.getPublicKey(otherUserId);
	}

	/** Creates a new temp chat (unique id). Use for multiple temp chats per user pair. */
	async createTempChat(userId: UserID, recipientId: UserID): Promise<TempChatSummary> {
		const chat = await this.tempChatService.create(userId, recipientId);
		return {
			id: chat.id,
			participant_ids: [chat.user_id_1, chat.user_id_2],
			created_at: chat.created_at,
		};
	}

	/** Legacy: create or return existing one-per-pair chat. Prefer createTempChat for new flows. */
	async createOrGetTempChat(userId: UserID, recipientId: UserID): Promise<TempChatSummary> {
		const chat = await this.tempChatService.createOrGet(userId, recipientId);
		return {
			id: chat.id,
			participant_ids: [chat.user_id_1, chat.user_id_2],
			created_at: chat.created_at,
		};
	}

	async listTempChats(userId: UserID): Promise<Array<TempChatSummary>> {
		const chats = await this.tempChatService.listByUserId(userId);
		return chats.map((c) => ({
			id: c.id,
			participant_ids: [c.user_id_1, c.user_id_2],
			created_at: c.created_at,
		}));
	}

	async getMessages(userId: UserID, tempChatId: string): Promise<Array<TempChatMessagePayload>> {
		const rows = await this.tempChatService.getMessages(userId, tempChatId);
		return rows.map((r) => ({
			id: r.message_id.toString(),
			sender_id: r.sender_id,
			ciphertext: r.ciphertext,
			iv: r.iv,
			ephemeral_public_key: r.ephemeral_public_key,
			created_at: r.created_at,
		}));
	}

	async sendMessage(
		userId: UserID,
		tempChatId: string,
		payload: {ciphertext: string; iv: string; ephemeral_public_key: string},
	): Promise<{id: string; created_at: Date}> {
		const result = await this.tempChatService.sendMessage(userId, tempChatId, payload);
		return {id: result.message_id.toString(), created_at: result.created_at};
	}

	async deleteTempChat(userId: UserID, tempChatId: string): Promise<void> {
		await this.tempChatService.deleteChat(userId, tempChatId);
	}

	/** Request delete; chat is only deleted when both participants have requested. Returns true if deleted. */
	async requestDeleteTempChat(userId: UserID, tempChatId: string): Promise<{deleted: boolean}> {
		return this.tempChatService.requestDelete(userId, tempChatId);
	}
}
