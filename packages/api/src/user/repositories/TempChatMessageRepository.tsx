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
import {deleteOneOrMany, fetchMany, upsertOne} from '@fluxer/api/src/database/Cassandra';
import type {TempChatMessageRow, TempChatMessageRowV2} from '@fluxer/api/src/database/types/TempChatTypes';
import {TempChatMessages, TempChatMessagesV2} from '@fluxer/api/src/Tables';
import type {ITempChatMessageRepository} from '@fluxer/api/src/user/repositories/ITempChatMessageRepository';

const LIST_MESSAGES_CQL = TempChatMessages.selectCql({
	where: [TempChatMessages.where.eq('user_id_1'), TempChatMessages.where.eq('user_id_2')],
});

const LIST_MESSAGES_V2_CQL = TempChatMessagesV2.selectCql({
	where: TempChatMessagesV2.where.eq('chat_id'),
});

export class TempChatMessageRepository implements ITempChatMessageRepository {
	async insert(row: TempChatMessageRow): Promise<void> {
		await upsertOne(TempChatMessages.upsertAll(row));
	}

	async listByChat(userId1: UserID, userId2: UserID): Promise<Array<TempChatMessageRow>> {
		const rows = await fetchMany<TempChatMessageRow>(LIST_MESSAGES_CQL, {
			user_id_1: userId1 as bigint,
			user_id_2: userId2 as bigint,
		});
		return rows.map((r) => ({
			user_id_1: createUserID(r.user_id_1),
			user_id_2: createUserID(r.user_id_2),
			message_id: r.message_id,
			sender_id: createUserID(r.sender_id),
			ciphertext: r.ciphertext,
			iv: r.iv,
			ephemeral_public_key: r.ephemeral_public_key,
			created_at: r.created_at,
		}));
	}

	async deleteAllForChat(userId1: UserID, userId2: UserID): Promise<void> {
		await deleteOneOrMany(
			TempChatMessages.deletePartition({
				user_id_1: userId1,
				user_id_2: userId2,
			}),
		);
	}

	async insertV2(row: TempChatMessageRowV2): Promise<void> {
		await upsertOne(TempChatMessagesV2.upsertAll(row));
	}

	async listByChatIdV2(chatId: bigint): Promise<Array<TempChatMessageRowV2>> {
		const rows = await fetchMany<TempChatMessageRowV2>(LIST_MESSAGES_V2_CQL, {chat_id: chatId});
		return rows.map((r) => ({
			chat_id: r.chat_id,
			message_id: r.message_id,
			sender_id: createUserID(r.sender_id),
			ciphertext: r.ciphertext,
			iv: r.iv,
			ephemeral_public_key: r.ephemeral_public_key,
			created_at: r.created_at,
		}));
	}

	async deleteAllForChatV2(chatId: bigint): Promise<void> {
		await deleteOneOrMany(TempChatMessagesV2.deletePartition({chat_id: chatId}));
	}
}
