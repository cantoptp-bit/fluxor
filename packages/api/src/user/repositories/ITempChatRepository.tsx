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
import type {TempChatRow, TempChatRowV2} from '@fluxer/api/src/database/types/TempChatTypes';

export interface TempChatSummary {
	id: string;
	user_id_1: UserID;
	user_id_2: UserID;
	created_at: Date;
}

export interface ITempChatRepository {
	/** Create temp chat with canonical order (user_id_1 < user_id_2). Returns existing if present. */
	createOrGet(userId1: UserID, userId2: UserID): Promise<TempChatRow>;
	getByUserIds(userId1: UserID, userId2: UserID): Promise<TempChatRow | null>;
	listByUserId(userId: UserID): Promise<Array<TempChatRow>>;
	/** V2: Create a new temp chat with unique chat_id (snowflake). Always creates; use for multiple chats per pair. */
	createV2(userId1: UserID, userId2: UserID, chatId: bigint): Promise<TempChatRowV2>;
	/** V2: Get chat by snowflake chat_id. */
	getByChatId(chatId: bigint): Promise<TempChatRowV2 | null>;
	/** V2: List all temp chats for this user (from by-user index). */
	listByUserIdV2(userId: UserID): Promise<Array<TempChatRowV2>>;
	/** Hard-delete chat and all messages and by-user index rows. */
	delete(userId1: UserID, userId2: UserID): Promise<void>;
	/** V2: Hard-delete chat by chat_id. */
	deleteByChatId(chatId: bigint): Promise<void>;
	/** Record that this user requested delete. */
	addDeleteRequest(userId1: UserID, userId2: UserID, userId: UserID): Promise<void>;
	/** V2: Record that this user requested delete for chat_id. */
	addDeleteRequestV2(chatId: bigint, userId: UserID): Promise<void>;
	/** List user IDs who have requested delete for this chat. */
	listDeleteRequestUserIds(userId1: UserID, userId2: UserID): Promise<Array<UserID>>;
	/** V2: List user IDs who requested delete for this chat_id. */
	listDeleteRequestUserIdsV2(chatId: bigint): Promise<Array<UserID>>;
	/** Clear all delete requests for this chat. */
	clearDeleteRequests(userId1: UserID, userId2: UserID): Promise<void>;
	/** V2: Clear all delete requests for this chat_id. */
	clearDeleteRequestsV2(chatId: bigint): Promise<void>;
}
