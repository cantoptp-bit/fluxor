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
import type {TempChatMessageRow, TempChatMessageRowV2} from '@fluxer/api/src/database/types/TempChatTypes';

export interface ITempChatMessageRepository {
	insert(row: TempChatMessageRow): Promise<void>;
	listByChat(userId1: UserID, userId2: UserID): Promise<Array<TempChatMessageRow>>;
	/** Delete all messages for this temp chat (by partition). */
	deleteAllForChat(userId1: UserID, userId2: UserID): Promise<void>;
	/** V2: Insert message for chat_id. */
	insertV2(row: TempChatMessageRowV2): Promise<void>;
	/** V2: List messages for chat_id. */
	listByChatIdV2(chatId: bigint): Promise<Array<TempChatMessageRowV2>>;
	/** V2: Delete all messages for this chat_id. */
	deleteAllForChatV2(chatId: bigint): Promise<void>;
}
