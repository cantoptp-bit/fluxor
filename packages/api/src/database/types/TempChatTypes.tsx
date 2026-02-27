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

export interface TempChatRow {
	user_id_1: UserID;
	user_id_2: UserID;
	created_at: Date;
}

export const TEMP_CHAT_COLUMNS = ['user_id_1', 'user_id_2', 'created_at'] as const satisfies ReadonlyArray<
	keyof TempChatRow
>;

/** Lookup: (user_id) -> list of (user_id_1, user_id_2) for temp chats the user participates in */
export interface TempChatByUserRow {
	user_id: UserID;
	user_id_1: UserID;
	user_id_2: UserID;
}

export const TEMP_CHAT_BY_USER_COLUMNS = ['user_id', 'user_id_1', 'user_id_2'] as const satisfies ReadonlyArray<
	keyof TempChatByUserRow
>;

/** One row per user who requested delete for a temp chat. Both participants must have a row to actually delete. */
export interface TempChatDeleteRequestRow {
	user_id_1: UserID;
	user_id_2: UserID;
	user_id: UserID;
}

export const TEMP_CHAT_DELETE_REQUEST_COLUMNS = ['user_id_1', 'user_id_2', 'user_id'] as const satisfies ReadonlyArray<
	keyof TempChatDeleteRequestRow
>;

export interface TempChatMessageRow {
	user_id_1: UserID;
	user_id_2: UserID;
	message_id: bigint;
	sender_id: UserID;
	ciphertext: string;
	iv: string;
	ephemeral_public_key: string;
	created_at: Date;
}

export const TEMP_CHAT_MESSAGE_COLUMNS = [
	'user_id_1',
	'user_id_2',
	'message_id',
	'sender_id',
	'ciphertext',
	'iv',
	'ephemeral_public_key',
	'created_at',
] as const satisfies ReadonlyArray<keyof TempChatMessageRow>;

export interface UserE2EKeyRow {
	user_id: UserID;
	public_key_base64: string;
	created_at: Date;
}

export const USER_E2E_KEY_COLUMNS = ['user_id', 'public_key_base64', 'created_at'] as const satisfies ReadonlyArray<
	keyof UserE2EKeyRow
>;
