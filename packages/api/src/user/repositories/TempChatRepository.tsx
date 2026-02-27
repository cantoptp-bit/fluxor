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
import {BatchBuilder, deleteOneOrMany, fetchMany, fetchOne, upsertOne} from '@fluxer/api/src/database/Cassandra';
import type {TempChatRow} from '@fluxer/api/src/database/types/TempChatTypes';
import {TempChatDeleteRequests, TempChatMessages, TempChats, TempChatsByUser} from '@fluxer/api/src/Tables';
import type {ITempChatRepository} from '@fluxer/api/src/user/repositories/ITempChatRepository';

function canonical(a: UserID, b: UserID): [UserID, UserID] {
	return a < b ? [a, b] : [b, a];
}

const GET_CHAT_CQL = TempChats.selectCql({
	where: [TempChats.where.eq('user_id_1'), TempChats.where.eq('user_id_2')],
	limit: 1,
});

const LIST_BY_USER_CQL = TempChatsByUser.selectCql({
	where: TempChatsByUser.where.eq('user_id'),
});

const LIST_DELETE_REQUESTS_CQL = TempChatDeleteRequests.selectCql({
	where: [
		TempChatDeleteRequests.where.eq('user_id_1'),
		TempChatDeleteRequests.where.eq('user_id_2'),
	],
});

export class TempChatRepository implements ITempChatRepository {
	async createOrGet(userId1: UserID, userId2: UserID): Promise<TempChatRow> {
		const [lo, hi] = canonical(userId1, userId2);
		const existing = await fetchOne<TempChatRow>(GET_CHAT_CQL, {
			user_id_1: lo as bigint,
			user_id_2: hi as bigint,
		});
		if (existing) {
			return {
				user_id_1: createUserID(existing.user_id_1),
				user_id_2: createUserID(existing.user_id_2),
				created_at: existing.created_at,
			};
		}
		const now = new Date();
		const row: TempChatRow = {
			user_id_1: lo,
			user_id_2: hi,
			created_at: now,
		};
		const batch = new BatchBuilder();
		batch.addPrepared(TempChats.upsertAll(row));
		batch.addPrepared(
			TempChatsByUser.upsertAll({
				user_id: lo,
				user_id_1: lo,
				user_id_2: hi,
			}),
		);
		batch.addPrepared(
			TempChatsByUser.upsertAll({
				user_id: hi,
				user_id_1: lo,
				user_id_2: hi,
			}),
		);
		await batch.execute();
		return row;
	}

	async getByUserIds(userId1: UserID, userId2: UserID): Promise<TempChatRow | null> {
		const [lo, hi] = canonical(userId1, userId2);
		const row = await fetchOne<TempChatRow>(GET_CHAT_CQL, {
			user_id_1: lo as bigint,
			user_id_2: hi as bigint,
		});
		if (!row) return null;
		return {
			user_id_1: createUserID(row.user_id_1),
			user_id_2: createUserID(row.user_id_2),
			created_at: row.created_at,
		};
	}

	async listByUserId(userId: UserID): Promise<Array<TempChatRow>> {
		const byUserRows = await fetchMany<{user_id: bigint; user_id_1: bigint; user_id_2: bigint}>(
			LIST_BY_USER_CQL,
			{user_id: userId as bigint},
		);
		const result: Array<TempChatRow> = [];
		for (const r of byUserRows) {
			const chat = await this.getByUserIds(createUserID(r.user_id_1), createUserID(r.user_id_2));
			if (chat) result.push(chat);
		}
		return result;
	}

	async delete(userId1: UserID, userId2: UserID): Promise<void> {
		const [lo, hi] = canonical(userId1, userId2);
		await deleteOneOrMany(TempChatMessages.deletePartition({user_id_1: lo, user_id_2: hi}));
		await deleteOneOrMany(TempChats.deleteByPk({user_id_1: lo, user_id_2: hi}));
		await deleteOneOrMany(TempChatsByUser.deleteByPk({user_id: lo, user_id_1: lo, user_id_2: hi}));
		await deleteOneOrMany(TempChatsByUser.deleteByPk({user_id: hi, user_id_1: lo, user_id_2: hi}));
	}

	async addDeleteRequest(userId1: UserID, userId2: UserID, userId: UserID): Promise<void> {
		const [lo, hi] = canonical(userId1, userId2);
		await upsertOne(TempChatDeleteRequests.upsertAll({user_id_1: lo, user_id_2: hi, user_id: userId}));
	}

	async listDeleteRequestUserIds(userId1: UserID, userId2: UserID): Promise<Array<UserID>> {
		const [lo, hi] = canonical(userId1, userId2);
		const rows = await fetchMany<{user_id: bigint}>(
			LIST_DELETE_REQUESTS_CQL,
			{user_id_1: lo as bigint, user_id_2: hi as bigint},
		);
		return rows.map((r) => createUserID(r.user_id));
	}

	async clearDeleteRequests(userId1: UserID, userId2: UserID): Promise<void> {
		const [lo, hi] = canonical(userId1, userId2);
		await deleteOneOrMany(TempChatDeleteRequests.deletePartition({user_id_1: lo, user_id_2: hi}));
	}
}
