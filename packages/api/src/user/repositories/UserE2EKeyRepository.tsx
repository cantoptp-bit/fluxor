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
import {fetchOne, upsertOne} from '@fluxer/api/src/database/Cassandra';
import type {UserE2EKeyRow} from '@fluxer/api/src/database/types/TempChatTypes';
import {UserE2EKeys} from '@fluxer/api/src/Tables';
import type {IUserE2EKeyRepository} from '@fluxer/api/src/user/repositories/IUserE2EKeyRepository';

const FETCH_KEY_CQL = UserE2EKeys.selectCql({
	where: UserE2EKeys.where.eq('user_id'),
	limit: 1,
});

export class UserE2EKeyRepository implements IUserE2EKeyRepository {
	async getByUserId(userId: UserID): Promise<UserE2EKeyRow | null> {
		const row = await fetchOne<UserE2EKeyRow>(FETCH_KEY_CQL, {user_id: userId as bigint});
		if (!row) return null;
		return {
			user_id: createUserID(row.user_id),
			public_key_base64: row.public_key_base64,
			created_at: row.created_at,
		};
	}

	async upsert(row: UserE2EKeyRow): Promise<void> {
		await upsertOne(UserE2EKeys.upsertAll(row));
	}
}
