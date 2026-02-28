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
import type {UserE2EKeyRow} from '@fluxer/api/src/database/types/TempChatTypes';
import type {IUserE2EKeyRepository} from '@fluxer/api/src/user/repositories/IUserE2EKeyRepository';

export class UserE2EKeyService {
	constructor(private readonly userE2EKeyRepository: IUserE2EKeyRepository) {}

	async getPublicKey(userId: UserID): Promise<string | null> {
		const row = await this.userE2EKeyRepository.getByUserId(userId);
		return row?.public_key_base64 ?? null;
	}

	async setPublicKey(userId: UserID, publicKeyBase64: string): Promise<void> {
		const row: UserE2EKeyRow = {
			user_id: userId,
			public_key_base64: publicKeyBase64,
			created_at: new Date(),
		};
		await this.userE2EKeyRepository.upsert(row);
	}
}
