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

import { createUserID, type UserID } from '@fluxer/api/src/BrandedTypes';
import { DefaultUserOnly, LoginRequired } from '@fluxer/api/src/middleware/AuthMiddleware';
import type { HonoApp } from '@fluxer/api/src/types/HonoEnv';
import { RelationshipTypes } from '@fluxer/constants/src/UserConstants';
import { UserRelationshipRepository } from '@fluxer/api/src/user/repositories/UserRelationshipRepository';
import { z } from 'zod';

const DEV_USERS_LIMIT = 200;
const DevUsersQuery = z.object({
	limit: z.coerce.number().min(1).max(500).optional().default(DEV_USERS_LIMIT),
	after: z.string().optional(),
});
const DevAddFriendsBody = z.object({
	user_ids: z.array(z.string()).max(100),
});

export function DevController(app: HonoApp): void {
	app.get(
		'/_dev/users',
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const query = DevUsersQuery.safeParse(ctx.req.query());
			if (!query.success) {
				return ctx.json({ error: query.error.message }, 400);
			}
			const { limit, after } = query.data;
			const lastUserId = after != null ? createUserID(BigInt(after)) : undefined;
			const userRepository = ctx.get('userRepository');
			const users = await userRepository.listAllUsersPaginated(limit, lastUserId);
			const list = users.map((u) => ({
				id: String(u.id),
				username: u.username,
				discriminator: u.discriminator,
				global_name: u.globalName ?? null,
			}));
			return ctx.json({ users: list, next_after: list.length === limit ? list[list.length - 1]?.id : null });
		},
	);

	app.post(
		'/_dev/relationships/friends',
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const bodyResult = DevAddFriendsBody.safeParse(await ctx.req.json());
			if (!bodyResult.success) {
				return ctx.json({ error: bodyResult.error.message }, 400);
			}
			const me = ctx.get('user');
			const userRepository = ctx.get('userRepository');
			const relationshipRepository = new UserRelationshipRepository();
			let added = 0;
			const errors: Array<{ user_id: string; error: string }> = [];
			const now = new Date();
			for (const idStr of bodyResult.data.user_ids) {
				if (idStr === String(me.id)) continue;
				let targetId: UserID;
				try {
					targetId = createUserID(BigInt(idStr));
				} catch {
					errors.push({ user_id: idStr, error: 'invalid id' });
					continue;
				}
				try {
					const targetUser = await userRepository.findUnique(targetId);
					if (!targetUser) {
						errors.push({ user_id: idStr, error: 'user not found' });
						continue;
					}
					// Remove any block in both directions so we can add friend
					await relationshipRepository.deleteRelationship(me.id, targetId, RelationshipTypes.BLOCKED);
					await relationshipRepository.deleteRelationship(targetId, me.id, RelationshipTypes.BLOCKED);
					const existing = await relationshipRepository.getRelationship(me.id, targetId, RelationshipTypes.FRIEND);
					if (existing) continue;
					await relationshipRepository.upsertRelationship({
						source_user_id: me.id,
						target_user_id: targetId,
						type: RelationshipTypes.FRIEND,
						nickname: null,
						since: now,
						version: 1,
					});
					await relationshipRepository.upsertRelationship({
						source_user_id: targetId,
						target_user_id: me.id,
						type: RelationshipTypes.FRIEND,
						nickname: null,
						since: now,
						version: 1,
					});
					added += 1;
				} catch (err) {
					errors.push({ user_id: idStr, error: (err as Error).message });
				}
			}
			return ctx.json({ added, errors });
		},
	);
}
