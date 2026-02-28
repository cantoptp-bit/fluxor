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

import {Endpoints} from '@app/Endpoints';
import http from '@app/lib/HttpClient';
import {Logger} from '@app/lib/Logger';
import RelationshipStore from '@app/stores/RelationshipStore';
import type {Relationship} from '@app/records/RelationshipRecord';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';

const logger = new Logger('RelationshipActionCreators');

/** Refetch relationships from the API and update the store. Use when Ready payload may be stale (e.g. after seed). */
export async function fetchRelationships(): Promise<void> {
	try {
		const response = await http.get<Array<Relationship>>({url: Endpoints.USER_RELATIONSHIPS});
		const relationships = response?.body ?? [];
		RelationshipStore.loadRelationships(relationships);
	} catch (error) {
		logger.error('Failed to fetch relationships:', error);
	}
}

export async function sendFriendRequest(userId: string) {
	try {
		await http.post({url: Endpoints.USER_RELATIONSHIP(userId)});
	} catch (error) {
		logger.error('Failed to send friend request:', error);
		throw error;
	}
}

export async function sendFriendRequestByTag(username: string, discriminator: string) {
	try {
		await http.post({url: Endpoints.USER_RELATIONSHIPS, body: {username, discriminator}});
	} catch (error) {
		logger.error('Failed to send friend request by tag:', error);
		throw error;
	}
}

export async function acceptFriendRequest(userId: string) {
	try {
		await http.put({url: Endpoints.USER_RELATIONSHIP(userId)});
	} catch (error) {
		logger.error('Failed to accept friend request:', error);
		throw error;
	}
}

export async function removeRelationship(userId: string) {
	try {
		await http.delete({url: Endpoints.USER_RELATIONSHIP(userId)});
	} catch (error) {
		logger.error('Failed to remove relationship:', error);
		throw error;
	}
}

export async function blockUser(userId: string) {
	try {
		await http.put({url: Endpoints.USER_RELATIONSHIP(userId), body: {type: RelationshipTypes.BLOCKED}});
	} catch (error) {
		logger.error('Failed to block user:', error);
		throw error;
	}
}

export async function updateFriendNickname(userId: string, nickname: string | null) {
	try {
		await http.patch({url: Endpoints.USER_RELATIONSHIP(userId), body: {nickname}});
	} catch (error) {
		logger.error('Failed to update friend nickname:', error);
		throw error;
	}
}

/** Dev only: list users (paginated). */
export async function fetchDevUsers(limit = 200, after?: string): Promise<{
	users: Array<{id: string; username: string; discriminator: number; global_name: string | null}>;
	next_after: string | null;
}> {
	const query: Record<string, string> = {limit: String(limit)};
	if (after != null) query.after = after;
	const response = await http.get<{users: Array<{id: string; username: string; discriminator: number; global_name: string | null}>; next_after: string | null}>({
		url: Endpoints.DEV_USERS,
		query,
	});
	return response.body;
}

/** Dev only: add users as friends by ID (skips self and already friends). */
export async function addDevFriends(userIds: Array<string>): Promise<{added: number; errors: Array<{user_id: string; error: string}>}> {
	const response = await http.post<{added: number; errors: Array<{user_id: string; error: string}>}>({
		url: Endpoints.DEV_ADD_FRIENDS,
		body: {user_ids: userIds},
	});
	return response.body;
}
