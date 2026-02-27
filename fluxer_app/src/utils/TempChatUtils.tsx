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

import * as TempChatApi from '@app/lib/TempChatApi';
import * as RouterUtils from '@app/utils/RouterUtils';
import {Routes} from '@app/Routes';
import type {UserRecord} from '@app/records/UserRecord';
import AuthenticationStore from '@app/stores/AuthenticationStore';
import * as TempChatLockStore from '@app/stores/TempChatLockStore';
import {getOrCreateKeyPair} from '@app/stores/TempChatKeyStore';

/**
 * Opens the temp chat as a full page for the given user.
 * If chatPassword is provided (e.g. from the "set password before create" modal), it is stored as the per-chat password.
 * Caller should catch and show a toast on error.
 * Users can have multiple temp chats with the same user; each requires its own password when created.
 *
 * We create the chat first so creation is never blocked by key setup (locked session or setMyE2EKey failure).
 * Key/password are best-effort after; if they fail, we still navigate so the user sees the chat (and unlock modal if needed).
 */
export async function openTempChatForUser(user: UserRecord, chatPassword?: string): Promise<void> {
	const currentUserId = AuthenticationStore.currentUserId;
	if (!currentUserId) {
		throw new Error('Not authenticated');
	}
	const chat = await TempChatApi.createOrGetTempChat(user.id);
	if (chatPassword?.trim()) {
		const keyPair = await getOrCreateKeyPair(currentUserId, chat.id);
		if (keyPair) {
			const ok = await TempChatLockStore.setChatPassword(currentUserId, chat.id, chatPassword.trim(), null);
			if (!ok) {
				throw new Error('Failed to set chat password');
			}
		}
		// If keyPair is null (locked), still navigate; user will see unlock modal on the chat page
	} else {
		try {
			await getOrCreateKeyPair(currentUserId);
		} catch {
			// Key setup failed (e.g. setMyE2EKey); still navigate so user can retry or unlock on the chat page
		}
	}
	RouterUtils.transitionTo(Routes.tempChat(chat.id));
}
