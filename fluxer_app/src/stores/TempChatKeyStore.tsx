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

import {
	exportPrivateKeyBase64,
	generateKeyPair,
	importPrivateKeyFromBase64,
	importPublicKey,
	type X25519KeyPair,
} from '@app/lib/E2EEncryption';
import * as TempChatApi from '@app/lib/TempChatApi';

const STORAGE_KEY_PREFIX = 'temp_chat_private_key_';

/**
 * Get or create X25519 key pair for the current user.
 * Private key is stored in sessionStorage; public key is synced to the server.
 * Call this before opening/sending in a temp chat.
 */
export async function getOrCreateKeyPair(userId: string): Promise<X25519KeyPair> {
	const storageKey = `${STORAGE_KEY_PREFIX}${userId}`;
	const stored = typeof sessionStorage !== 'undefined' ? sessionStorage.getItem(storageKey) : null;

	if (stored) {
		try {
			const privateKey = await importPrivateKeyFromBase64(stored);
			const serverKey = await TempChatApi.getMyE2EKey();
			if (serverKey) {
				const publicKey = await importPublicKey(serverKey);
				return {
					privateKey,
					publicKey,
					publicKeyBase64: serverKey,
				};
			}
		} catch {
			// Invalid or outdated stored key; regenerate below.
		}
	}

	const keyPair = await generateKeyPair();
	const privateKeyBase64 = await exportPrivateKeyBase64(keyPair.privateKey);
	if (typeof sessionStorage !== 'undefined') {
		sessionStorage.setItem(storageKey, privateKeyBase64);
	}
	await TempChatApi.setMyE2EKey(keyPair.publicKeyBase64);
	return keyPair;
}

/**
 * Fetch another user's E2E public key (only works if the current user is friends with them).
 */
export async function getRecipientPublicKey(recipientUserId: string): Promise<string | null> {
	return TempChatApi.getOtherUserE2EKey(recipientUserId);
}
