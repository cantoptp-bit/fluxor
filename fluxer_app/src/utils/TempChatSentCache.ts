/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

export const SENT_PLAINTEXT_PREFIX = 'fluxer_temp_sent:';

/** Storage key for a sent message's plaintext (for testing and consistency). */
export function getSentPlaintextKey(tempChatId: string, messageId: string): string {
	return `${SENT_PLAINTEXT_PREFIX}${tempChatId}:${messageId}`;
}

export function getSentPlaintext(tempChatId: string, messageId: string): string | null {
	if (typeof sessionStorage === 'undefined') return null;
	return sessionStorage.getItem(getSentPlaintextKey(tempChatId, messageId));
}

export function setSentPlaintext(tempChatId: string, messageId: string, text: string): void {
	if (typeof sessionStorage === 'undefined') return;
	sessionStorage.setItem(getSentPlaintextKey(tempChatId, messageId), text);
}

export interface RawTempChatMessage {
	id: string;
	sender_id: string;
	ciphertext: string;
	iv: string;
	ephemeral_public_key: string;
	created_at: string;
}

export interface DecryptedMessage {
	id: string;
	senderId: string;
	text: string;
	createdAt: string;
}

export interface ProcessTempChatMessagesResult {
	messages: Array<DecryptedMessage>;
	decryptErrorCount: number;
}

/**
 * Process raw temp chat messages: use sent-message cache for own messages (never decrypt),
 * decrypt only messages from others. Ensures "some messages can't be decrypted" never
 * appears for messages the user sent (they are encrypted for the recipient, not for us).
 */
export async function processTempChatMessages(
	raw: Array<RawTempChatMessage>,
	currentUserId: string,
	tempChatId: string,
	placeholderForOwn: string,
	decrypt: (payload: {
		ciphertext: string;
		iv: string;
		ephemeralPublicKey: string;
	}) => Promise<string>,
): Promise<ProcessTempChatMessagesResult> {
	const messages: Array<DecryptedMessage> = [];
	let decryptErrorCount = 0;
	for (const m of raw) {
		const isOwn = m.sender_id === currentUserId;
		if (isOwn) {
			const cached = getSentPlaintext(tempChatId, m.id);
			messages.push({
				id: m.id,
				senderId: m.sender_id,
				text: cached ?? placeholderForOwn,
				createdAt: m.created_at,
			});
			continue;
		}
		try {
			const text = await decrypt({
				ciphertext: m.ciphertext,
				iv: m.iv,
				ephemeralPublicKey: m.ephemeral_public_key,
			});
			messages.push({
				id: m.id,
				senderId: m.sender_id,
				text,
				createdAt: m.created_at,
			});
		} catch {
			decryptErrorCount += 1;
		}
	}
	return { messages, decryptErrorCount };
}
