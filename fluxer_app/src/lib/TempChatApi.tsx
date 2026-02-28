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
import {HttpError} from '@app/lib/HttpError';
import http from '@app/lib/HttpClient';

export interface TempChatSummary {
	id: string;
	participant_ids: [string, string];
	created_at: string;
}

export interface TempChatMessagePayload {
	id: string;
	sender_id: string;
	ciphertext: string;
	iv: string;
	ephemeral_public_key: string;
	created_at: string;
}

export async function getMyE2EKey(): Promise<string | null> {
	const res = await http.get<{public_key_base64: string}>({url: Endpoints.USER_E2E_KEY});
	if (!res.ok || res.body == null) return null;
	return res.body.public_key_base64 ?? null;
}

export async function setMyE2EKey(publicKeyBase64: string): Promise<void> {
	await http.post({
		url: Endpoints.USER_E2E_KEY,
		body: {public_key: publicKeyBase64},
	});
}

/** Returns the other user's E2E public key, or null if 404/not set (so we can show a friendly message instead of throwing). */
export async function getOtherUserE2EKey(userId: string): Promise<string | null> {
	const res = await http.get<{public_key_base64: string}>({
		url: Endpoints.USER_E2E_KEY_BY_USER(userId),
		rejectWithError: false,
	});
	if (!res.ok || res.body == null) return null;
	return res.body.public_key_base64 ?? null;
}

function extractTempChatErrorMessage(err: unknown): string {
	if (err instanceof HttpError && err.body && typeof err.body === 'object') {
		const body = err.body as {error?: string; detail?: string};
		// Prefer detail in dev (server includes actual error); fallback to error code
		return body.detail ?? body.error ?? 'Failed to create temp chat';
	}
	return 'Failed to create temp chat';
}

export async function createOrGetTempChat(recipientId: string): Promise<TempChatSummary> {
	try {
		const res = await http.post<TempChatSummary | {error?: string; detail?: string}>({
			url: Endpoints.USER_TEMP_CHATS,
			body: {recipient_id: String(recipientId)},
		});
		if (!res.ok || !res.body) {
			const body = res.body && typeof res.body === 'object' ? (res.body as {error?: string; detail?: string}) : null;
			const message = body?.detail ?? body?.error ?? 'Failed to create temp chat';
			throw new Error(message);
		}
		if ('error' in res.body) {
			const body = res.body as {error: string; detail?: string};
			throw new Error(body.detail ?? body.error);
		}
		return res.body as TempChatSummary;
	} catch (err) {
		if (err instanceof Error && err.message !== 'Failed to create temp chat' && !(err instanceof HttpError)) {
			throw err;
		}
		throw new Error(extractTempChatErrorMessage(err));
	}
}

export async function listTempChats(): Promise<Array<TempChatSummary>> {
	const res = await http.get<Array<TempChatSummary>>({url: Endpoints.USER_TEMP_CHATS});
	if (!res.ok) return [];
	return res.body ?? [];
}

export async function getTempChatMessages(tempChatId: string): Promise<Array<TempChatMessagePayload>> {
	const res = await http.get<Array<TempChatMessagePayload>>({
		url: Endpoints.USER_TEMP_CHAT_MESSAGES(tempChatId),
	});
	if (!res.ok) return [];
	return res.body ?? [];
}

export async function sendTempChatMessage(
	tempChatId: string,
	payload: {ciphertext: string; iv: string; ephemeral_public_key: string},
): Promise<{id: string; created_at: string}> {
	const res = await http.post<{id: string; created_at: string}>({
		url: Endpoints.USER_TEMP_CHAT_MESSAGES(tempChatId),
		body: payload,
	});
	if (!res.ok || !res.body) throw new Error((res.body as {error?: string})?.error ?? 'Failed to send message');
	return res.body;
}

export async function deleteTempChat(tempChatId: string): Promise<void> {
	await http.delete({url: Endpoints.USER_TEMP_CHAT(tempChatId)});
}

/** Request delete; chat is only deleted when both participants have requested. Returns true if already deleted. */
export async function requestDeleteTempChat(tempChatId: string): Promise<{deleted: boolean}> {
	const res = await http.post<{deleted: boolean}>({
		url: Endpoints.USER_TEMP_CHAT_REQUEST_DELETE(tempChatId),
	});
	if (!res.ok || !res.body) throw new Error((res.body as {error?: string})?.error ?? 'Failed to request delete');
	return res.body;
}

/** Dev only: provision E2E key for the other participant so you can send without them opening the chat. 404 in production. */
export async function provisionRecipientKeyForTesting(tempChatId: string): Promise<void> {
	const res = await http.post({
		url: Endpoints.USER_TEMP_CHAT_PROVISION_RECIPIENT_KEY(tempChatId),
		rejectWithError: false,
	});
	if (!res.ok) throw new Error((res.body as {error?: string})?.error ?? 'Failed to provision (dev only)');
}

/** Dev only: get server-provisioned private key if any. 404 in production or when none. */
export async function getMyE2EPrivateKey(): Promise<string | null> {
	const res = await http.get<{private_key_base64: string}>({
		url: Endpoints.USER_E2E_PRIVATE_KEY,
		rejectWithError: false,
	});
	if (!res.ok || res.body == null) return null;
	return res.body.private_key_base64 ?? null;
}
