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
	importPrivateKeyFromBase64,
	importPublicKey,
	type X25519KeyPair,
} from '@app/lib/E2EEncryption';
import * as TempChatApi from '@app/lib/TempChatApi';

const MASTER_STORAGE_PREFIX = 'temp_chat_lock_master_';
const CHAT_STORAGE_PREFIX = 'temp_chat_lock_chat_';
const SESSION_KEY_PREFIX = 'temp_chat_private_key_';
const PBKDF2_ITERATIONS = 100_000;
const SALT_LENGTH = 16;
const IV_LENGTH = 12;
const AES_KEY_LENGTH = 256;

interface MasterBlob {
	version: number;
	saltB64: string;
	passwordHashB64: string;
	encryptedPrivateKeyB64: string;
	ivB64: string;
}

interface ChatBlob {
	version: number;
	saltB64: string;
	passwordHashB64: string;
	encryptedPrivateKeyB64: string;
	ivB64: string;
}

const storage = typeof localStorage !== 'undefined' ? localStorage : null;

function getMasterKey(userId: string): string {
	return `${MASTER_STORAGE_PREFIX}${userId}`;
}

function getChatKey(tempChatId: string): string {
	return `${CHAT_STORAGE_PREFIX}${tempChatId}`;
}

function getSessionKey(userId: string): string {
	return `${SESSION_KEY_PREFIX}${userId}`;
}

/** Copy Uint8Array to a plain ArrayBuffer for Web Crypto and base64 helpers. */
function toArrayBuffer(u: Uint8Array): ArrayBuffer {
	return u.buffer.slice(u.byteOffset, u.byteOffset + u.byteLength) as ArrayBuffer;
}

async function deriveKeyFromPassword(password: string, salt: Uint8Array): Promise<CryptoKey> {
	const enc = new TextEncoder();
	const keyMaterial = await crypto.subtle.importKey(
		'raw',
		enc.encode(password),
		{ name: 'PBKDF2' },
		false,
		['deriveBits'],
	);
	const saltBuf = toArrayBuffer(salt);
	const bits = await crypto.subtle.deriveBits(
		{
			name: 'PBKDF2',
			salt: saltBuf,
			iterations: PBKDF2_ITERATIONS,
			hash: 'SHA-256',
		},
		keyMaterial,
		AES_KEY_LENGTH,
	);
	return crypto.subtle.importKey(
		'raw',
		bits,
		{ name: 'AES-GCM', length: AES_KEY_LENGTH },
		false,
		['encrypt', 'decrypt'],
	);
}

/** Deterministic verification hash: PBKDF2(password, salt) first 256 bits. */
async function hashPasswordForVerify(password: string, salt: Uint8Array): Promise<ArrayBuffer> {
	const enc = new TextEncoder();
	const keyMaterial = await crypto.subtle.importKey(
		'raw',
		enc.encode(password),
		{ name: 'PBKDF2' },
		false,
		['deriveBits'],
	);
	const saltBuf = toArrayBuffer(salt);
	const bits = await crypto.subtle.deriveBits(
		{ name: 'PBKDF2', salt: saltBuf, iterations: PBKDF2_ITERATIONS, hash: 'SHA-256' },
		keyMaterial,
		256,
	);
	return bits;
}

function arrayBufferToBase64(buffer: ArrayBuffer): string {
	const bytes = new Uint8Array(buffer);
	let binary = '';
	for (let i = 0; i < bytes.length; i++) binary += String.fromCharCode(bytes[i]);
	return btoa(binary);
}

function base64ToArrayBuffer(base64: string): ArrayBuffer {
	const binary = atob(base64);
	const bytes = new Uint8Array(binary.length);
	for (let i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);
	return bytes.buffer;
}

async function encryptPrivateKeyWithPassword(
	privateKeyBase64: string,
	password: string,
): Promise<{ saltB64: string; passwordHashB64: string; encryptedB64: string; ivB64: string }> {
	const salt = crypto.getRandomValues(new Uint8Array(SALT_LENGTH));
	const key = await deriveKeyFromPassword(password, salt);
	const iv = crypto.getRandomValues(new Uint8Array(IV_LENGTH));
	const enc = new TextEncoder();
	const ivBuf = toArrayBuffer(iv);
	const cipher = await crypto.subtle.encrypt(
		{ name: 'AES-GCM', iv: ivBuf, tagLength: 128 },
		key,
		enc.encode(privateKeyBase64),
	);
	const verifyHash = await hashPasswordForVerify(password, salt);
	return {
		saltB64: arrayBufferToBase64(toArrayBuffer(salt)),
		passwordHashB64: arrayBufferToBase64(verifyHash),
		encryptedB64: arrayBufferToBase64(cipher),
		ivB64: arrayBufferToBase64(toArrayBuffer(iv)),
	};
}

async function decryptPrivateKeyWithPassword(
	encryptedB64: string,
	ivB64: string,
	saltB64: string,
	password: string,
): Promise<string> {
	const key = await deriveKeyFromPassword(password, new Uint8Array(base64ToArrayBuffer(saltB64)));
	const iv = new Uint8Array(base64ToArrayBuffer(ivB64));
	const ivBuf = toArrayBuffer(iv);
	const cipher = base64ToArrayBuffer(encryptedB64);
	const dec = await crypto.subtle.decrypt(
		{ name: 'AES-GCM', iv: ivBuf, tagLength: 128 },
		key,
		cipher,
	);
	return new TextDecoder().decode(dec);
}

async function verifyPasswordAgainstHash(password: string, saltB64: string, hashB64: string): Promise<boolean> {
	const salt = new Uint8Array(base64ToArrayBuffer(saltB64));
	const expected = new Uint8Array(base64ToArrayBuffer(hashB64));
	const actual = await hashPasswordForVerify(password, salt);
	if (actual.byteLength !== expected.length) return false;
	const actualView = new Uint8Array(actual);
	for (let i = 0; i < expected.length; i++) if (actualView[i] !== expected[i]) return false;
	return true;
}

function parseMasterBlob(raw: string): MasterBlob | null {
	try {
		const o = JSON.parse(raw) as MasterBlob;
		if (o.version === 1 && o.saltB64 && o.passwordHashB64 && o.encryptedPrivateKeyB64 && o.ivB64) return o;
	} catch {
		// ignore
	}
	return null;
}

function parseChatBlob(raw: string): ChatBlob | null {
	try {
		const o = JSON.parse(raw) as ChatBlob;
		if (o.version === 1 && o.saltB64 && o.passwordHashB64 && o.encryptedPrivateKeyB64 && o.ivB64) return o;
	} catch {
		// ignore
	}
	return null;
}

/**
 * Whether the user has set a master password (keys may be locked).
 */
export function hasMasterPassword(userId: string): boolean {
	if (!storage) return false;
	const raw = storage.getItem(getMasterKey(userId));
	return parseMasterBlob(raw ?? '') != null;
}

/**
 * Whether temp chat keys are currently locked (master or per-chat password required).
 */
export function isLocked(userId: string, tempChatId?: string): boolean {
	if (!storage) return false;
	const masterRaw = storage.getItem(getMasterKey(userId));
	const master = parseMasterBlob(masterRaw ?? '');
	if (!master) return false;
	// If master is set, check if we have key in session (unlocked)
	const sessionKey = getSessionKey(userId);
	const hasSession = typeof sessionStorage !== 'undefined' && sessionStorage.getItem(sessionKey);
	if (hasSession) return false;
	// If this chat has its own password, we can unlock with that even without master
	if (tempChatId) {
		const chatRaw = storage.getItem(getChatKey(tempChatId));
		const chat = parseChatBlob(chatRaw ?? '');
		if (chat) return true; // still need to enter password (master or chat)
	}
	return true;
}

/**
 * Unlock with master password. On success, decrypts key and stores in sessionStorage for the session.
 */
export async function unlockWithMasterPassword(
	userId: string,
	password: string,
): Promise<X25519KeyPair | null> {
	if (!storage) return null;
	const raw = storage.getItem(getMasterKey(userId));
	const blob = parseMasterBlob(raw ?? '');
	if (!blob) return null;
	const ok = await verifyPasswordAgainstHash(password, blob.saltB64, blob.passwordHashB64);
	if (!ok) return null;
	const privateKeyBase64 = await decryptPrivateKeyWithPassword(
		blob.encryptedPrivateKeyB64,
		blob.ivB64,
		blob.saltB64,
		password,
	);
	const privateKey = await importPrivateKeyFromBase64(privateKeyBase64);
	const serverKey = await TempChatApi.getMyE2EKey();
	if (!serverKey) return null;
	const publicKey = await importPublicKey(serverKey);
	const keyPair: X25519KeyPair = {
		privateKey,
		publicKey,
		publicKeyBase64: serverKey,
	};
	if (typeof sessionStorage !== 'undefined') {
		sessionStorage.setItem(getSessionKey(userId), privateKeyBase64);
	}
	return keyPair;
}

/**
 * Unlock with per-chat password. On success, decrypts key and stores in sessionStorage for the session.
 */
export async function unlockWithChatPassword(
	userId: string,
	tempChatId: string,
	password: string,
): Promise<X25519KeyPair | null> {
	if (!storage) return null;
	const raw = storage.getItem(getChatKey(tempChatId));
	const blob = parseChatBlob(raw ?? '');
	if (!blob) return null;
	const ok = await verifyPasswordAgainstHash(password, blob.saltB64, blob.passwordHashB64);
	if (!ok) return null;
	const privateKeyBase64 = await decryptPrivateKeyWithPassword(
		blob.encryptedPrivateKeyB64,
		blob.ivB64,
		blob.saltB64,
		password,
	);
	const privateKey = await importPrivateKeyFromBase64(privateKeyBase64);
	const serverKey = await TempChatApi.getMyE2EKey();
	if (!serverKey) return null;
	const publicKey = await importPublicKey(serverKey);
	const keyPair: X25519KeyPair = {
		privateKey,
		publicKey,
		publicKeyBase64: serverKey,
	};
	if (typeof sessionStorage !== 'undefined') {
		sessionStorage.setItem(getSessionKey(userId), privateKeyBase64);
	}
	return keyPair;
}

/**
 * Set master password and encrypt current key (from sessionStorage) with it.
 * If no key exists yet, creates a new key pair and uploads public key.
 */
export async function setMasterPassword(
	userId: string,
	password: string,
	currentPrivateKeyBase64: string | null,
): Promise<boolean> {
	if (!storage) return false;
	let privateKeyBase64 = currentPrivateKeyBase64;
	if (!privateKeyBase64 && typeof sessionStorage !== 'undefined') {
		privateKeyBase64 = sessionStorage.getItem(getSessionKey(userId));
	}
	if (!privateKeyBase64) return false;
	const { saltB64, passwordHashB64, encryptedB64, ivB64 } = await encryptPrivateKeyWithPassword(
		privateKeyBase64,
		password,
	);
	const blob: MasterBlob = {
		version: 1,
		saltB64,
		passwordHashB64,
		encryptedPrivateKeyB64: encryptedB64,
		ivB64,
	};
	storage.setItem(getMasterKey(userId), JSON.stringify(blob));
	// Keep key in session so they stay unlocked
	if (typeof sessionStorage !== 'undefined') {
		sessionStorage.setItem(getSessionKey(userId), privateKeyBase64);
	}
	return true;
}

/**
 * Change master password (requires current password).
 */
export async function changeMasterPassword(
	userId: string,
	currentPassword: string,
	newPassword: string,
): Promise<boolean> {
	const pair = await unlockWithMasterPassword(userId, currentPassword);
	if (!pair) return false;
	const privateKeyBase64 = await exportPrivateKeyBase64(pair.privateKey);
	clearMasterPassword(userId);
	return setMasterPassword(userId, newPassword, privateKeyBase64);
}

/**
 * Remove master password and keep key in sessionStorage (unlocked).
 */
export function clearMasterPassword(userId: string): void {
	if (storage) storage.removeItem(getMasterKey(userId));
	// Do not clear sessionStorage so they remain unlocked
}

/**
 * Lock the current session (remove key from sessionStorage). Next getOrCreateKeyPair will be locked.
 */
export function lockSession(userId: string): void {
	if (typeof sessionStorage !== 'undefined') {
		sessionStorage.removeItem(getSessionKey(userId));
	}
}

/**
 * Set a per-chat password for a temp chat. Encrypts current key with that password.
 */
export async function setChatPassword(
	userId: string,
	tempChatId: string,
	password: string,
	currentPrivateKeyBase64: string | null,
): Promise<boolean> {
	if (!storage) return false;
	let privateKeyBase64 = currentPrivateKeyBase64;
	if (!privateKeyBase64 && typeof sessionStorage !== 'undefined') {
		privateKeyBase64 = sessionStorage.getItem(getSessionKey(userId));
	}
	if (!privateKeyBase64) return false;
	const { saltB64, passwordHashB64, encryptedB64, ivB64 } = await encryptPrivateKeyWithPassword(
		privateKeyBase64,
		password,
	);
	const blob: ChatBlob = {
		version: 1,
		saltB64,
		passwordHashB64,
		encryptedPrivateKeyB64: encryptedB64,
		ivB64,
	};
	storage.setItem(getChatKey(tempChatId), JSON.stringify(blob));
	return true;
}

/**
 * Whether this temp chat has a per-chat password set.
 */
export function hasChatPassword(tempChatId: string): boolean {
	if (!storage) return false;
	const raw = storage.getItem(getChatKey(tempChatId));
	return parseChatBlob(raw ?? '') != null;
}

/**
 * Remove per-chat password for a temp chat.
 */
export function clearChatPassword(tempChatId: string): void {
	if (storage) storage.removeItem(getChatKey(tempChatId));
}
