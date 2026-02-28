/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is distributed under the GNU Affero General Public License v3.0.
 * In-memory store for dev-provisioned E2E private keys. Only used when nodeEnv === 'development'.
 */

import type {UserID} from '@fluxer/api/src/BrandedTypes';

/** userId -> privateKeyBase64 (PKCS#8). Cleared on process restart. */
const provisionedPrivateKeys = new Map<string, string>();

export function setProvisionedPrivateKey(userId: UserID, privateKeyBase64: string): void {
	provisionedPrivateKeys.set(String(userId), privateKeyBase64);
}

export function getProvisionedPrivateKey(userId: UserID): string | null {
	return provisionedPrivateKeys.get(String(userId)) ?? null;
}
