/**
 * Parses backend config JSON (from /backend-config.json or /api/fluxer-config)
 * and returns the API URL for bootstrap. Used by RuntimeConfigStore.
 */
export interface BackendConfigData {
	base_domain?: string;
	api?: string;
	gateway?: string;
	/** Backend /.well-known/fluxer shape */
	endpoints?: { api?: string; api_client?: string; gateway?: string };
}

/**
 * Returns the API URL from backend config, or null if not present.
 * Accepts both Vercel fluxer-config shape (api, base_domain) and backend .well-known/fluxer (endpoints.api).
 */
export function parseBackendConfigApiUrl(data: BackendConfigData | null | undefined): string | null {
	if (!data) return null;
	const api =
		data.api ??
		data.endpoints?.api ??
		data.endpoints?.api_client ??
		(data.base_domain ? `https://${data.base_domain}/api` : null);
	return api?.trim() ? api : null;
}

/**
 * Returns true when the given URL is a localhost origin (for ignoring persisted localhost on Vercel).
 */
export function isLocalhostOrigin(urlString: string): boolean {
	try {
		const url = new URL(urlString.startsWith('/') ? 'https://x' + urlString : urlString);
		return url.hostname === 'localhost' || url.hostname === '127.0.0.1';
	} catch {
		return false;
	}
}
