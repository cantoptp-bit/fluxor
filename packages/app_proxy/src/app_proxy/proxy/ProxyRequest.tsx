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

const BLOCKED_PROXY_REQUEST_HEADERS = [
	'authorization',
	'connection',
	'cookie',
	'host',
	'keep-alive',
	'proxy-authenticate',
	'proxy-authorization',
	'te',
	'trailer',
	'trailers',
	'transfer-encoding',
	'upgrade',
] as const;

const BLOCKED_PROXY_RESPONSE_HEADERS = [
	'connection',
	'keep-alive',
	'proxy-authenticate',
	'proxy-authorization',
	'te',
	'trailer',
	'trailers',
	'transfer-encoding',
	'upgrade',
] as const;

interface NodeFetchRequestInit extends RequestInit {
	duplex?: 'half';
}

export interface CreateProxyRequestHeadersOptions {
	incomingHeaders: Headers;
	upstreamHost: string;
	forwardedHost?: string;
	forwardedProto?: string;
}

const UPSTREAM_REQUEST_TIMEOUT_MS = 90_000;

export interface ForwardProxyRequestOptions {
	targetUrl: URL;
	method: string;
	headers: Headers;
	body?: Request['body'];
	bufferResponseBody?: boolean;
}

export function createProxyRequestHeaders(options: CreateProxyRequestHeadersOptions): Headers {
	const headers = new Headers(options.incomingHeaders);
	const forwardedHost =
		options.forwardedHost ??
		options.incomingHeaders.get('x-forwarded-host') ??
		options.incomingHeaders.get('host');
	const forwardedProto =
		options.forwardedProto ??
		options.incomingHeaders.get('x-forwarded-proto');

	for (const headerName of BLOCKED_PROXY_REQUEST_HEADERS) {
		headers.delete(headerName);
	}

	headers.set('host', options.upstreamHost);
	if (forwardedHost) headers.set('x-forwarded-host', forwardedHost);
	if (forwardedProto) headers.set('x-forwarded-proto', forwardedProto);
	return headers;
}

export async function forwardProxyRequest(options: ForwardProxyRequestOptions): Promise<Response> {
	const {targetUrl, method, headers, body, bufferResponseBody = false} = options;
	const controller = new AbortController();
	const timeoutId = setTimeout(() => controller.abort(), UPSTREAM_REQUEST_TIMEOUT_MS);
	const requestInit: NodeFetchRequestInit = {method, headers, signal: controller.signal};

	if (method !== 'GET' && method !== 'HEAD' && body !== null && body !== undefined) {
		requestInit.body = body;
		requestInit.duplex = 'half';
	}

	let upstreamResponse: Response;
	try {
		upstreamResponse = await fetch(targetUrl.toString(), requestInit);
	} catch (err) {
		clearTimeout(timeoutId);
		if ((err as Error).name === 'AbortError') {
			return new Response('Upstream timeout', { status: 504, statusText: 'Gateway Timeout' });
		}
		throw err;
	}
	clearTimeout(timeoutId);

	const responseHeaders = new Headers(upstreamResponse.headers);
	for (const headerName of BLOCKED_PROXY_RESPONSE_HEADERS) {
		responseHeaders.delete(headerName);
	}

	responseHeaders.delete('content-length');
	responseHeaders.delete('content-encoding');

	if (bufferResponseBody) {
		const bodyBuffer = new Uint8Array(await upstreamResponse.arrayBuffer());
		return new Response(bodyBuffer, {
			status: upstreamResponse.status,
			headers: responseHeaders,
		});
	}

	return new Response(upstreamResponse.body, {
		status: upstreamResponse.status,
		headers: responseHeaders,
	});
}
