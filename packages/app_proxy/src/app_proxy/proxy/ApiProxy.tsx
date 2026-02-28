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

import { createProxyRequestHeaders, forwardProxyRequest } from '@fluxer/app_proxy/src/app_proxy/proxy/ProxyRequest';
import { HttpStatus } from '@fluxer/constants/src/HttpConstants';
import type { Logger } from '@fluxer/logger/src/Logger';
import type { Context } from 'hono';

export interface ProxyApiOptions {
    apiEndpoint: string;
    logger: Logger;
    /** When set, use this path instead of c.req.path for the upstream URL (e.g. to strip /marketing prefix). */
    pathOverride?: string;
}

export async function proxyAPI(c: Context, options: ProxyApiOptions): Promise<Response> {
    const { apiEndpoint, logger, pathOverride } = options;
    const apiBaseUrl = new URL(apiEndpoint);

    const requestPath = pathOverride ?? c.req.path;
    const targetUrl = new URL(requestPath, apiBaseUrl);

    logger.debug(
        { method: c.req.method, path: c.req.path, target: targetUrl.toString() },
        'Proxying request',
    );

    // Append query parameters
    const urlSearch = c.req.raw.url.split('?')[1];
    if (urlSearch) {
        targetUrl.search = urlSearch;
    }

    const headers = createProxyRequestHeaders({
        incomingHeaders: c.req.raw.headers,
        upstreamHost: apiBaseUrl.host,
    });

    // Preserve the Authorization header for API requests
    const originalAuth = c.req.header('Authorization');
    if (originalAuth) {
        headers.set('Authorization', originalAuth);
    }

    // Forward Cookie for /api/* so session-based auth works when proxied
    if (c.req.path.startsWith('/api/')) {
        const cookie = c.req.header('Cookie');
        if (cookie) {
            headers.set('Cookie', cookie);
        }
    }

    if (!headers.has('X-Forwarded-For')) {
        headers.set('X-Forwarded-For', '127.0.0.1');
    }

    try {
        return await forwardProxyRequest({
            targetUrl,
            method: c.req.method,
            headers,
            body: c.req.raw.body,
        });
    } catch (error) {
        logger.error(
            {
                path: c.req.path,
                targetUrl: targetUrl.toString(),
                method: c.req.method,
                error,
            },
            'api proxy error',
        );
        return c.text('Bad Gateway', HttpStatus.BAD_GATEWAY);
    }
}

/** Rewrite path by stripping the /marketing prefix for upstream (marketing server serves at /). */
export function stripMarketingPath(path: string): string {
    const rewritten = path.replace(/^\/marketing\/?/, '/') || '/';
    return rewritten;
}
