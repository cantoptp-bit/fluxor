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

import { readFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import type { AppProxyHonoEnv } from '@fluxer/app_proxy/src/AppProxyTypes';
import { proxyAssets } from '@fluxer/app_proxy/src/app_proxy/proxy/AssetsProxy';
import { proxyAPI, stripMarketingPath } from '@fluxer/app_proxy/src/app_proxy/proxy/ApiProxy';
import { createSpaIndexRoute } from '@fluxer/app_proxy/src/app_server/routes/SpaIndexRoute';
import type { CSPOptions } from '@fluxer/app_proxy/src/app_server/utils/CSP';
import { applySpaHeaders } from '@fluxer/app_proxy/src/app_server/utils/StaticFileUtils';
import { isStaticAsset } from '@fluxer/app_proxy/src/app_server/utils/Mime';
import {
	serveSpaFallback,
	serveStaticFile,
} from '@fluxer/app_proxy/src/app_server/utils/StaticFileUtils';
import type { Logger } from '@fluxer/logger/src/Logger';
import type { Hono } from 'hono';
import { streamSSE } from 'hono/streaming';

const clients = new Set<(data: string) => void>();

const __dirname = dirname(fileURLToPath(import.meta.url));

interface RegisterAppProxyRoutesOptions {
	app: Hono<AppProxyHonoEnv>;
	assetsPath: string;
	cspDirectives?: CSPOptions;
	logger: Logger;
	staticCDNEndpoint: string | undefined;
	staticDir?: string;
	apiEndpoint?: string;
	gatewayEndpoint?: string;
	mediaEndpoint?: string;
	appDevEndpoint?: string;
	marketingEndpoint?: string;
}

export function registerAppProxyRoutes(options: RegisterAppProxyRoutesOptions): void {
	const { app, assetsPath, cspDirectives, logger, staticCDNEndpoint, staticDir, apiEndpoint, gatewayEndpoint, mediaEndpoint, appDevEndpoint, marketingEndpoint } = options;

	app.get('/_health', (c) => c.text('OK'));

	// Chrome DevTools requests this endpoint when DevTools are open (Chrome 114+).
	// Return an empty JSON object so Chrome doesn't log a 404 to the console.
	app.get('/.well-known/appspecific/com.chrome.devtools.json', (c) => c.json({}));

	// Simple page to verify proxy is reachable (no app dev server required)
	app.get('/connection-test', (c) =>
		c.html(
			'<!DOCTYPE html><html><head><meta charset="utf-8"><title>Connection test</title></head><body><h1>Proxy is working</h1><p>If you see this, the proxy on port 48763 is reachable.</p><p><a href="/">Open Fluxer app</a></p></body></html>',
		),
	);

	// Register /marketing first so it always wins over the SPA catch-all below.
	if (marketingEndpoint) {
		app.all('/marketing', (c) =>
			proxyAPI(c, {
				apiEndpoint: marketingEndpoint,
				logger,
				pathOverride: stripMarketingPath(c.req.path),
			}),
		);
		app.all('/marketing/*', (c) =>
			proxyAPI(c, {
				apiEndpoint: marketingEndpoint,
				logger,
				pathOverride: stripMarketingPath(c.req.path),
			}),
		);
		// Serve /help and /help/* from marketing so http://localhost:48763/help works.
		app.all('/help', (c) =>
			proxyAPI(c, {
				apiEndpoint: marketingEndpoint,
				logger,
			}),
		);
		app.all('/help/*', (c) =>
			proxyAPI(c, {
				apiEndpoint: marketingEndpoint,
				logger,
			}),
		);
		// Serve policy pages (ToS, Privacy, etc.) at root so /terms and /marketing/terms both work.
		app.all('/terms', (c) =>
			proxyAPI(c, {
				apiEndpoint: marketingEndpoint,
				logger,
			}),
		);
		app.all('/privacy', (c) =>
			proxyAPI(c, {
				apiEndpoint: marketingEndpoint,
				logger,
			}),
		);
		app.all('/guidelines', (c) =>
			proxyAPI(c, {
				apiEndpoint: marketingEndpoint,
				logger,
			}),
		);
		app.all('/security', (c) =>
			proxyAPI(c, {
				apiEndpoint: marketingEndpoint,
				logger,
			}),
		);
		app.all('/company-information', (c) =>
			proxyAPI(c, {
				apiEndpoint: marketingEndpoint,
				logger,
			}),
		);
		// Marketing static assets (CSS/JS) when help is served at /help (assets referenced as /static/...).
		app.all('/static/*', (c) =>
			proxyAPI(c, {
				apiEndpoint: marketingEndpoint,
				logger,
			}),
		);
	}

	app.get('/debug', (c) => {
		const html = readFileSync(resolve(__dirname, '../app_server/pages/DebugTerminal.html'), 'utf-8');
		return c.html(html);
	});

	app.get('/_debug/events', (c) => {
		return streamSSE(c, async (stream) => {
			const send = (data: string) => stream.writeSSE({ data });
			clients.add(send);
			c.req.raw.signal.addEventListener('abort', () => clients.delete(send));

			// Keep-alive
			while (!c.req.raw.signal.aborted) {
				await stream.sleep(30000);
				await stream.writeSSE({ event: 'ping', data: 'pong' });
			}
		});
	});

	app.post('/_debug/log', async (c) => {
		const log = await c.req.json();
		const data = JSON.stringify(log);
		for (const send of clients) {
			send(data);
		}
		return c.json({ ok: true });
	});

	if (apiEndpoint) {
		app.get('/api/_status', (c) =>
			proxyAPI(c, {
				apiEndpoint,
				logger,
				pathOverride: '/_health',
			}),
		);
		app.all('/api/*', (c) =>
			proxyAPI(c, {
				apiEndpoint,
				logger,
			}),
		);
		app.options('/.well-known/fluxer', (_c) => {
			return new Response(null, {
				status: 204,
				headers: {
					'Access-Control-Allow-Origin': '*',
					'Access-Control-Allow-Methods': 'GET, OPTIONS',
					'Access-Control-Allow-Headers': 'Content-Type, Authorization',
				},
			});
		});
		app.get('/.well-known/fluxer', async (c) => {
			const maxAttempts = 15;
			const retryDelayMs = 2000;
			let res: Response | null = null;
			for (let attempt = 1; attempt <= maxAttempts; attempt++) {
				res = await proxyAPI(c, { apiEndpoint, logger });
				if (res.status !== 502 || attempt === maxAttempts) break;
				logger.warn(
					{ attempt, maxAttempts },
					'Bootstrap endpoint returned 502 (API may still be starting), retrying',
				);
				await new Promise((r) => setTimeout(r, retryDelayMs));
			}
			const out = res!;
			const corsHeaders = new Headers(out.headers);
			corsHeaders.set('Access-Control-Allow-Origin', '*');
			return new Response(out.body, {
				status: out.status,
				statusText: out.statusText,
				headers: corsHeaders,
			});
		});
	}

	if (gatewayEndpoint) {
		app.all('/gateway/*', (c) =>
			proxyAPI(c, {
				apiEndpoint: gatewayEndpoint,
				logger,
			}),
		);
	}

	if (mediaEndpoint) {
		app.all('/media/*', (c) =>
			proxyAPI(c, {
				apiEndpoint: mediaEndpoint,
				logger,
			}),
		);
	}

	if (staticCDNEndpoint) {
		app.get(`${assetsPath}/*`, (c) =>
			proxyAssets(c, {
				logger,
				staticCDNEndpoint,
			}),
		);
	}

	if (appDevEndpoint) {
		const projectRoot = process.env['FLUXER_PROJECT_ROOT'];
		const resolvedStaticDir = staticDir
			? (projectRoot ? resolve(projectRoot, staticDir) : resolve(staticDir))
			: undefined;
		app.get('*', async (c) => {
			const response = await proxyAPI(c, {
				apiEndpoint: appDevEndpoint,
				logger,
			});
			// When dev server is unreachable (502) or returns 5xx, fall back to static app if available
			const shouldFallback =
				resolvedStaticDir &&
				(response.status === 502 || (response.status >= 500 && response.status < 600));
			if (shouldFallback) {
				logger.warn(
					{ path: c.req.path, upstreamStatus: response.status },
					'App dev server unreachable or error, falling back to static build',
				);
				const requestPath = c.req.path;
				if (isStaticAsset(requestPath)) {
					const result = serveStaticFile({
						requestPath,
						resolvedStaticDir,
						logger,
					});
					if (result.success) {
						return new Response(result.content, {
							headers: {
								'Content-Type': result.mimeType,
								'Cache-Control': result.cacheControl,
							},
						});
					}
					if (result.error) return c.text(result.error, 500);
					return c.notFound();
				}
				const fallbackResult = serveSpaFallback({
					resolvedStaticDir,
					cspDirectives,
					logger,
				});
				if (!fallbackResult.success) {
					return c.text(fallbackResult.error, 500);
				}
				applySpaHeaders(c, fallbackResult.csp);
				return c.body(fallbackResult.content);
			}
			return response;
		});
	} else if (staticDir) {
		const projectRoot = process.env['FLUXER_PROJECT_ROOT'];
		const resolvedStaticDir = projectRoot ? resolve(projectRoot, staticDir) : resolve(staticDir);
		createSpaIndexRoute(app, { staticDir: resolvedStaticDir, cspDirectives, logger });
	}
}
