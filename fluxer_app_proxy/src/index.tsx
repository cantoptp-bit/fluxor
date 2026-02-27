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

import { Config } from '@app/Config';
import { shutdownInstrumentation } from '@app/Instrument';
import { Logger } from '@app/Logger';
import { createAppProxyApp } from '@fluxer/app_proxy/src/App';
import { buildFluxerCSPOptions } from '@fluxer/app_proxy/src/app_server/utils/CSP';
import { createServiceTelemetry } from '@fluxer/hono/src/middleware/TelemetryAdapters';
import { createServerWithUpgrade, setupGracefulShutdown } from '@fluxer/hono/src/Server';
import http from 'node:http';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { extractClientIpFromHeaders } from '@fluxer/ip_utils/src/ClientIp';

const telemetry = createServiceTelemetry({
	serviceName: 'fluxer-app-proxy',
	skipPaths: ['/_health'],
});

/** Resolve assets_dir to an absolute path so static files are found regardless of cwd or FLUXER_PROJECT_ROOT. */
function resolveStaticDir(assetsDir: string): string {
	const projectRoot =
		process.env['FLUXER_PROJECT_ROOT'] ||
		path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..', '..');
	return path.resolve(projectRoot, assetsDir);
}

async function main(): Promise<void> {
	const cspDirectives = buildFluxerCSPOptions({ sentryDsn: Config.sentry_dsn });

	const staticDirResolved = resolveStaticDir(Config.assets_dir);

	const { app, shutdown } = await createAppProxyApp({
		config: Config,
		cspDirectives,
		logger: Logger,
		metricsCollector: telemetry.metricsCollector,
		staticCDNEndpoint: Config.static_cdn_endpoint,
		staticDir: staticDirResolved,
		apiEndpoint: Config.internal_api_endpoint,
		gatewayEndpoint: Config.internal_gateway_endpoint,
		mediaEndpoint: Config.internal_media_endpoint,
		appDevEndpoint: Config.app_dev_endpoint,
		marketingEndpoint: Config.marketing_endpoint,
		tracing: telemetry.tracing,
	});

	const port = Config.port;
	Logger.info({ port }, 'Starting Fluxer App Proxy');

	const gatewayUrl = new URL(Config.internal_gateway_endpoint);
	const gatewayHost = gatewayUrl.hostname;
	const gatewayPort = Number(gatewayUrl.port) || (gatewayUrl.protocol === 'https:' ? 443 : 80);

	const onUpgrade = (req: http.IncomingMessage, socket: any, head: Buffer) => {
		if (req.url?.startsWith('/gateway')) {
			Logger.info({ url: req.url }, 'Proxying gateway WebSocket upgrade');

			const stripped = req.url.replace(/^\/gateway/, '');
			const forwardPath = stripped === '' || stripped.startsWith('?') ? `/${stripped}` : stripped;

			const clientIp = extractClientIpFromHeaders(req.headers, {
				trustCfConnectingIp: Config.proxy?.trust_cf_connecting_ip,
			});

			const options = {
				hostname: gatewayHost,
				port: gatewayPort,
				path: forwardPath,
				method: req.method,
				headers: {
					...req.headers,
					...(clientIp ? { 'x-forwarded-for': clientIp } : {}),
				},
			};

			const proxyReq = http.request(options);
			proxyReq.on('upgrade', (res, proxySocket, proxyHead) => {
				socket.write(`HTTP/${res.httpVersion} ${res.statusCode} ${res.statusMessage}\r\n`);
				Object.entries(res.headers).forEach(([key, value]) => {
					socket.write(`${key}: ${value}\r\n`);
				});
				socket.write('\r\n');
				socket.write(proxyHead);
				proxySocket.write(head);
				proxySocket.pipe(socket);
				socket.pipe(proxySocket);
			});

			proxyReq.on('error', (err) => {
				Logger.error({ err }, 'Gateway WebSocket proxy error');
				socket.destroy();
			});

			proxyReq.end();
		} else {
			socket.destroy();
		}
	};

	const server = createServerWithUpgrade(app, {
		port,
		onUpgrade
	});

	setupGracefulShutdown(
		async () => {
			await shutdown();
			await shutdownInstrumentation();
			await new Promise<void>((resolve) => {
				server.close(() => resolve());
			});
		},
		{ logger: Logger },
	);
}

main().catch((err) => {
	Logger.fatal({ error: err }, 'fatal error');
	process.exit(1);
});
