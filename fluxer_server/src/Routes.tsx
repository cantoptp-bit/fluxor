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

import type { Config } from '@app/Config';
import { createHealthCheckHandler, createLivenessCheckHandler, createReadinessCheckHandler } from '@app/HealthCheck';
import { createComponentLogger } from '@app/Logger';
import {
	type InitializedServices,
	initializeAllServices,
	runServiceInitialization,
	type ServiceInitializer,
	shutdownAllServices,
	startBackgroundServices,
} from '@app/ServiceInitializer';
import { getBuildMetadata } from '@fluxer/config/src/BuildMetadata';
import { AppErrorHandler, AppNotFoundHandler } from '@fluxer/errors/src/domains/core/ErrorHandlers';
import { applyMiddlewareStack } from '@fluxer/hono/src/middleware/MiddlewareStack';
import { createServiceTelemetry } from '@fluxer/hono/src/middleware/TelemetryAdapters';
import type { BaseHonoEnv } from '@fluxer/hono_types/src/HonoTypes';
import { Hono } from 'hono';
import { trimTrailingSlash } from 'hono/trailing-slash';

export interface MountedRoutes {
	app: Hono<BaseHonoEnv>;
	services: InitializedServices;
	initialize: () => Promise<void>;
	start: () => Promise<void>;
	shutdown: () => Promise<void>;
}

export interface MountRoutesOptions {
	config: Config;
	staticDir?: string | undefined;
}

const startTime = Date.now();
const BUILD_METADATA = getBuildMetadata();

export async function mountRoutes(options: MountRoutesOptions): Promise<MountedRoutes> {
	const { config, staticDir } = options;
	const logger = createComponentLogger('routes');
	const VERSION = BUILD_METADATA.buildNumber ?? '0.0.0';

	logger.info('Starting route mounting and service initialization');

	const app = new Hono<BaseHonoEnv>();

	app.get('/.well-known/fluxer', (ctx) => {
		const forwardedHost = ctx.req.header('x-forwarded-host');
		const forwardedProto = ctx.req.header('x-forwarded-proto');
		const requestHost = ctx.req.header('host');
		const useForwarded = Boolean(forwardedHost);
		const effectiveHost = forwardedHost ?? requestHost;
		const instanceDomain = effectiveHost
			? (effectiveHost.split(':')[0] ?? config.domain.base_domain)
			: (config.domain.base_domain || 'localhost');
		const scheme = useForwarded
			? (forwardedProto === 'https' ? 'https' : 'http')
			: (config.domain.public_scheme ?? (config.dev?.test_mode_enabled ? 'http' : 'https'));
		const portFromHost = effectiveHost?.includes(':') ? effectiveHost.split(':')[1] : null;
		const cfgPort = config.domain.public_port ?? config.port;
		const isStandard = (s: string, p: string) =>
			(s === 'https' && (p === '443' || p === '')) || (s === 'http' && (p === '80' || p === ''));
		// When behind a reverse proxy / tunnel, trust the Host header's port (or lack thereof) instead of the config port
		const portVal = useForwarded
			? (portFromHost ?? '')
			: (portFromHost ?? (cfgPort === 80 || cfgPort === 443 ? '' : String(cfgPort)));
		const port = isStandard(scheme, portVal) ? '' : (portVal ? `:${portVal}` : '');
		const baseUrl = `${scheme}://${instanceDomain}${port}`;
		const gatewayScheme = scheme === 'https' ? 'wss' : 'ws';

		return ctx.json({
			api_code_version: 1,
			endpoints: {
				api: `${baseUrl}/api`,
				api_client: `${baseUrl}/api`,
				api_public: `${baseUrl}/api`,
				gateway: `${gatewayScheme}://${instanceDomain}${port}/gateway`,
				media: `${baseUrl}/media`,
				static_cdn: 'https://fluxerstatic.com',
				marketing: baseUrl,
				admin: `${baseUrl}/admin`,
				invite: `${baseUrl}/invite`,
				gift: `${baseUrl}/gift`,
				webapp: baseUrl,
			},
			captcha: {
				provider: 'none',
				hcaptcha_site_key: null,
				turnstile_site_key: null,
			},
			features: {
				sms_mfa_enabled: false,
				voice_enabled: false,
				stripe_enabled: false,
				self_hosted: true,
				manual_review_enabled: false,
			},
			test_mode_enabled: config.dev?.test_mode_enabled ?? false,
			gif: {
				provider: 'tenor',
			},
			sso: {
				enabled: false,
				enforced: false,
				display_name: null,
				redirect_uri: '',
			},
			limits: {
				version: 2,
				traitDefinitions: [],
				rules: [],
				defaultsHash: 'default',
			},
			push: {
				public_vapid_key: null,
			},
			app_public: {
				sentry_dsn: '',
			},
		});
	});

	app.use(trimTrailingSlash());

	const telemetry = createServiceTelemetry({
		serviceName: 'fluxer-server',
		skipPaths: ['/_health', '/_ready', '/_live'],
	});

	applyMiddlewareStack(app, {
		requestId: {},
		tracing: telemetry.tracing,
		metrics: {
			enabled: true,
			collector: telemetry.metricsCollector,
			skipPaths: ['/_health', '/_ready', '/_live'],
		},
		logger: {
			log: (data) => {
				logger.info(
					{
						method: data.method,
						path: data.path,
						status: data.status,
						durationMs: data.durationMs,
					},
					'Request completed',
				);
			},
			skip: ['/_health', '/_ready', '/_live'],
		},
		skipErrorHandler: true,
	});
	let initializers: Array<ServiceInitializer> = [];
	let services: InitializedServices = {};

	try {
		const result = await initializeAllServices({
			config,
			logger,
			staticDir,
		});

		initializers = result.initializers;
		services = result.services;

		if (services.s3 !== undefined) {
			app.route('/s3', services.s3.app);
			logger.info(config.isMonolith ? 'S3 service mounted at /s3 (restricted mode)' : 'S3 service mounted at /s3');
		}

		if (services.mediaProxy !== undefined) {
			app.route('/media', services.mediaProxy.app);
			logger.info(
				config.isMonolith
					? 'Media Proxy service mounted at /media (public-only mode)'
					: 'Media Proxy service mounted at /media',
			);
		}

		if (services.admin !== undefined) {
			app.route('/admin', services.admin.app);
			logger.info('Admin service mounted at /admin');
		}

		if (services.api !== undefined) {
			const apiService = services.api;
			app.route('/api', apiService.app);
			app.get('/.well-known/fluxer', (ctx) => {
				const instanceDomain = config.domain.base_domain || 'localhost';
				const scheme =
					config.domain.public_scheme ?? (config.dev?.test_mode_enabled ? 'http' : 'https');
				const publicPort = config.domain.public_port ?? config.port;
				const port = publicPort === 80 || publicPort === 443 ? '' : `:${publicPort}`;
				const baseUrl = `${scheme}://${instanceDomain}${port}`;
				const gatewayScheme = scheme === 'https' ? 'wss' : 'ws';

				return ctx.json({
					api_code_version: 1,
					endpoints: {
						api: `${baseUrl}/api`,
						api_client: `${baseUrl}/api`,
						api_public: `${baseUrl}/api`,
						gateway: `${gatewayScheme}://${instanceDomain}${port}/gateway`,
						media: `${baseUrl}/media`,
						static_cdn: 'https://fluxerstatic.com',
						marketing: baseUrl,
						admin: `${baseUrl}/admin`,
						invite: `${baseUrl}/invite`,
						gift: `${baseUrl}/gift`,
						webapp: baseUrl,
					},
					captcha: {
						provider: 'none',
						hcaptcha_site_key: null,
						turnstile_site_key: null,
					},
					features: {
						sms_mfa_enabled: false,
						voice_enabled: false,
						stripe_enabled: false,
						self_hosted: true,
						manual_review_enabled: false,
					},
					gif: {
						provider: 'tenor',
					},
					sso: {
						enabled: false,
						enforced: false,
						display_name: null,
						redirect_uri: '',
					},
					limits: {
						version: 2,
						traitDefinitions: [],
						rules: [],
						defaultsHash: 'default',
					},
					push: {
						public_vapid_key: null,
					},
					app_public: {
						sentry_dsn: '',
					},
				});
			});
			logger.info('API service mounted at /api');
		}

		const healthHandler = createHealthCheckHandler({
			services,
			staticDir,
			version: VERSION,
			startTime,
			latencyThresholdMs: config.healthCheck.latencyThresholdMs,
		});
		app.get('/_health', healthHandler);
		logger.info('Health check endpoint mounted at /_health');

		const readinessHandler = createReadinessCheckHandler({
			services,
			staticDir,
			version: VERSION,
			startTime,
			latencyThresholdMs: config.healthCheck.latencyThresholdMs,
		});
		app.get('/_ready', readinessHandler);
		logger.info('Readiness check endpoint mounted at /_ready');

		const livenessHandler = createLivenessCheckHandler();
		app.get('/_live', livenessHandler);
		logger.info('Liveness check endpoint mounted at /_live');

		if (config.env === 'development') {
			const devSeedInit = initializers.find((i) => i.name === 'Dev Seed');
			const devSeed = devSeedInit?.service as { ensureAdminFriends?: () => Promise<{ ok: boolean; message: string }> } | undefined;
			if (devSeed?.ensureAdminFriends) {
				app.post('/_dev/seed-admin-friends', async (ctx) => {
					try {
						const result = await devSeed.ensureAdminFriends();
						return ctx.json(result);
					} catch (e) {
						return ctx.json({ ok: false, message: (e as Error).message }, 500);
					}
				});
				logger.info('Dev endpoint mounted at POST /_dev/seed-admin-friends');
			}
		}

		if (services.appServer !== undefined) {
			app.route('/', services.appServer.app);
			logger.info('SPA App server mounted at /');
		}

		app.onError(AppErrorHandler);
		app.notFound(AppNotFoundHandler);

		logger.info({ serviceCount: initializers.length }, 'All services mounted successfully');
	} catch (error) {
		logger.error({ error: error instanceof Error ? error.message : 'Unknown error' }, 'Failed to mount routes');
		throw error;
	}

	const initialize = async (): Promise<void> => {
		await runServiceInitialization(initializers, logger);
	};

	const start = async (): Promise<void> => {
		await startBackgroundServices(initializers, logger);
	};

	const shutdown = async (): Promise<void> => {
		await shutdownAllServices(initializers, logger);
	};

	return {
		app,
		services,
		initialize,
		start,
		shutdown,
	};
}
