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

import { loadConfig } from '@fluxer/config/src/ConfigLoader';
import { extractBaseServiceConfig } from '@fluxer/config/src/ServiceConfigSlices';

const master = await loadConfig();
const appProxy = master.services.app_proxy;

if (!appProxy) {
	throw new Error('Application proxy requires `services.app_proxy` configuration');
}

export const Config = {
	...extractBaseServiceConfig(master),
	port: appProxy.port,
	static_cdn_endpoint: appProxy.static_cdn_endpoint,
	sentry_dsn: master.app_public.sentry_dsn,
	assets_dir: appProxy.assets_dir,
	proxy: master.proxy,
	// The internal endpoints where the proxy should forward requests
	internal_api_endpoint: `http://localhost:${master.services.server?.port ?? 8080}`,
	internal_gateway_endpoint: `http://localhost:${master.services.gateway?.port ?? 8082}`,
	internal_media_endpoint: `http://localhost:${master.services.server?.port ?? 8080}`,
	// When set and non-empty, proxy serves from dev server; otherwise serves from assets_dir (production build).
	app_dev_endpoint: appProxy.app_dev_endpoint?.trim() || undefined,
	// Marketing site (help, terms, etc.) - proxy /marketing/* to this endpoint so Help Center works.
	marketing_endpoint:
		master.services.marketing?.enabled && master.services.marketing?.port != null
			? `http://127.0.0.1:${master.services.marketing.port}`
			: undefined,
};

export type Config = typeof Config;
