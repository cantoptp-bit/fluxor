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

import Config from '@app/Config';
import type {HttpRequestConfig} from '@app/lib/HttpClient';
import HttpClient from '@app/lib/HttpClient';
import {makePersistent} from '@app/lib/MobXPersistence';
import relayClient from '@app/lib/RelayClient';
import DeveloperOptionsStore from '@app/stores/DeveloperOptionsStore';
import {parseBackendConfigApiUrl} from '@app/utils/backendConfigUtils';
import {API_CODE_VERSION} from '@fluxer/constants/src/AppConstants';
import {expandWireFormat} from '@fluxer/limits/src/LimitDiffer';
import type {LimitConfigSnapshot, LimitConfigWireFormat} from '@fluxer/limits/src/LimitTypes';
import {makeAutoObservable, reaction, runInAction} from 'mobx';

export interface InstanceFeatures {
	sms_mfa_enabled: boolean;
	voice_enabled: boolean;
	stripe_enabled: boolean;
	self_hosted: boolean;
	manual_review_enabled: boolean;
}

export interface InstanceSsoConfig {
	enabled: boolean;
	enforced: boolean;
	display_name: string | null;
	redirect_uri: string;
}

export interface InstanceEndpoints {
	api: string;
	api_client?: string;
	api_public?: string;
	gateway: string;
	media: string;
	static_cdn: string;
	marketing: string;
	admin: string;
	invite: string;
	gift: string;
	webapp: string;
}

export interface InstanceCaptcha {
	provider: 'hcaptcha' | 'turnstile' | 'none';
	hcaptcha_site_key: string | null;
	turnstile_site_key: string | null;
}

export interface InstancePush {
	public_vapid_key: string | null;
}

export interface InstanceAppPublic {
	sentry_dsn: string;
}

export type GifProvider = 'klipy' | 'tenor';

export interface InstanceDiscoveryResponse {
	api_code_version: number;
	endpoints: InstanceEndpoints;
	captcha: InstanceCaptcha;
	features: InstanceFeatures;
	gif?: {provider: GifProvider};
	sso?: InstanceSsoConfig;
	limits: LimitConfigSnapshot | LimitConfigWireFormat;
	push?: InstancePush;
	app_public: InstanceAppPublic;
	test_mode_enabled?: boolean;
}

export interface RuntimeConfigSnapshot {
	apiEndpoint: string;
	apiPublicEndpoint: string;
	gatewayEndpoint: string;
	mediaEndpoint: string;
	staticCdnEndpoint: string;
	marketingEndpoint: string;
	adminEndpoint: string;
	inviteEndpoint: string;
	giftEndpoint: string;
	webAppEndpoint: string;
	gifProvider: GifProvider;
	captchaProvider: 'hcaptcha' | 'turnstile' | 'none';
	hcaptchaSiteKey: string | null;
	turnstileSiteKey: string | null;
	apiCodeVersion: number;
	features: InstanceFeatures;
	sso: InstanceSsoConfig | null;
	publicPushVapidKey: string | null;
	limits: LimitConfigSnapshot;
	sentryDsn: string;
	relayDirectoryUrl: string | null;
	testModeEnabled: boolean;
}

type InitState = 'initializing' | 'ready' | 'error';

class RuntimeConfigStore {
	private _initState: InitState = 'initializing';
	private _initError: Error | null = null;

	private _initPromise: Promise<void>;
	private _resolveInit!: () => void;
	private _rejectInit!: (err: Error) => void;

	private _connectSeq = 0;

	apiEndpoint: string = '';
	apiPublicEndpoint: string = '';
	gatewayEndpoint: string = '';
	mediaEndpoint: string = '';
	staticCdnEndpoint: string = '';
	marketingEndpoint: string = '';
	adminEndpoint: string = '';
	inviteEndpoint: string = '';
	giftEndpoint: string = '';
	webAppEndpoint: string = '';

	gifProvider: GifProvider = 'klipy';

	captchaProvider: 'hcaptcha' | 'turnstile' | 'none' = 'none';
	hcaptchaSiteKey: string | null = null;
	turnstileSiteKey: string | null = null;

	apiCodeVersion: number = API_CODE_VERSION;
	features: InstanceFeatures = {
		sms_mfa_enabled: false,
		voice_enabled: false,
		stripe_enabled: false,
		self_hosted: false,
		manual_review_enabled: false,
	};
	sso: InstanceSsoConfig | null = null;
	publicPushVapidKey: string | null = null;
	limits: LimitConfigSnapshot = this.createEmptyLimitConfig();
	currentDefaultsHash: string | null = null;

	sentryDsn: string = '';

	relayDirectoryUrl: string | null = Config.PUBLIC_RELAY_DIRECTORY_URL;

	testModeEnabled: boolean = false;

	/** True when we loaded from minimal config (Vercel fallback) because discovery failed; login page may show a "backend unreachable" hint. */
	usedMinimalConfig: boolean = false;

	get relayModeEnabled(): boolean {
		return this.relayDirectoryUrl != null;
	}

	constructor() {
		this._initPromise = new Promise<void>((resolve, reject) => {
			this._resolveInit = resolve;
			this._rejectInit = reject;
		});

		makeAutoObservable(this, {}, {autoBind: true});

		this.initialize().catch(() => {});

		reaction(
			() => this.apiEndpoint,
			(endpoint) => {
				if (endpoint) {
					HttpClient.setBaseUrl(endpoint, this.apiCodeVersion);
					this.updateTargetInstanceDomain(endpoint);
				}
			},
			{fireImmediately: true},
		);

		reaction(
			() => this.relayDirectoryUrl,
			(directoryUrl) => {
				HttpClient.setRelayDirectoryUrl(directoryUrl);
				if (directoryUrl) {
					relayClient.setRelayDirectoryUrl(directoryUrl);
				}
			},
			{fireImmediately: true},
		);
	}

	private updateTargetInstanceDomain(endpoint: string): void {
		try {
			const url = new URL(endpoint);
			HttpClient.setTargetInstanceDomain(url.hostname);
		} catch {
			HttpClient.setTargetInstanceDomain(null);
		}
	}

	/**
	 * Resolve the bootstrap API URL. When the app is on Vercel we must not use persisted
	 * localhost; use build config or same-origin /api instead.
	 */
	private getEffectiveBootstrapEndpoint(): string {
		let configured =
			this.apiEndpoint || Config.PUBLIC_BOOTSTRAP_API_ENDPOINT || '/api';
		if (typeof window !== 'undefined' && configured) {
			try {
				const url = new URL(configured.startsWith('/') ? window.location.origin + configured : configured);
				// Ignore persisted localhost when we're on a different origin (e.g. Vercel)
				const isLocalhost =
					url.hostname === 'localhost' || url.hostname === '127.0.0.1';
				if (
					isLocalhost &&
					window.location.origin !== url.origin
				) {
					configured = Config.PUBLIC_BOOTSTRAP_API_ENDPOINT || '/api';
				}
			} catch {
				// keep configured
			}
		}
		if (typeof window === 'undefined') return configured;
		if (configured.startsWith('/')) return configured;
		try {
			new URL(configured);
			return configured;
		} catch {
			// not a valid URL
		}
		return configured;
	}

	/**
	 * Resolve bootstrap API URL: static config, injected domain, serverless config, or build config.
	 */
	private async resolveBootstrapEndpoint(): Promise<string> {
		if (typeof window === 'undefined') {
			return this.getEffectiveBootstrapEndpoint();
		}
		const origin = window.location.origin;
		const isVercel = origin.includes('vercel.app');
		// On Vercel, try runtime config first so updating FLUXER_PUBLIC_DOMAIN + refresh works without redeploy
		if (isVercel) {
			try {
				const res = await fetch(`${origin}/api/fluxer-config`, {
					method: 'GET',
					headers: { Accept: 'application/json' },
					cache: 'no-store',
				});
				if (res.ok) {
					const data = (await res.json()) as { base_domain?: string; api?: string };
					const api = parseBackendConfigApiUrl(data);
					if (api) return api;
				}
			} catch {
				// ignore
			}
		}
		// 1) Static file written at build (backend-config.json)
		try {
			const res = await fetch(`${origin}/backend-config.json`, {
				method: 'GET',
				headers: { Accept: 'application/json' },
				cache: 'no-store',
			});
			let api: string | null = null;
			if (res.ok) {
				try {
					const data = (await res.json()) as { base_domain?: string; api?: string };
					api = parseBackendConfigApiUrl(data);
				} catch {
					api = null;
				}
			}
			if (res.ok && api) return api;
		} catch {
			// ignore
		}
		// 2) Build-time injection (scripts/inject-fluxer-domain.mjs)
		const injected = (window as unknown as { __FLUXER_PUBLIC_DOMAIN__?: string }).__FLUXER_PUBLIC_DOMAIN__;
		if (injected?.trim()) {
			const d = injected.trim().replace(/^https?:\/\//, '').split('/')[0];
			if (d) return `https://${d}/api`;
		}
		// 3) Runtime config (non-Vercel or Vercel fallback)
		if (!isVercel) {
			try {
				const res = await fetch(`${origin}/api/fluxer-config`, {
					method: 'GET',
					headers: { Accept: 'application/json' },
					cache: 'no-store',
				});
				if (res.ok) {
					const data = (await res.json()) as { base_domain?: string; api?: string };
					const api = parseBackendConfigApiUrl(data);
					if (api) return api;
				}
			} catch {
				// ignore
			}
		}
		// On Vercel, retry fluxer-config once (transient failure or cold start)
		if (isVercel) {
			try {
				const retryRes = await fetch(`${origin}/api/fluxer-config`, {
					method: 'GET',
					headers: { Accept: 'application/json' },
					cache: 'reload',
				});
				if (retryRes.ok) {
					const data = (await retryRes.json()) as { base_domain?: string; api?: string };
					const api = parseBackendConfigApiUrl(data);
					if (api) return api;
				}
			} catch {
				// ignore
			}
		}
		const fallback = this.getEffectiveBootstrapEndpoint();
		// On Vercel, never use localhost or same-origin (backend is elsewhere; same-origin would hit Vercel, not your API)
		if (isVercel && fallback) {
			try {
				const fallbackUrl = fallback.startsWith('/') ? origin + fallback : fallback;
				const u = new URL(fallbackUrl);
				if (u.hostname === 'localhost' || u.hostname === '127.0.0.1') {
					throw new Error(
						'FLUXER_PUBLIC_DOMAIN is not set. Set it in Vercel → Settings → Environment Variables (e.g. your ngrok hostname), then run .\\scripts\\set_vercel_backend_from_ngrok.ps1 and refresh.',
					);
				}
				if (u.origin === origin) {
					throw new Error(
						'Backend URL not configured for Vercel. Set FLUXER_PUBLIC_DOMAIN in Vercel → Settings → Environment Variables to your backend hostname (e.g. from ngrok). Run .\\scripts\\set_vercel_backend_from_ngrok.ps1 with ngrok running, then refresh.',
					);
				}
			} catch (e) {
				if (e instanceof Error && (e.message.includes('FLUXER_PUBLIC_DOMAIN') || e.message.includes('Backend URL'))) throw e;
			}
		}
		return fallback;
	}

	/** More retries after restart so backend has time to bind (proxy is up first, API may take 15–30s). */
	private static readonly BOOTSTRAP_RETRY_ATTEMPTS = 18;
	private static readonly BOOTSTRAP_RETRY_DELAY_MS = 2000;

	private async initialize(): Promise<void> {
		try {
			await makePersistent(this, 'runtimeConfig', [
				'apiEndpoint',
				'apiPublicEndpoint',
				'gatewayEndpoint',
				'mediaEndpoint',
				'staticCdnEndpoint',
				'marketingEndpoint',
				'adminEndpoint',
				'inviteEndpoint',
				'giftEndpoint',
				'webAppEndpoint',
				'gifProvider',
				'captchaProvider',
				'hcaptchaSiteKey',
				'turnstileSiteKey',
				'apiCodeVersion',
				'features',
				'sso',
				'publicPushVapidKey',
				'limits',
				'currentDefaultsHash',
				'sentryDsn',
				'relayDirectoryUrl',
			]);

			const bootstrapEndpoint = await this.resolveBootstrapEndpoint();
			const isVercel =
				typeof window !== 'undefined' && window.location.origin.includes('vercel.app');

			try {
				await this.connectToEndpointWithRetry(bootstrapEndpoint);
			} catch (connectError) {
				// On Vercel, if discovery fails (CORS, backend down, etc.) still show the login page
				// with a minimal config so the user sees the form instead of "Connection Issue".
				if (isVercel) {
					runInAction(() => {
						const minimal = this.buildMinimalInstanceFromBootstrapUrl(bootstrapEndpoint);
						this.updateFromInstance(minimal);
						this.usedMinimalConfig = true;
						this._initState = 'ready';
						this._initError = null;
					});
					this._resolveInit();
					return;
				}
				throw connectError;
			}

			runInAction(() => {
				this._initState = 'ready';
				this._initError = null;
			});

			this._resolveInit();
		} catch (error) {
			const err = error instanceof Error ? error : new Error(String(error));
			runInAction(() => {
				this._initState = 'error';
				this._initError = err;
			});
			this._rejectInit(err);
		}
	}

	/** Build minimal discovery payload from bootstrap API URL so the app can load when /.well-known/fluxer is unreachable (e.g. CORS or backend down). */
	private buildMinimalInstanceFromBootstrapUrl(apiEndpoint: string): InstanceDiscoveryResponse {
		const normalized = this.normalizeEndpoint(apiEndpoint);
		let base: string;
		try {
			const u = new URL(normalized);
			u.pathname = '';
			u.search = '';
			u.hash = '';
			base = u.toString().replace(/\/$/, '');
		} catch {
			base = normalized.replace(/\/api\/?$/, '');
		}
		const gatewayBase = base.replace(/^http:/, 'ws:').replace(/^https:/, 'wss:');
		return {
			api_code_version: API_CODE_VERSION,
			endpoints: {
				api: normalized,
				api_client: normalized,
				api_public: normalized,
				gateway: `${gatewayBase}/gateway`,
				media: `${base}/media`,
				static_cdn: `${base}/cdn`,
				marketing: `${base}/marketing`,
				admin: `${base}/admin`,
				invite: `${base}/invite`,
				gift: `${base}/gift`,
				webapp: base,
			},
			captcha: { provider: 'none', hcaptcha_site_key: null, turnstile_site_key: null },
			features: {
				sms_mfa_enabled: false,
				voice_enabled: false,
				stripe_enabled: false,
				self_hosted: false,
				manual_review_enabled: false,
			},
			limits: { version: 1, traitDefinitions: [], rules: [] },
			app_public: { sentry_dsn: '' },
			test_mode_enabled: false,
		};
	}

	/**
	 * Connect to the bootstrap endpoint with retries and backoff.
	 * After a server restart the backend may not be ready immediately; retrying
	 * avoids showing "Connection Issue" when the user loads the page too soon.
	 */
	private async connectToEndpointWithRetry(bootstrapEndpoint: string): Promise<void> {
		let lastError: Error | null = null;
		for (let attempt = 0; attempt < RuntimeConfigStore.BOOTSTRAP_RETRY_ATTEMPTS; attempt++) {
			try {
				await this.connectToEndpoint(bootstrapEndpoint);
				return;
			} catch (error) {
				lastError = error instanceof Error ? error : new Error(String(error));
				if (attempt < RuntimeConfigStore.BOOTSTRAP_RETRY_ATTEMPTS - 1) {
					await new Promise((resolve) =>
						setTimeout(resolve, RuntimeConfigStore.BOOTSTRAP_RETRY_DELAY_MS),
					);
				}
			}
		}
		throw lastError ?? new Error('Failed to connect to API');
	}

	waitForInit(): Promise<void> {
		return this._initPromise;
	}

	get initialized(): boolean {
		return this._initState === 'ready';
	}

	get initError(): Error | null {
		return this._initError;
	}

	applySnapshot(snapshot: RuntimeConfigSnapshot): void {
		this.apiEndpoint = snapshot.apiEndpoint;
		this.apiPublicEndpoint = snapshot.apiPublicEndpoint;
		this.gatewayEndpoint = snapshot.gatewayEndpoint;
		this.mediaEndpoint = snapshot.mediaEndpoint;
		this.staticCdnEndpoint = snapshot.staticCdnEndpoint;
		this.marketingEndpoint = snapshot.marketingEndpoint;
		this.adminEndpoint = snapshot.adminEndpoint;
		this.inviteEndpoint = snapshot.inviteEndpoint;
		this.giftEndpoint = snapshot.giftEndpoint;
		this.webAppEndpoint = snapshot.webAppEndpoint;

		this.gifProvider = snapshot.gifProvider;

		this.captchaProvider = snapshot.captchaProvider;
		this.hcaptchaSiteKey = snapshot.hcaptchaSiteKey;
		this.turnstileSiteKey = snapshot.turnstileSiteKey;

		this.apiCodeVersion = snapshot.apiCodeVersion;
		this.features = snapshot.features;
		this.sso = snapshot.sso;
		this.publicPushVapidKey = snapshot.publicPushVapidKey;
		this.limits = this.normalizeLimits(snapshot.limits ?? this.createEmptyLimitConfig());
		this.currentDefaultsHash = null;

		this.sentryDsn = snapshot.sentryDsn;

		this.relayDirectoryUrl = snapshot.relayDirectoryUrl;

		this.testModeEnabled = snapshot.testModeEnabled;
	}
	getSnapshot(): RuntimeConfigSnapshot {
		return {
			apiEndpoint: this.apiEndpoint,
			apiPublicEndpoint: this.apiPublicEndpoint,
			gatewayEndpoint: this.gatewayEndpoint,
			mediaEndpoint: this.mediaEndpoint,
			staticCdnEndpoint: this.staticCdnEndpoint,
			marketingEndpoint: this.marketingEndpoint,
			adminEndpoint: this.adminEndpoint,
			inviteEndpoint: this.inviteEndpoint,
			giftEndpoint: this.giftEndpoint,
			webAppEndpoint: this.webAppEndpoint,
			gifProvider: this.gifProvider,
			captchaProvider: this.captchaProvider,
			hcaptchaSiteKey: this.hcaptchaSiteKey,
			turnstileSiteKey: this.turnstileSiteKey,
			apiCodeVersion: this.apiCodeVersion,
			features: {...this.features},
			sso: this.sso ? {...this.sso} : null,
			publicPushVapidKey: this.publicPushVapidKey,
			limits: this.cloneLimits(this.limits),
			sentryDsn: this.sentryDsn,
			relayDirectoryUrl: this.relayDirectoryUrl,
			testModeEnabled: this.testModeEnabled,
		};
	}
	private createEmptyLimitConfig(): LimitConfigSnapshot {
		return {
			version: 1,
			traitDefinitions: [],
			rules: [],
		};
	}

	private cloneLimits(limits: LimitConfigSnapshot): LimitConfigSnapshot {
		return JSON.parse(JSON.stringify(limits));
	}

	private normalizeLimits(limits?: LimitConfigSnapshot): LimitConfigSnapshot {
		const cloned = this.cloneLimits(limits ?? this.createEmptyLimitConfig());
		return {
			...cloned,
			traitDefinitions: cloned.traitDefinitions ?? [],
			rules: cloned.rules ?? [],
		};
	}

	private processLimitsFromApi(limits: LimitConfigSnapshot | LimitConfigWireFormat | undefined): LimitConfigSnapshot {
		if (limits && 'defaultsHash' in limits && limits.version === 2) {
			const expanded = expandWireFormat(limits);
			this.currentDefaultsHash = limits.defaultsHash;
			return this.normalizeLimits(expanded);
		}
		this.currentDefaultsHash = null;
		return this.normalizeLimits(limits as LimitConfigSnapshot | undefined);
	}

	async withSnapshot<T>(snapshot: RuntimeConfigSnapshot, fn: () => Promise<T>): Promise<T> {
		const before = this.getSnapshot();
		this.applySnapshot(snapshot);

		try {
			return await fn();
		} finally {
			this.applySnapshot(before);
		}
	}

	async resetToDefaults(): Promise<void> {
		await this.connectToEndpoint(this.getEffectiveBootstrapEndpoint());
	}

	async connectToEndpoint(input: string): Promise<void> {
		const connectId = ++this._connectSeq;

		const apiEndpoint = this.normalizeEndpoint(input);
		const wellKnownUrl = this.buildWellKnownUrl(apiEndpoint);
		const request: HttpRequestConfig = {url: wellKnownUrl};

		try {
			const response = await HttpClient.get<InstanceDiscoveryResponse>(request);

			if (connectId !== this._connectSeq) {
				return;
			}

			if (!response.ok) {
				throw new Error(`Failed to reach ${wellKnownUrl} (${response.status})`);
			}

			this.updateFromInstance(response.body);
		} catch (e) {
			throw e;
		}
	}

	private buildWellKnownUrl(apiEndpoint: string): string {
		try {
			const url = new URL(apiEndpoint);
			url.pathname = '/.well-known/fluxer';
			return url.toString();
		} catch {
			return `${apiEndpoint.replace(/\/api\/?$/, '')}/.well-known/fluxer`;
		}
	}

	private normalizeEndpoint(input: string): string {
		const trimmed = input['trim']();
		if (!trimmed) {
			throw new Error('API endpoint is required');
		}

		let candidate = trimmed;

		if (candidate.startsWith('/')) {
			candidate = `${window.location.origin}${candidate}`;
		} else if (!/^[a-zA-Z][a-zA-Z0-9+\-.]*:\/\//.test(candidate)) {
			candidate = `https://${candidate}`;
		}

		const url = new URL(candidate);
		if (url.pathname === '' || url.pathname === '/') {
			url.pathname = '/api';
		}
		url.pathname = url.pathname.replace(/\/+$/, '');
		return url.toString();
	}

	private updateFromInstance(instance: InstanceDiscoveryResponse): void {
		this.assertCodeVersion(instance.api_code_version);

		const apiEndpoint = instance.endpoints.api_client ?? instance.endpoints.api;
		const apiPublicEndpoint = instance.endpoints.api_public ?? apiEndpoint;
		const sso = instance.sso ?? null;
		const gifProvider: GifProvider = instance.gif?.provider === 'tenor' ? 'tenor' : 'klipy';

		runInAction(() => {
			this.apiEndpoint = apiEndpoint;
			this.apiPublicEndpoint = apiPublicEndpoint;

			this.gatewayEndpoint = instance.endpoints.gateway;
			this.mediaEndpoint = instance.endpoints.media;
			this.staticCdnEndpoint = instance.endpoints.static_cdn;
			this.marketingEndpoint = instance.endpoints.marketing;
			this.adminEndpoint = instance.endpoints.admin;
			this.inviteEndpoint = instance.endpoints.invite;
			this.giftEndpoint = instance.endpoints.gift;
			this.webAppEndpoint = instance.endpoints.webapp;

			this.gifProvider = gifProvider;

			this.captchaProvider = instance.captcha.provider;
			this.hcaptchaSiteKey = instance.captcha.hcaptcha_site_key;
			this.turnstileSiteKey = instance.captcha.turnstile_site_key;

			this.apiCodeVersion = instance.api_code_version;
			this.features = instance.features;
			this.sso = sso;
			this.publicPushVapidKey = instance.push?.public_vapid_key ?? null;
			this.limits = this.processLimitsFromApi(instance.limits);

			if (instance.app_public) {
				this.sentryDsn = instance.app_public.sentry_dsn;
			}

			this.testModeEnabled = instance.test_mode_enabled ?? false;
		});
	}
	private assertCodeVersion(instanceVersion: number): void {
		if (instanceVersion < API_CODE_VERSION) {
			throw new Error(
				`Incompatible server (code version ${instanceVersion}); this client requires ${API_CODE_VERSION}.`,
			);
		}
	}

	get webAppBaseUrl(): string {
		if (this.webAppEndpoint) {
			return this.webAppEndpoint.replace(/\/$/, '');
		}

		try {
			const url = new URL(this.apiEndpoint);
			if (url.pathname.endsWith('/api')) {
				url.pathname = url.pathname.slice(0, -4) || '/';
			}
			return url.toString().replace(/\/$/, '');
		} catch {
			return this.apiEndpoint.replace(/\/api$/, '');
		}
	}

	isSelfHosted(): boolean {
		return DeveloperOptionsStore.selfHostedModeOverride || this.features.self_hosted;
	}

	get marketingHost(): string {
		try {
			return new URL(this.marketingEndpoint).host;
		} catch {
			return '';
		}
	}

	get inviteHost(): string {
		try {
			return new URL(this.inviteEndpoint).host;
		} catch {
			return '';
		}
	}

	get giftHost(): string {
		try {
			return new URL(this.giftEndpoint).host;
		} catch {
			return '';
		}
	}

	get inviteUrlBase(): string {
		try {
			const url = new URL(this.inviteEndpoint);
			const path = url.pathname !== '/' ? url.pathname.replace(/\/$/, '') : '';
			return `${url.host}${path}`;
		} catch {
			return '';
		}
	}

	get giftUrlBase(): string {
		try {
			const url = new URL(this.giftEndpoint);
			const path = url.pathname !== '/' ? url.pathname.replace(/\/$/, '') : '';
			return `${url.host}${path}`;
		} catch {
			return '';
		}
	}

	get localInstanceDomain(): string {
		try {
			const url = new URL(this.apiEndpoint);
			return url.hostname;
		} catch {
			return 'localhost';
		}
	}
}

export function describeApiEndpoint(endpoint: string): string {
	try {
		const url = new URL(endpoint);
		const path = url.pathname === '/api' ? '' : url.pathname;
		return `${url.host}${path}`;
	} catch {
		return endpoint;
	}
}

export default new RuntimeConfigStore();
