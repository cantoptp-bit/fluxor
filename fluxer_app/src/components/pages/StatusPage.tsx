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

import {FluxerIcon} from '@app/components/icons/FluxerIcon';
import {useFluxerDocumentTitle} from '@app/hooks/useFluxerDocumentTitle';
import {Link} from '@app/lib/router/React';
import {Routes} from '@app/Routes';
import {
	CheckCircleIcon,
	CircleDashedIcon,
	WarningIcon,
	XCircleIcon,
} from '@phosphor-icons/react';
import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useState} from 'react';
import styles from './StatusPage.module.css';

type ServiceStatus = 'healthy' | 'degraded' | 'unhealthy' | 'disabled';

interface ServiceHealth {
	status: ServiceStatus;
	message?: string;
	latencyMs?: number;
	details?: Record<string, unknown>;
}

interface HealthCheckResponse {
	status: ServiceStatus;
	timestamp: string;
	uptime: number;
	version: string;
	services: {
		kv: ServiceHealth;
		s3: ServiceHealth;
		jetstream: ServiceHealth;
		mediaProxy: ServiceHealth;
		admin: ServiceHealth;
		api: ServiceHealth;
		app: ServiceHealth;
	};
}

const SERVICE_LABELS: Record<string, string> = {
	kv: 'KV Store',
	s3: 'S3 Storage',
	jetstream: 'JetStream',
	mediaProxy: 'Media Proxy',
	admin: 'Admin API',
	api: 'API',
	app: 'App Server',
};

const STATUS_DISPLAY: Record<ServiceStatus, string> = {
	healthy: 'Operational',
	degraded: 'Degraded Performance',
	unhealthy: 'Major Outage',
	disabled: 'Disabled',
};

function statusClass(status: ServiceStatus): string {
	switch (status) {
		case 'healthy':
			return styles.statusHealthy;
		case 'degraded':
			return styles.statusDegraded;
		case 'unhealthy':
			return styles.statusUnhealthy;
		case 'disabled':
			return styles.statusDisabled;
		default:
			return '';
	}
}

function StatusDot({status}: {status: ServiceStatus}) {
	return <span className={`${styles.statusDot} ${statusClass(status)}`} aria-hidden />;
}

function StatusIcon({status}: {status: ServiceStatus}) {
	const iconClass = statusClass(status);
	switch (status) {
		case 'healthy':
			return <CheckCircleIcon className={iconClass} weight="fill" size={20} />;
		case 'degraded':
			return <WarningIcon className={iconClass} weight="fill" size={20} />;
		case 'unhealthy':
			return <XCircleIcon className={iconClass} weight="fill" size={20} />;
		case 'disabled':
			return <CircleDashedIcon className={iconClass} size={20} />;
		default:
			return <CircleDashedIcon className={iconClass} size={20} />;
	}
}

function overallStatusLabel(status: ServiceStatus): string {
	switch (status) {
		case 'healthy':
			return 'All Systems Operational';
		case 'degraded':
			return 'Degraded Performance';
		case 'unhealthy':
			return 'Major Outage';
		case 'disabled':
			return 'Partial System Outage';
		default:
			return 'Unknown';
	}
}

const TIMELINE_DAYS = 60;

/** One bar per day; without history we use current status for all bars. */
function StatusTimelineBar({ status }: { status: ServiceStatus }) {
	return (
		<div className={styles.timelineBar} role="presentation" aria-hidden>
			{Array.from({ length: TIMELINE_DAYS }, (_, i) => (
				<span
					key={i}
					className={`${styles.timelineSegment} ${statusClass(status)}`}
					title={`Day ${i + 1}`}
				/>
			))}
		</div>
	);
}

export const StatusPage = observer(function StatusPage() {
	useFluxerDocumentTitle('Status');

	const [data, setData] = useState<HealthCheckResponse | null>(null);
	const [loading, setLoading] = useState(true);
	const [error, setError] = useState<string | null>(null);

	const fetchStatus = useCallback(async () => {
		setLoading(true);
		setError(null);
		try {
			const res = await fetch('/api/_status');
			if (!res.ok) {
				setError(`Request failed: ${res.status}`);
				setData(null);
				return;
			}
			const json = (await res.json()) as HealthCheckResponse;
			setData(json);
		} catch (err) {
			setError(err instanceof Error ? err.message : 'Unable to load status');
			setData(null);
		} finally {
			setLoading(false);
		}
	}, []);

	useEffect(() => {
		void fetchStatus();
	}, [fetchStatus]);

	return (
		<div className={styles.wrapper}>
			<header className={styles.topBar}>
				<div className={styles.topBarInner}>
					<Link to={Routes.HOME} className={styles.logoLink} aria-label="Back to app">
						<FluxerIcon className={styles.logo} aria-hidden />
					</Link>
					<h1 className={styles.pageTitle}>
						<Trans>Status</Trans>
					</h1>
					<button
						type="button"
						className={styles.refreshBtn}
						onClick={() => void fetchStatus()}
						disabled={loading}
					>
						<Trans>Refresh</Trans>
					</button>
				</div>
			</header>

			<main className={styles.main}>
				<div className={styles.content}>
					{loading && (
						<div className={styles.loadingState}>
							<div className={styles.spinner} aria-hidden />
							<p className={styles.loadingText}>
								<Trans>Checking services…</Trans>
							</p>
						</div>
					)}

					{error && !data && (
						<div className={styles.heroBanner} data-status="unhealthy">
							<XCircleIcon className={styles.heroIcon} size={28} weight="fill" />
							<div className={styles.heroText}>
								<p className={styles.heroTitle}>
									<Trans>Unable to load status</Trans>
								</p>
								<p className={styles.heroSub}>{error}</p>
							</div>
							<button
								type="button"
								className={styles.heroBtn}
								onClick={() => void fetchStatus()}
							>
								<Trans>Retry</Trans>
							</button>
						</div>
					)}

					{data && !loading && (
						<>
							<section className={styles.heroSection} aria-label="Overall status">
								<div
									className={styles.heroBanner}
									data-status={data.status}
									aria-live="polite"
								>
									<span className={styles.heroIconWrap}>
										<StatusIcon status={data.status} />
									</span>
									<div className={styles.heroText}>
										<p className={styles.heroTitle}>{overallStatusLabel(data.status)}</p>
										<p className={styles.heroSub}>
											<Trans>Last checked</Trans>: {data.timestamp ? new Date(data.timestamp).toLocaleString() : '—'}
											{data.uptime != null && (
												<>
													{' · '}
													<Trans>Uptime</Trans>: {Math.floor(data.uptime / 3600)}h {Math.floor((data.uptime % 3600) / 60)}m
												</>
											)}
										</p>
									</div>
								</div>
								{(data.status === 'degraded' || data.status === 'unhealthy') && (
									<div
										className={styles.alertBanner}
										data-status={data.status}
										role="alert"
									>
										<WarningIcon className={styles.alertIcon} weight="fill" size={20} aria-hidden />
										<p className={styles.alertText}>
											<Trans>Some services are currently affected.</Trans>
										</p>
									</div>
								)}
								<div className={styles.legend} role="presentation">
									<span className={styles.legendItem}>
										<StatusDot status="healthy" />
										<Trans>Operational</Trans>
									</span>
									<span className={styles.legendItem}>
										<StatusDot status="degraded" />
										<Trans>Degraded</Trans>
									</span>
									<span className={styles.legendItem}>
										<StatusDot status="unhealthy" />
										<Trans>Outage</Trans>
									</span>
									<span className={styles.legendItem}>
										<StatusDot status="disabled" />
										<Trans>Disabled</Trans>
									</span>
								</div>
							</section>

							<section className={styles.servicesSection} aria-label="Services">
								<h2 className={styles.sectionTitle}>
									<Trans>Services</Trans>
								</h2>
								<p className={styles.sectionSubtitle}>
									<Trans>Past 60 days</Trans> · <Trans>Today</Trans>
								</p>
								<div className={styles.servicesCard}>
									{Object.entries(data.services).map(([key, svc]) => (
										<div key={key} className={styles.serviceRow}>
											<StatusDot status={svc.status} />
											<div className={styles.serviceNameBlock}>
												<span className={styles.serviceName}>{SERVICE_LABELS[key] ?? key}</span>
												{svc.message && (
													<span className={styles.serviceMessage}>{svc.message}</span>
												)}
											</div>
											<div className={styles.serviceTimeline}>
												<StatusTimelineBar status={svc.status} />
											</div>
											<div className={styles.serviceMeta}>
												<span className={statusClass(svc.status)}>{STATUS_DISPLAY[svc.status]}</span>
												{svc.latencyMs != null && (
													<span className={styles.latency}>{svc.latencyMs} ms</span>
												)}
											</div>
										</div>
									))}
								</div>
							</section>
						</>
					)}
				</div>
			</main>
		</div>
	);
});
