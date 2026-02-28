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

import styles from '@app/components/pages/ChangelogPage.module.css';
import {useFluxerDocumentTitle} from '@app/hooks/useFluxerDocumentTitle';
import i18n, {initI18n} from '@app/I18n';
import {I18nProvider} from '@lingui/react';
import {Trans, useLingui} from '@lingui/react/macro';
import {
	ArrowLeftIcon,
	RocketLaunchIcon,
	SparkleIcon,
	TrendUpIcon,
	WrenchIcon,
} from '@phosphor-icons/react';
import {type ReactNode, useEffect, useState} from 'react';

type EntryKind = 'new' | 'fixed' | 'improved';

interface ChangelogEntry {
	kind: EntryKind;
	title: ReactNode;
	description: ReactNode;
}

interface Release {
	version: string;
	date: ReactNode;
	latest?: boolean;
	entries: Array<ChangelogEntry>;
}

const BADGE_STYLES: Record<EntryKind, string> = {
	new: styles.badgeNew,
	fixed: styles.badgeFixed,
	improved: styles.badgeImproved,
};

const DOT_STYLES: Record<EntryKind, string> = {
	new: styles.dotNew,
	fixed: styles.dotFixed,
	improved: styles.dotImproved,
};

function BadgeIcon({kind}: {kind: EntryKind}) {
	const cls = styles.badgeIcon;
	switch (kind) {
		case 'new':
			return <RocketLaunchIcon weight="bold" className={cls} aria-hidden />;
		case 'fixed':
			return <WrenchIcon weight="bold" className={cls} aria-hidden />;
		case 'improved':
			return <TrendUpIcon weight="bold" className={cls} aria-hidden />;
	}
}

function BadgeLabel({kind}: {kind: EntryKind}) {
	switch (kind) {
		case 'new':
			return <Trans>New</Trans>;
		case 'fixed':
			return <Trans>Fixed</Trans>;
		case 'improved':
			return <Trans>Improved</Trans>;
	}
}

function ReleaseCard({release}: {release: Release}) {
	return (
		<article className={styles.release}>
			<header className={styles.releaseHeader}>
				<span className={styles.versionPill}>{release.version}</span>
				<div className={styles.releaseHeaderBody}>
					<h2 className={styles.releaseTitle}>{release.date}</h2>
				</div>
				{release.latest && (
					<span className={styles.latestBadge}>
						<Trans>Latest</Trans>
					</span>
				)}
			</header>

			<div className={styles.timeline}>
				{release.entries.map((entry, i) => (
					<section key={i} className={styles.entry}>
						<div className={`${styles.dot} ${DOT_STYLES[entry.kind]}`} aria-hidden />
						<div className={styles.entryBody}>
							<div className={styles.entryHeader}>
								<span className={BADGE_STYLES[entry.kind]}>
									<BadgeIcon kind={entry.kind} />
									<BadgeLabel kind={entry.kind} />
								</span>
								<span className={styles.entryTitle}>{entry.title}</span>
							</div>
							<p className={styles.entryText}>{entry.description}</p>
						</div>
					</section>
				))}
			</div>
		</article>
	);
}

function useReleases(): Array<Release> {
	return [
		{
			version: 'v0.9.3',
			date: <Trans>February 25, 2026</Trans>,
			latest: true,
			entries: [
				{
					kind: 'new',
					title: <Trans>Revamped UI design</Trans>,
					description: (
						<Trans>
							Experience a fresh new look! Check out our revamped UI with better contrast and sleeker design
							elements across the app.
						</Trans>
					),
				},
				{
					kind: 'new',
					title: <Trans>Temporary chats</Trans>,
					description: (
						<Trans>
							Start encrypted temporary chats with any user. Each chat is password-protected and end-to-end
							encrypted for your privacy.
						</Trans>
					),
				},
				{
					kind: 'fixed',
					title: <Trans>Favorites modal display</Trans>,
					description: (
						<Trans>
							The "Add Favorites" modal now displays communities correctly again without attempting to fetch
							direct messages.
						</Trans>
					),
				},
				{
					kind: 'fixed',
					title: <Trans>Friend relationship loading</Trans>,
					description: (
						<Trans>
							Opening a DM now correctly loads the friendship status, so action buttons like "Remove Friend" or
							"Start Temp Chat" show up reliably.
						</Trans>
					),
				},
				{
					kind: 'improved',
					title: <Trans>Login stability</Trans>,
					description: (
						<Trans>
							Login stability has been enhanced, especially when returning from idle states or checking
							multi-factor authentication codes.
						</Trans>
					),
				},
			],
		},
		{
			version: 'v0.9.2',
			date: <Trans>February 10, 2026</Trans>,
			entries: [
				{
					kind: 'new',
					title: <Trans>Favorites system</Trans>,
					description: (
						<Trans>
							Pin your favorite channels and friends to a dedicated sidebar section for quick access. Add up to
							50 favorites across servers.
						</Trans>
					),
				},
				{
					kind: 'new',
					title: <Trans>Custom themes</Trans>,
					description: (
						<Trans>
							Create and share custom color themes. Apply themes from the community or craft your own in the
							appearance settings.
						</Trans>
					),
				},
				{
					kind: 'improved',
					title: <Trans>Message search speed</Trans>,
					description: (
						<Trans>
							Search results now load up to 3x faster thanks to optimized indexing on the backend. Large servers
							will see the biggest improvement.
						</Trans>
					),
				},
				{
					kind: 'fixed',
					title: <Trans>Notification badge counts</Trans>,
					description: (
						<Trans>
							Badge counts on the server list now update in real-time and no longer show stale numbers after
							reading messages.
						</Trans>
					),
				},
			],
		},
		{
			version: 'v0.9.1',
			date: <Trans>January 28, 2026</Trans>,
			entries: [
				{
					kind: 'new',
					title: <Trans>OAuth2 application support</Trans>,
					description: (
						<Trans>
							Developers can now register OAuth2 applications and use Fluxer as an identity provider. Manage
							your apps from the developer portal.
						</Trans>
					),
				},
				{
					kind: 'improved',
					title: <Trans>File upload experience</Trans>,
					description: (
						<Trans>
							Drag-and-drop uploads now show a live progress bar with estimated time remaining. Failed uploads
							can be retried without re-selecting the file.
						</Trans>
					),
				},
				{
					kind: 'improved',
					title: <Trans>Accessibility improvements</Trans>,
					description: (
						<Trans>
							Screen reader support has been expanded across the friend list, DM list, and settings pages.
							Keyboard navigation is now fully functional in all modal dialogs.
						</Trans>
					),
				},
				{
					kind: 'fixed',
					title: <Trans>Emoji picker scroll position</Trans>,
					description: (
						<Trans>
							The emoji picker no longer resets its scroll position when switching between categories or using
							the search bar.
						</Trans>
					),
				},
			],
		},
		{
			version: 'v0.9.0',
			date: <Trans>January 15, 2026</Trans>,
			entries: [
				{
					kind: 'new',
					title: <Trans>Initial public release</Trans>,
					description: (
						<Trans>
							Fluxer launches with real-time messaging, voice channels, server creation, friend system, and a
							fully responsive web client. Welcome aboard!
						</Trans>
					),
				},
				{
					kind: 'new',
					title: <Trans>Community server discovery</Trans>,
					description: (
						<Trans>
							Browse and join public community servers through the built-in discovery directory. Filter by
							category, language, and member count.
						</Trans>
					),
				},
				{
					kind: 'new',
					title: <Trans>Role management</Trans>,
					description: (
						<Trans>
							Server owners can create custom roles with granular permissions, assign colors, and control channel
							access for each role.
						</Trans>
					),
				},
			],
		},
	];
}

function ChangelogPageContent() {
	const {t} = useLingui();
	useFluxerDocumentTitle(t`Changelog`);

	const releases = useReleases();

	return (
		<div className={styles.page}>
			<div className={styles.hero}>
				<SparkleIcon weight="fill" className={styles.heroIcon} aria-hidden />
				<h1 className={styles.heroTitle}>
					<Trans>Changelog</Trans>
				</h1>
				<p className={styles.heroSubtitle}>
					<Trans>All the latest updates, improvements, and fixes to Fluxer — in one place.</Trans>
				</p>
				<a href="/login" className={styles.backLink}>
					<ArrowLeftIcon weight="bold" className={styles.backLinkIcon} aria-hidden />
					<Trans>Back to login</Trans>
				</a>
			</div>

			<div className={styles.releases}>
				{releases.map((release) => (
					<ReleaseCard key={release.version} release={release} />
				))}
			</div>
		</div>
	);
}

export default function ChangelogPage() {
	const [ready, setReady] = useState(false);

	useEffect(() => {
		initI18n().then(() => setReady(true));
	}, []);

	if (!ready) return null;

	return (
		<I18nProvider i18n={i18n}>
			<ChangelogPageContent />
		</I18nProvider>
	);
}
