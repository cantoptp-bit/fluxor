import styles from '@app/components/auth/AuthPatchNotes.module.css';
import { APP_VERSION_LABEL } from '@app/lib/AppVersion';
import { Trans } from '@lingui/react/macro';
import { ArrowRightIcon, RocketLaunchIcon, SparkleIcon, TrendUpIcon, WrenchIcon } from '@phosphor-icons/react';

type WhatsNewItemType = 'new' | 'improved' | 'fixed';

interface WhatsNewItem {
	type: WhatsNewItemType;
	icon: typeof RocketLaunchIcon;
	title: React.ReactNode;
	description?: React.ReactNode;
	bulletPoints?: Array<React.ReactNode>;
}

const WHATS_NEW_DATE_LABEL = '2/27/2026';

const WHATS_NEW_ITEMS: Array<WhatsNewItem> = [
	{
		type: 'new',
		icon: RocketLaunchIcon,
		title: <Trans>Revamped UI design</Trans>,
		bulletPoints: [
			<Trans key="ui-1">Refined typography and spacing on core messaging and navigation views.</Trans>,
			<Trans key="ui-2">Cleaner panels and cards for better visual hierarchy.</Trans>,
		],
	},
	{
		type: 'fixed',
		icon: WrenchIcon,
		title: <Trans>Favorites modal</Trans>,
		bulletPoints: [
			<Trans key="fav-1">Communities and servers load consistently in the Favorites list.</Trans>,
			<Trans key="fav-2">Direct messages are no longer mixed into the Favorites selector.</Trans>,
		],
	},
	{
		type: 'improved',
		icon: TrendUpIcon,
		title: <Trans>Login reliability</Trans>,
		bulletPoints: [
			<Trans key="lp-1">More resilient session handling after long idle periods.</Trans>,
			<Trans key="lp-2">Clearer error messages when a code expires or is reused.</Trans>,
		],
	},
];

export function AuthPatchNotes() {
	return (
		<aside className={styles.wrapper} aria-label="What's new">
			<div className={styles.card}>
				<header className={styles.header}>
					<div className={styles.headerMain}>
						<h2 className={styles.headerTitle}>
							<SparkleIcon weight="fill" className={styles.headerTitleIcon} aria-hidden />
							<Trans>What&apos;s New</Trans>
						</h2>
					</div>
					<div className={styles.headerMeta}>
						<p className={styles.headerDate}>{WHATS_NEW_DATE_LABEL}</p>
					</div>
				</header>

				<ul className={styles.list}>
					{WHATS_NEW_ITEMS.map((item, index) => {
						const Icon = item.icon;
						return (
							<li key={index} className={styles.listItem}>
								<div className={styles.itemIconWrapper} aria-hidden>
									<span
										className={`${styles.itemTypeBadge} ${
											item.type === 'new'
												? styles.itemTypeNew
												: item.type === 'fixed'
													? styles.itemTypeFixed
													: styles.itemTypeImproved
										}`}
									>
										<Icon weight="bold" className={styles.itemTypeIcon} aria-hidden />
									</span>
								</div>
								<div className={styles.itemBody}>
									<p className={styles.itemTitle}>{item.title}</p>
									{item.description && <p className={styles.itemDescription}>{item.description}</p>}
									{item.bulletPoints && item.bulletPoints.length > 0 && (
										<ul className={styles.itemBulletList}>
											{item.bulletPoints.map((point, bulletIndex) => (
												<li key={bulletIndex} className={styles.itemBullet}>
													{point}
												</li>
											))}
										</ul>
									)}
								</div>
							</li>
						);
					})}
				</ul>

				<footer className={styles.footer}>
					<div className={styles.footerMeta}>
						<span className={styles.footerVersion}>{APP_VERSION_LABEL}</span>
					</div>
					<a href="/changelog" className={styles.footerButton}>
						<span className={styles.footerButtonLabel}>
							<Trans>View all changes</Trans>
						</span>
						<span className={styles.footerButtonIconWrapper}>
							<ArrowRightIcon weight="bold" className={styles.footerButtonIcon} aria-hidden />
						</span>
					</a>
				</footer>
			</div>
		</aside>
	);
}
