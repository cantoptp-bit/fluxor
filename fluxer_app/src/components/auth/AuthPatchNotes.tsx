import styles from '@app/components/auth/AuthPatchNotes.module.css';
import {Trans} from '@lingui/react/macro';
import {ArrowRightIcon, RocketLaunchIcon, SparkleIcon, TrendUpIcon, WrenchIcon} from '@phosphor-icons/react';

export function AuthPatchNotes() {
	return (
		<aside className={styles.wrapper} aria-label="What's new">
			<div className={styles.card}>
				<header className={styles.header}>
					<span className={styles.versionPill}>v0.9.3</span>
					<div className={styles.headerBody}>
						<h2 className={styles.headerTitle}>
							<SparkleIcon weight="fill" className={styles.headerTitleIcon} aria-hidden />
							<Trans>What's New</Trans>
						</h2>
						<p className={styles.headerDate}>
							<Trans>February 2026</Trans>
						</p>
					</div>
				</header>

				<div className={styles.timeline}>
					<section className={styles.timelineEntry}>
						<div className={`${styles.timelineDot} ${styles.dotNew}`} aria-hidden />
						<div className={styles.timelineBody}>
							<div className={styles.entryHeader}>
								<span className={styles.badgeNew}>
									<RocketLaunchIcon weight="bold" className={styles.badgeIcon} aria-hidden />
									<Trans>New</Trans>
								</span>
								<span className={styles.entryTitle}>
									<Trans>Revamped UI design</Trans>
								</span>
							</div>
							<p className={styles.entryText}>
								<Trans>
									Experience a fresh new look! Check out our revamped UI with better contrast and sleeker design
									elements across the app.
								</Trans>
							</p>
						</div>
					</section>

					<section className={styles.timelineEntry}>
						<div className={`${styles.timelineDot} ${styles.dotFixed}`} aria-hidden />
						<div className={styles.timelineBody}>
							<div className={styles.entryHeader}>
								<span className={styles.badgeFixed}>
									<WrenchIcon weight="bold" className={styles.badgeIcon} aria-hidden />
									<Trans>Fixed</Trans>
								</span>
								<span className={styles.entryTitle}>
									<Trans>Favorites modal display</Trans>
								</span>
							</div>
							<p className={styles.entryText}>
								<Trans>
									The "Add Favorites" modal now displays communities correctly again without attempting to fetch
									direct messages.
								</Trans>
							</p>
						</div>
					</section>

					<section className={styles.timelineEntry}>
						<div className={`${styles.timelineDot} ${styles.dotImproved}`} aria-hidden />
						<div className={styles.timelineBody}>
							<div className={styles.entryHeader}>
								<span className={styles.badgeImproved}>
									<TrendUpIcon weight="bold" className={styles.badgeIcon} aria-hidden />
									<Trans>Improved</Trans>
								</span>
								<span className={styles.entryTitle}>
									<Trans>Login stability</Trans>
								</span>
							</div>
							<p className={styles.entryText}>
								<Trans>
									Login stability has been enhanced, especially when returning from idle states or checking
									multi-factor authentication codes.
								</Trans>
							</p>
						</div>
					</section>
				</div>

				<footer className={styles.footer}>
					<a href="/changelog" className={styles.footerButton}>
						<Trans>View all changes</Trans>
						<ArrowRightIcon weight="bold" className={styles.footerButtonIcon} aria-hidden />
					</a>
				</footer>
			</div>
		</aside>
	);
}
