import styles from '@app/components/auth/AuthPatchNotes.module.css';
import { Trans } from '@lingui/react/macro';
import { SparkleIcon } from '@phosphor-icons/react';

export function AuthPatchNotes() {
	return (
		<aside className={styles.wrapper} aria-label="What's new">
			<div className={styles.card}>
				<header className={styles.header}>
					<SparkleIcon weight="fill" className={styles.headerIcon} aria-hidden />
					<h2 className={styles.headerTitle}>
						<Trans>What's New</Trans>
					</h2>
				</header>

				<div className={styles.content}>
					<section className={styles.entry}>
						<span className={styles.badgeNew}><Trans>New</Trans></span>
						<p className={styles.entryText}>
							<Trans>Experience a fresh new look! Check out our revamped UI with better contrast and sleeker design elements across the app.</Trans>
						</p>
					</section>
					<section className={styles.entry}>
						<span className={styles.badgeFixed}><Trans>Fixed</Trans></span>
						<p className={styles.entryText}>
							<Trans>The "Add Favorites" modal now displays communities correctly again without attempting to fetch direct messages.</Trans>
						</p>
					</section>
					<section className={styles.entry}>
						<span className={styles.badgeImproved}><Trans>Improved</Trans></span>
						<p className={styles.entryText}>
							<Trans>Login stability has been enhanced, especially when returning from idle states or checking multi-factor authentication codes.</Trans>
						</p>
					</section>
				</div>

				<footer className={styles.footer}>
					<a href="/changelog" className={styles.footerLink}>
						<Trans>Read full patch notes</Trans>
					</a>
				</footer>
			</div>
		</aside>
	);
}
