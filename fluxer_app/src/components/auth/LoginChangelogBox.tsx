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

import styles from '@app/components/auth/LoginChangelogBox.module.css';
import { Trans } from '@lingui/react/macro';
import { CaretDownIcon, CaretUpIcon, ListChecksIcon } from '@phosphor-icons/react';
import { useState } from 'react';

export function LoginChangelogBox() {
	const [expanded, setExpanded] = useState(false);

	return (
		<div className={styles.wrapper}>
			<button
				type="button"
				className={styles.trigger}
				onClick={() => setExpanded((e) => !e)}
				aria-expanded={expanded}
			>
				<ListChecksIcon className={styles.triggerIcon} weight="regular" />
				<span className={styles.triggerText}>
					{expanded ? <Trans>Hide changelog</Trans> : <Trans>View changelog</Trans>}
				</span>
				{expanded ? (
					<CaretUpIcon className={styles.triggerCaret} weight="bold" />
				) : (
					<CaretDownIcon className={styles.triggerCaret} weight="bold" />
				)}
			</button>
			{expanded && (
				<div className={styles.box} role="region" aria-label="Changelog">
					<h3 className={styles.title}>
						<Trans>Changelog</Trans>
					</h3>
					<div className={styles.content}>
						<div className={styles.entry}>
							<span className={styles.badgeNew}>
								<Trans>New</Trans>
							</span>
							<p className={styles.entryText}>
								<Trans>Experience a fresh new look! Revamped UI with better contrast and sleeker design across the app.</Trans>
							</p>
						</div>
						<div className={styles.entry}>
							<span className={styles.badgeFixed}>
								<Trans>Fixed</Trans>
							</span>
							<p className={styles.entryText}>
								<Trans>The "Add Favorites" modal now displays communities correctly again without attempting to fetch direct messages.</Trans>
							</p>
						</div>
						<div className={styles.entry}>
							<span className={styles.badgeImproved}>
								<Trans>Improved</Trans>
							</span>
							<p className={styles.entryText}>
								<Trans>Login stability enhanced when returning from idle or checking multi-factor authentication codes.</Trans>
							</p>
						</div>
						<div className={styles.entry}>
							<span className={styles.badgeNew}>
								<Trans>New</Trans>
							</span>
							<p className={styles.entryText}>
								<Trans>Encrypted direct messages for private conversations with friends.</Trans>
							</p>
						</div>
					</div>
				</div>
			)}
		</div>
	);
}
