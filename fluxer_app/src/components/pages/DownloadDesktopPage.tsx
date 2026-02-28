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

import styles from '@app/components/pages/DownloadDesktopPage.module.css';
import {openExternalUrl} from '@app/utils/NativeUtils';
import {Trans} from '@lingui/react/macro';
import {DownloadSimpleIcon, WindowsLogoIcon} from '@phosphor-icons/react';

const WINDOWS_STABLE_SETUP_URL =
	'https://api.fluxer.app/dl/desktop/stable/win32/x64/latest/setup';

export function DownloadDesktopPage() {
	const handleDownloadWindows = () => {
		void openExternalUrl(WINDOWS_STABLE_SETUP_URL);
	};

	return (
		<div className={styles.container}>
			<div className={styles.card}>
				<div className={styles.iconRow}>
					<div className={styles.mainIcon}>
						<DownloadSimpleIcon weight="bold" />
					</div>
				</div>

				<h1 className={styles.title}>
					<Trans>Download Fluxer for Windows</Trans>
				</h1>
				<p className={styles.subtitle}>
					<Trans>
						Get the desktop app to stay connected, use push-to-talk, and receive system notifications.
					</Trans>
				</p>

				<button type="button" className={styles.primaryButton} onClick={handleDownloadWindows}>
					<WindowsLogoIcon weight="fill" className={styles.primaryButtonIcon} />
					<span className={styles.primaryButtonLabel}>
						<Trans>Download for Windows (.exe)</Trans>
					</span>
				</button>

				<p className={styles.helperText}>
					<Trans>Works on Windows 10 and newer, 64-bit.</Trans>
				</p>
			</div>
		</div>
	);
}

