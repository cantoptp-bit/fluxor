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

import * as Modal from '@app/components/modals/Modal';
import {Button} from '@app/components/uikit/button/Button';
import * as TempChatLockStore from '@app/stores/TempChatLockStore';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useRef, useState} from 'react';
import styles from '@app/components/modals/TempChatUnlockModal.module.css';

export interface TempChatSetChatPasswordModalProps {
	userId: string;
	tempChatId: string;
	onDone: () => void;
	onCancel: () => void;
}

export const TempChatSetChatPasswordModal = observer(
	({userId, tempChatId, onDone, onCancel}: TempChatSetChatPasswordModalProps) => {
		const {t} = useLingui();
		const [password, setPassword] = useState('');
		const [confirm, setConfirm] = useState('');
		const [error, setError] = useState<string | null>(null);
		const [submitting, setSubmitting] = useState(false);
		const inputRef = useRef<HTMLInputElement>(null);

		const handleSubmit = useCallback(async () => {
			if (password !== confirm) {
				setError(t`Passwords do not match`);
				return;
			}
			if (password.trim().length < 6) {
				setError(t`Use at least 6 characters`);
				return;
			}
			setSubmitting(true);
			setError(null);
			try {
				const ok = await TempChatLockStore.setChatPassword(userId, tempChatId, password.trim(), null);
				if (ok) {
					onDone();
				} else {
					setError(t`Failed to set password. Make sure you're unlocked.`);
				}
			} catch {
				setError(t`Something went wrong`);
			} finally {
				setSubmitting(false);
			}
		}, [userId, tempChatId, password, confirm, onDone, t]);

		return (
			<Modal.Root size="small" initialFocusRef={inputRef} centered>
				<Modal.Header title={t`Password for this chat`}>
					<p className={styles.subtitle}>
						{t`You can unlock this chat with this password or your master password.`}
					</p>
				</Modal.Header>
				<Modal.Content>
					<div className={styles.content}>
						<input
							ref={inputRef}
							type="password"
							className={styles.input}
							placeholder={t`Password`}
							value={password}
							onChange={(e) => { setPassword(e.target.value); setError(null); }}
							onKeyDown={(e) => e.key === 'Enter' && handleSubmit()}
							autoComplete="new-password"
							aria-invalid={!!error}
						/>
						<input
							type="password"
							className={styles.input}
							placeholder={t`Confirm password`}
							value={confirm}
							onChange={(e) => { setConfirm(e.target.value); setError(null); }}
							onKeyDown={(e) => e.key === 'Enter' && handleSubmit()}
							autoComplete="new-password"
							aria-invalid={!!error}
						/>
						{error && <p className={styles.error} role="alert">{error}</p>}
					</div>
				</Modal.Content>
				<Modal.Footer className={styles.footer}>
					<Button variant="secondary" onClick={onCancel}>{t`Cancel`}</Button>
					<Button onClick={handleSubmit} submitting={submitting} disabled={!password.trim() || !confirm.trim()}>
						{t`Set password`}
					</Button>
				</Modal.Footer>
			</Modal.Root>
		);
	},
);
