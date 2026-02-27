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
import type {UserRecord} from '@app/records/UserRecord';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useRef, useState} from 'react';
import styles from '@app/components/modals/TempChatUnlockModal.module.css';

export interface TempChatSetPasswordBeforeCreateModalProps {
	/** The user you're starting the temp chat with (for display). */
	user: UserRecord;
	onConfirm: (password: string) => Promise<void>;
	onCancel: () => void;
}

/**
 * Shown before creating a temp chat. User must set a password for that chat.
 * Supports multiple temp chats with the same user; each chat has its own password.
 */
export const TempChatSetPasswordBeforeCreateModal = observer(
	({user, onConfirm, onCancel}: TempChatSetPasswordBeforeCreateModalProps) => {
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
			const trimmed = password.trim();
			if (trimmed.length < 6) {
				setError(t`Use at least 6 characters`);
				return;
			}
			setSubmitting(true);
			setError(null);
			try {
				await onConfirm(trimmed);
			} catch (err) {
				const message =
					err && typeof err === 'object' && 'message' in err
						? String((err as {message: string}).message)
						: t`Something went wrong`;
				setError(message);
			} finally {
				setSubmitting(false);
			}
		}, [password, confirm, onConfirm, t]);

		return (
			<Modal.Root size="small" initialFocusRef={inputRef} centered>
				<Modal.Header title={t`Set password for this chat`}>
					<p className={styles.subtitle}>
						<Trans>
							Set a password to protect this temp chat with <strong>{user.username ?? user.globalName ?? user.id}</strong>.
							You'll need it to open this chat again.
						</Trans>
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
							onChange={(e) => {
								setPassword(e.target.value);
								setError(null);
							}}
							onKeyDown={(e) => e.key === 'Enter' && handleSubmit()}
							autoComplete="new-password"
							aria-invalid={!!error}
						/>
						<input
							type="password"
							className={styles.input}
							placeholder={t`Confirm password`}
							value={confirm}
							onChange={(e) => {
								setConfirm(e.target.value);
								setError(null);
							}}
							onKeyDown={(e) => e.key === 'Enter' && handleSubmit()}
							autoComplete="new-password"
							aria-invalid={!!error}
						/>
						{error && (
							<p className={styles.error} role="alert">
								{error}
							</p>
						)}
					</div>
				</Modal.Content>
				<Modal.Footer className={styles.footer}>
					<Button variant="secondary" onClick={onCancel}>
						{t`Cancel`}
					</Button>
					<Button
						onClick={handleSubmit}
						submitting={submitting}
						disabled={!password.trim() || !confirm.trim()}
					>
						{t`Create temp chat`}
					</Button>
				</Modal.Footer>
			</Modal.Root>
		);
	},
);
