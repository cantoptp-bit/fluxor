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

export interface TempChatUnlockModalProps {
	userId: string;
	tempChatId: string;
	onUnlocked: () => void;
	onCancel: () => void;
}

export const TempChatUnlockModal = observer(
	({userId, tempChatId, onUnlocked, onCancel}: TempChatUnlockModalProps) => {
		const {t} = useLingui();
		const [password, setPassword] = useState('');
		const [error, setError] = useState<string | null>(null);
		const [submitting, setSubmitting] = useState(false);
		const inputRef = useRef<HTMLInputElement>(null);

		const hasMaster = TempChatLockStore.hasMasterPassword(userId);
		const hasChat = TempChatLockStore.hasChatPassword(tempChatId);

		const handleSubmit = useCallback(async () => {
			const p = password.trim();
			if (!p) {
				setError(t`Enter your password`);
				return;
			}
			setSubmitting(true);
			setError(null);
			try {
				if (hasMaster) {
					const pair = await TempChatLockStore.unlockWithMasterPassword(userId, p);
					if (pair) {
						onUnlocked();
						return;
					}
				}
				if (hasChat) {
					const pair = await TempChatLockStore.unlockWithChatPassword(userId, tempChatId, p);
					if (pair) {
						onUnlocked();
						return;
					}
				}
				setError(t`Incorrect password`);
			} catch {
				setError(t`Something went wrong. Try again.`);
			} finally {
				setSubmitting(false);
			}
		}, [userId, tempChatId, password, hasMaster, hasChat, onUnlocked, t]);

		return (
			<Modal.Root size="small" initialFocusRef={inputRef} centered>
				<Modal.Header title={t`Unlock temp chat`}>
					<p className={styles.subtitle}>
						{hasChat && hasMaster
							? t`Enter your master password or this chat's password`
							: hasChat
								? t`Enter this chat's password`
								: t`Enter your master password`}
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
							onKeyDown={(e) => {
								if (e.key === 'Enter') handleSubmit();
							}}
							autoComplete="current-password"
							aria-invalid={!!error}
							aria-describedby={error ? 'temp-chat-unlock-error' : undefined}
						/>
						{error && (
							<p id="temp-chat-unlock-error" className={styles.error} role="alert">
								{error}
							</p>
						)}
					</div>
				</Modal.Content>
				<Modal.Footer className={styles.footer}>
					<Button variant="secondary" onClick={onCancel}>
						{t`Cancel`}
					</Button>
					<Button onClick={handleSubmit} submitting={submitting} disabled={!password.trim()}>
						{t`Unlock`}
					</Button>
				</Modal.Footer>
			</Modal.Root>
		);
	},
);
