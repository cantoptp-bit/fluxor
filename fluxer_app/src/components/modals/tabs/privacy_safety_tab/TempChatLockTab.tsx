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

import * as ToastActionCreators from '@app/actions/ToastActionCreators';
import {Button} from '@app/components/uikit/button/Button';
import {getOrCreateKeyPair} from '@app/stores/TempChatKeyStore';
import * as TempChatLockStore from '@app/stores/TempChatLockStore';
import AuthenticationStore from '@app/stores/AuthenticationStore';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useState} from 'react';
import styles from '@app/components/modals/tabs/privacy_safety_tab/TempChatLockTab.module.css';

export const TempChatLockTabContent: React.FC = observer(() => {
	const {t} = useLingui();
	const userId = AuthenticationStore.currentUserId ?? '';
	const hasMaster = TempChatLockStore.hasMasterPassword(userId);
	const locked = TempChatLockStore.isLocked(userId);

	const [unlockPassword, setUnlockPassword] = useState('');
	const [setPassword, setSetPassword] = useState('');
	const [setPasswordConfirm, setSetPasswordConfirm] = useState('');
	const [changeCurrent, setChangeCurrent] = useState('');
	const [changeNew, setChangeNew] = useState('');
	const [changeNewConfirm, setChangeNewConfirm] = useState('');
	const [submitting, setSubmitting] = useState(false);
	const [unlockError, setUnlockError] = useState<string | null>(null);

	const handleUnlock = useCallback(async () => {
		if (!unlockPassword.trim()) return;
		setSubmitting(true);
		setUnlockError(null);
		try {
			const pair = await TempChatLockStore.unlockWithMasterPassword(userId, unlockPassword.trim());
			if (pair) {
				setUnlockPassword('');
				ToastActionCreators.createToast({type: 'success', children: t`Temp chat unlocked`});
			} else {
				setUnlockError(t`Incorrect password`);
			}
		} catch {
			setUnlockError(t`Something went wrong`);
		} finally {
			setSubmitting(false);
		}
	}, [userId, unlockPassword, t]);

	const handleSetMasterPassword = useCallback(async () => {
		if (setPassword !== setPasswordConfirm || !setPassword.trim()) {
			ToastActionCreators.createToast({type: 'error', children: t`Passwords do not match`});
			return;
		}
		if (setPassword.length < 6) {
			ToastActionCreators.createToast({type: 'error', children: t`Use at least 6 characters`});
			return;
		}
		setSubmitting(true);
		try {
			const keyPair = await getOrCreateKeyPair(userId);
			if (!keyPair) {
				ToastActionCreators.createToast({type: 'error', children: t`Unlock temp chat first, then set a master password`});
				return;
			}
			const {exportPrivateKeyBase64} = await import('@app/lib/E2EEncryption');
			const privateKeyBase64 = await exportPrivateKeyBase64(keyPair.privateKey);
			const ok = await TempChatLockStore.setMasterPassword(userId, setPassword.trim(), privateKeyBase64);
			if (ok) {
				setSetPassword('');
				setSetPasswordConfirm('');
				ToastActionCreators.createToast({type: 'success', children: t`Master password set`});
			} else {
				ToastActionCreators.createToast({type: 'error', children: t`Failed to set master password`});
			}
		} catch {
			ToastActionCreators.createToast({type: 'error', children: t`Failed to set master password`});
		} finally {
			setSubmitting(false);
		}
	}, [userId, setPassword, setPasswordConfirm, t]);

	const handleChangeMasterPassword = useCallback(async () => {
		if (changeNew !== changeNewConfirm || !changeNew.trim()) {
			ToastActionCreators.createToast({type: 'error', children: t`New passwords do not match`});
			return;
		}
		if (changeNew.length < 6) {
			ToastActionCreators.createToast({type: 'error', children: t`Use at least 6 characters`});
			return;
		}
		setSubmitting(true);
		try {
			const ok = await TempChatLockStore.changeMasterPassword(userId, changeCurrent.trim(), changeNew.trim());
			if (ok) {
				setChangeCurrent('');
				setChangeNew('');
				setChangeNewConfirm('');
				ToastActionCreators.createToast({type: 'success', children: t`Master password changed`});
			} else {
				ToastActionCreators.createToast({type: 'error', children: t`Current password is incorrect`});
			}
		} catch {
			ToastActionCreators.createToast({type: 'error', children: t`Failed to change password`});
		} finally {
			setSubmitting(false);
		}
	}, [userId, changeCurrent, changeNew, changeNewConfirm, t]);

	const handleLockSession = useCallback(() => {
		TempChatLockStore.lockSession(userId);
		ToastActionCreators.createToast({type: 'success', children: t`Temp chat locked. Enter your password when you open a temp chat.`});
	}, [userId, t]);

	const handleRemoveMasterPassword = useCallback(() => {
		TempChatLockStore.clearMasterPassword(userId);
		ToastActionCreators.createToast({type: 'success', children: t`Master password removed`});
	}, [userId, t]);

	if (!userId) return null;

	return (
		<div className={styles.container}>
			<p className={styles.description}>
				<Trans>
					Protect your temp (encrypted) chats with a master password. You can also set a separate password per chat. Once set, you'll need to enter the password to open temp chats.
				</Trans>
			</p>

			{locked && hasMaster && (
				<div className={styles.block}>
					<h3 className={styles.heading}>{t`Unlock`}</h3>
					<p className={styles.hint}>{t`Enter your master password to unlock temp chats for this session.`}</p>
					<input
						type="password"
						className={styles.input}
						placeholder={t`Master password`}
						value={unlockPassword}
						onChange={(e) => { setUnlockPassword(e.target.value); setUnlockError(null); }}
						onKeyDown={(e) => e.key === 'Enter' && handleUnlock()}
						aria-invalid={!!unlockError}
					/>
					{unlockError && <p className={styles.error} role="alert">{unlockError}</p>}
					<Button onClick={handleUnlock} submitting={submitting} disabled={!unlockPassword.trim()}>
						{t`Unlock`}
					</Button>
				</div>
			)}

			{!hasMaster && (
				<div className={styles.block}>
					<h3 className={styles.heading}>{t`Set master password`}</h3>
					<input
						type="password"
						className={styles.input}
						placeholder={t`New password`}
						value={setPassword}
						onChange={(e) => setSetPassword(e.target.value)}
					/>
					<input
						type="password"
						className={styles.input}
						placeholder={t`Confirm password`}
						value={setPasswordConfirm}
						onChange={(e) => setSetPasswordConfirm(e.target.value)}
					/>
					<Button onClick={handleSetMasterPassword} submitting={submitting} disabled={!setPassword.trim() || !setPasswordConfirm.trim()}>
						{t`Set master password`}
					</Button>
				</div>
			)}

			{hasMaster && !locked && (
				<>
					<div className={styles.block}>
						<h3 className={styles.heading}>{t`Change master password`}</h3>
						<input
							type="password"
							className={styles.input}
							placeholder={t`Current password`}
							value={changeCurrent}
							onChange={(e) => setChangeCurrent(e.target.value)}
						/>
						<input
							type="password"
							className={styles.input}
							placeholder={t`New password`}
							value={changeNew}
							onChange={(e) => setChangeNew(e.target.value)}
						/>
						<input
							type="password"
							className={styles.input}
							placeholder={t`Confirm new password`}
							value={changeNewConfirm}
							onChange={(e) => setChangeNewConfirm(e.target.value)}
						/>
						<Button
							onClick={handleChangeMasterPassword}
							submitting={submitting}
							disabled={!changeCurrent.trim() || !changeNew.trim() || !changeNewConfirm.trim()}
						>
							{t`Change password`}
						</Button>
					</div>
					<div className={styles.block}>
						<h3 className={styles.heading}>{t`Lock session`}</h3>
						<p className={styles.hint}>{t`Lock temp chats for this session. You'll need to enter your password next time you open a temp chat.`}</p>
						<Button variant="secondary" onClick={handleLockSession}>
							{t`Lock now`}
						</Button>
					</div>
					<div className={styles.block}>
						<h3 className={styles.heading}>{t`Remove master password`}</h3>
						<p className={styles.hint}>{t`Your temp chat key will stay in this session only. New sessions will have no password.`}</p>
						<Button variant="danger-secondary" onClick={handleRemoveMasterPassword}>
							{t`Remove master password`}
						</Button>
					</div>
				</>
			)}
		</div>
	);
});
