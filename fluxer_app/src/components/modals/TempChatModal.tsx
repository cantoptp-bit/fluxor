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

import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import {modal} from '@app/actions/ModalActionCreators';
import * as ToastActionCreators from '@app/actions/ToastActionCreators';
import {ConfirmModal} from '@app/components/modals/ConfirmModal';
import * as Modal from '@app/components/modals/Modal';
import {TempChatUnlockModal} from '@app/components/modals/TempChatUnlockModal';
import {Button} from '@app/components/uikit/button/Button';
import {Scroller, type ScrollerHandle} from '@app/components/uikit/Scroller';
import {decryptFromSenderToString, encryptForRecipient} from '@app/lib/E2EEncryption';
import * as TempChatApi from '@app/lib/TempChatApi';
import {getOrCreateKeyPair, getRecipientPublicKey} from '@app/stores/TempChatKeyStore';
import type {UserRecord} from '@app/records/UserRecord';
import AuthenticationStore from '@app/stores/AuthenticationStore';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useRef, useState} from 'react';
import styles from '@app/components/modals/TempChatModal.module.css';

interface DecryptedMessage {
	id: string;
	senderId: string;
	text: string;
	createdAt: string;
}

interface TempChatModalProps {
	tempChatId: string;
	otherUser: UserRecord;
	onClose: () => void;
}

export const TempChatModal = observer(({tempChatId, otherUser, onClose}: TempChatModalProps) => {
	const {t} = useLingui();
	const [messages, setMessages] = useState<Array<DecryptedMessage>>([]);
	const [loading, setLoading] = useState(true);
	const [sending, setSending] = useState(false);
	const [inputValue, setInputValue] = useState('');
	const [decryptErrors, setDecryptErrors] = useState(0);
	const [needUnlock, setNeedUnlock] = useState(false);
	const unlockModalPushedRef = useRef(false);
	const scrollerRef = useRef<ScrollerHandle>(null);
	const currentUserId = AuthenticationStore.currentUserId ?? '';

	const loadMessages = useCallback(async () => {
		if (!currentUserId) return;
		try {
			const keyPair = await getOrCreateKeyPair(currentUserId, tempChatId);
			if (!keyPair) {
				setNeedUnlock(true);
				setLoading(false);
				return;
			}
			const raw = await TempChatApi.getTempChatMessages(tempChatId);
			const decrypted: Array<DecryptedMessage> = [];
			for (const m of raw) {
				try {
					const text = await decryptFromSenderToString(
						{
							ciphertext: m.ciphertext,
							iv: m.iv,
							ephemeralPublicKey: m.ephemeral_public_key,
						},
						keyPair.privateKey,
					);
					decrypted.push({
						id: m.id,
						senderId: m.sender_id,
						text,
						createdAt: m.created_at,
					});
				} catch {
					setDecryptErrors((n) => n + 1);
				}
			}
			setMessages(decrypted);
		} catch {
			ToastActionCreators.createToast({type: 'error', children: t`Failed to load messages`});
		} finally {
			setLoading(false);
		}
	}, [currentUserId, tempChatId, t]);

	useEffect(() => {
		loadMessages();
		const interval = setInterval(loadMessages, 5000);
		return () => clearInterval(interval);
	}, [loadMessages]);

	// When locked, show unlock modal once
	useEffect(() => {
		if (!needUnlock || !currentUserId || unlockModalPushedRef.current) return;
		unlockModalPushedRef.current = true;
		ModalActionCreators.push(
			modal(() => (
				<TempChatUnlockModal
					userId={currentUserId}
					tempChatId={tempChatId}
					onUnlocked={() => {
						ModalActionCreators.pop();
						unlockModalPushedRef.current = false;
						setNeedUnlock(false);
						loadMessages();
					}}
					onCancel={() => {
						ModalActionCreators.pop();
						unlockModalPushedRef.current = false;
						setNeedUnlock(false);
						onClose();
					}}
				/>
			)),
		);
	}, [needUnlock, currentUserId, tempChatId, onClose, loadMessages]);

	const handleSend = useCallback(async () => {
		const text = inputValue.trim();
		if (!text || !currentUserId || sending) return;
		const recipientKey = await getRecipientPublicKey(otherUser.id);
		if (!recipientKey) {
			ToastActionCreators.createToast({type: 'error', children: t`Cannot send: recipient key not available`});
			return;
		}
		setSending(true);
		try {
			const payload = await encryptForRecipient(text, recipientKey);
			await TempChatApi.sendTempChatMessage(tempChatId, {
				ciphertext: payload.ciphertext,
				iv: payload.iv,
				ephemeral_public_key: payload.ephemeralPublicKey,
			});
			setInputValue('');
			setMessages((prev) => [
				...prev,
				{
					id: `opt-${Date.now()}`,
					senderId: currentUserId,
					text,
					createdAt: new Date().toISOString(),
				},
			]);
			scrollerRef.current?.scrollToBottom();
		} catch {
			ToastActionCreators.createToast({type: 'error', children: t`Failed to send message`});
		} finally {
			setSending(false);
		}
	}, [inputValue, currentUserId, otherUser.id, sending, tempChatId, t]);

	const handleDeleteChat = useCallback(() => {
		ModalActionCreators.push(
			modal(() => (
				<ConfirmModal
					title={t`Delete temp chat`}
					description={t`Delete this temp chat and all messages? This cannot be undone.`}
					primaryText={t`Delete`}
					primaryVariant="danger-primary"
					onPrimary={async () => {
						try {
							await TempChatApi.deleteTempChat(tempChatId);
							ModalActionCreators.pop();
							onClose();
							ToastActionCreators.createToast({type: 'success', children: t`Temp chat deleted`});
						} catch {
							ToastActionCreators.createToast({type: 'error', children: t`Failed to delete chat`});
						}
					}}
				/>
			)),
		);
	}, [tempChatId, onClose, t]);

	return (
		<Modal.Root size="medium" onClose={onClose}>
			<Modal.Header title={otherUser.username}>
				<div className={styles.headerActions}>
					<p className={styles.subtitle}>{t`Temporary encrypted chat`}</p>
					<Button variant="secondary" size="small" onClick={handleDeleteChat} className={styles.deleteButton}>
						{t`Delete chat`}
					</Button>
				</div>
			</Modal.Header>
			<div className={styles.content}>
				{loading ? (
					<div className={styles.loading}>{t`Loading...`}</div>
				) : (
					<div className={styles.inner}>
						<Scroller ref={scrollerRef} className={styles.scroller}>
							<div className={styles.messageList}>
								{messages.map((m) => (
									<div
										key={m.id}
										className={m.senderId === currentUserId ? styles.bubbleOwn : styles.bubbleOther}
									>
										{m.text}
									</div>
								))}
								{decryptErrors > 0 && (
									<div className={styles.decryptError}>
										{t`Some messages could not be decrypted`}
									</div>
								)}
							</div>
						</Scroller>
						<div className={styles.inputRow}>
							<input
								type="text"
								className={styles.input}
								placeholder={t`Message`}
								value={inputValue}
								onChange={(e) => setInputValue(e.target.value)}
								onKeyDown={(e) => {
									if (e.key === 'Enter' && !e.shiftKey) {
										e.preventDefault();
										handleSend();
									}
								}}
								disabled={sending}
							/>
							<Button onClick={handleSend} disabled={sending || !inputValue.trim()} submitting={sending}>
								{t`Send`}
							</Button>
						</div>
					</div>
				)}
			</div>
		</Modal.Root>
	);
});
