/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License or (at your option) any later version.
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
import {TempChatSetChatPasswordModal} from '@app/components/modals/TempChatSetChatPasswordModal';
import {TempChatUnlockModal} from '@app/components/modals/TempChatUnlockModal';
import {Button} from '@app/components/uikit/button/Button';
import {Scroller, type ScrollerHandle} from '@app/components/uikit/Scroller';
import {StatusAwareAvatar} from '@app/components/uikit/StatusAwareAvatar';
import {decryptFromSenderToString, encryptForRecipient} from '@app/lib/E2EEncryption';
import * as TempChatApi from '@app/lib/TempChatApi';
import {Routes} from '@app/Routes';
import * as RouterUtils from '@app/utils/RouterUtils';
import {getOrCreateKeyPair, getRecipientPublicKey} from '@app/stores/TempChatKeyStore';
import * as TempChatLockStore from '@app/stores/TempChatLockStore';
import type {UserRecord} from '@app/records/UserRecord';
import AuthenticationStore from '@app/stores/AuthenticationStore';
import UserStore from '@app/stores/UserStore';
import * as DateUtils from '@app/utils/DateUtils';
import {
	processTempChatMessages,
	setSentPlaintext,
	type DecryptedMessage,
} from '@app/utils/TempChatSentCache';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useRef, useState} from 'react';
import styles from '@app/components/pages/TempChatFullPage.module.css';

function parseOtherUserId(tempChatId: string, currentUserId: string): string | null {
	const parts = tempChatId.split('_');
	if (parts.length !== 2) return null;
	const [a, b] = parts;
	if (a === currentUserId) return b;
	if (b === currentUserId) return a;
	return null;
}

interface TempChatFullPageProps {
	tempChatId: string;
}

export const TempChatFullPage = observer(({tempChatId}: TempChatFullPageProps) => {
	const {t, i18n} = useLingui();
	const currentUserId = AuthenticationStore.currentUserId ?? '';
	const currentUser = UserStore.getUser(currentUserId) ?? null;
	const [otherUser, setOtherUser] = useState<UserRecord | null>(null);
	const [loadingUser, setLoadingUser] = useState(true);
	const [messages, setMessages] = useState<Array<DecryptedMessage>>([]);
	const [loading, setLoading] = useState(true);
	const [sending, setSending] = useState(false);
	const [inputValue, setInputValue] = useState('');
	const [decryptErrors, setDecryptErrors] = useState(0);
	const [needUnlock, setNeedUnlock] = useState(false);
	const unlockModalPushedRef = useRef(false);
	const scrollerRef = useRef<ScrollerHandle>(null);

	// Resolve other participant from temp chat list or parse from id
	useEffect(() => {
		let cancelled = false;
		(async () => {
			const otherId = parseOtherUserId(tempChatId, currentUserId);
			if (otherId) {
				const user = UserStore.getUser(otherId);
				if (user && !cancelled) {
					setOtherUser(user);
				} else {
					const list = await TempChatApi.listTempChats();
					if (cancelled) return;
					const chat = list.find((c) => c.id === tempChatId);
					if (chat) {
						const id = chat.participant_ids[0] === currentUserId ? chat.participant_ids[1] : chat.participant_ids[0];
						setOtherUser(UserStore.getUser(id) ?? null);
					}
				}
			}
			setLoadingUser(false);
		})();
		return () => {
			cancelled = true;
		};
	}, [tempChatId, currentUserId]);

	const loadMessages = useCallback(async () => {
		if (!currentUserId) return;
		setDecryptErrors(0);
		try {
			const keyPair = await getOrCreateKeyPair(currentUserId, tempChatId);
			if (!keyPair) {
				setNeedUnlock(true);
				setLoading(false);
				return;
			}
			const raw = await TempChatApi.getTempChatMessages(tempChatId);
			const { messages: decrypted, decryptErrorCount: errors } = await processTempChatMessages(
				raw,
				currentUserId,
				tempChatId,
				t`[Your message]`,
				(payload) => decryptFromSenderToString(payload, keyPair.privateKey),
			);
			setDecryptErrors(errors);
			setMessages(decrypted);
		} catch {
			ToastActionCreators.createToast({type: 'error', children: t`Failed to load messages`});
		} finally {
			setLoading(false);
		}
	}, [currentUserId, tempChatId, t]);

	useEffect(() => {
		if (!otherUser) return;
		loadMessages();
		const interval = setInterval(loadMessages, 5000);
		return () => clearInterval(interval);
	}, [otherUser, loadMessages]);

	// When locked, show unlock modal once
	useEffect(() => {
		if (!needUnlock || !currentUserId || !otherUser || unlockModalPushedRef.current) return;
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
						RouterUtils.transitionTo(Routes.ME);
					}}
				/>
			)),
		);
	}, [needUnlock, currentUserId, tempChatId, otherUser, loadMessages]);

	const handleSend = useCallback(async () => {
		const text = inputValue.trim();
		if (!text || !currentUserId || sending || !otherUser) return;
		const recipientKey = await getRecipientPublicKey(otherUser.id);
		if (!recipientKey) {
			ToastActionCreators.createToast({type: 'error', children: t`Cannot send: recipient key not available`});
			return;
		}
		setSending(true);
		try {
			const payload = await encryptForRecipient(text, recipientKey);
			const res = await TempChatApi.sendTempChatMessage(tempChatId, {
				ciphertext: payload.ciphertext,
				iv: payload.iv,
				ephemeral_public_key: payload.ephemeralPublicKey,
			});
			setSentPlaintext(tempChatId, res.id, text);
			setInputValue('');
			setMessages((prev) => [
				...prev,
				{
					id: res.id,
					senderId: currentUserId,
					text,
					createdAt: res.created_at,
				},
			]);
			scrollerRef.current?.scrollToBottom();
		} catch {
			ToastActionCreators.createToast({type: 'error', children: t`Failed to send message`});
		} finally {
			setSending(false);
		}
	}, [inputValue, currentUserId, otherUser, sending, tempChatId, t]);

	const handleDeleteChat = useCallback(() => {
		ModalActionCreators.push(
			modal(() => (
				<ConfirmModal
					title={t`Request delete temp chat`}
					description={t`Both you and the other person must agree to delete. Your request will be sent; the chat will be deleted only when they agree too.`}
					primaryText={t`Request delete`}
					primaryVariant="danger-primary"
					onPrimary={async () => {
						try {
							const {deleted} = await TempChatApi.requestDeleteTempChat(tempChatId);
							ModalActionCreators.pop();
							if (deleted) {
								ToastActionCreators.createToast({type: 'success', children: t`Temp chat deleted`});
								RouterUtils.transitionTo(Routes.ME);
							} else {
								ToastActionCreators.createToast({
									type: 'success',
									children: t`Delete requested. The chat will be deleted when the other person agrees.`,
								});
							}
						} catch {
							ToastActionCreators.createToast({type: 'error', children: t`Failed to request delete`});
						}
					}}
				/>
			)),
		);
	}, [tempChatId, t]);

	const goBack = useCallback(() => {
		RouterUtils.transitionTo(Routes.ME);
	}, []);

	const hasChatPassword = TempChatLockStore.hasChatPassword(tempChatId);

	const handleSetChatPassword = useCallback(() => {
		ModalActionCreators.push(
			modal(() => (
				<TempChatSetChatPasswordModal
					userId={currentUserId}
					tempChatId={tempChatId}
					onDone={() => {
						ModalActionCreators.pop();
						ToastActionCreators.createToast({type: 'success', children: t`Password set for this chat`});
					}}
					onCancel={() => ModalActionCreators.pop()}
				/>
			)),
		);
	}, [currentUserId, tempChatId, t]);

	const handleRemoveChatPassword = useCallback(() => {
		ModalActionCreators.push(
			modal(() => (
				<ConfirmModal
					title={t`Remove password for this chat?`}
					description={t`You can still unlock with your master password.`}
					primaryText={t`Remove`}
					primaryVariant="danger-primary"
					onPrimary={() => {
						TempChatLockStore.clearChatPassword(tempChatId);
						ModalActionCreators.pop();
						ToastActionCreators.createToast({type: 'success', children: t`Chat password removed`});
					}}
					onSecondary={() => ModalActionCreators.pop()}
				/>
			)),
		);
	}, [tempChatId, t]);

	if (loadingUser) {
		return (
			<div className={styles.container}>
				<div className={styles.loading}>{t`Loading...`}</div>
			</div>
		);
	}
	if (!otherUser) {
		return (
			<div className={styles.container}>
				<div className={styles.loading}>{t`Temp chat not found`}</div>
				<Button variant="secondary" onClick={goBack}>{t`Back to DMs`}</Button>
			</div>
		);
	}

	return (
		<div className={styles.container}>
			<header className={styles.header}>
				<button type="button" className={styles.backButton} onClick={goBack} aria-label={t`Back`}>
					←
				</button>
				<StatusAwareAvatar user={otherUser} size={32} className={styles.headerAvatar} disablePresence />
				<div className={styles.headerTitleBlock}>
					<div className={styles.headerTitle}>{otherUser.username}</div>
					<span className={styles.headerTempLabel}>{t`Temporary encrypted chat`}</span>
				</div>
				<div className={styles.headerActions}>
					{hasChatPassword ? (
						<Button variant="secondary" size="small" onClick={handleRemoveChatPassword}>
							{t`Remove chat password`}
						</Button>
					) : (
						<Button variant="secondary" size="small" onClick={handleSetChatPassword}>
							{t`Set password for this chat`}
						</Button>
					)}
					<Button variant="secondary" size="small" onClick={handleDeleteChat} className={styles.deleteButton}>
						{t`Delete chat`}
					</Button>
				</div>
			</header>
			<div className={styles.tempChatBanner} role="status">
				<span className={styles.tempChatBannerIcon} aria-hidden>🔒</span>
				<span>{t`You're in a temporary chat. Messages are end-to-end encrypted and not stored permanently.`}</span>
			</div>
			<div className={styles.content}>
				{loading ? (
					<div className={styles.loading}>{t`Loading...`}</div>
				) : (
					<div className={styles.inner}>
						<Scroller ref={scrollerRef} className={styles.scroller}>
							<div className={styles.messageList}>
								{messages.map((m) => {
									const isOwn = m.senderId === currentUserId;
									const senderUser = isOwn ? currentUser : otherUser;
									const timestamp = new Date(m.createdAt);
									const timeLabel = DateUtils.getRelativeDateString(timestamp, i18n);
									return (
										<div
											key={m.id}
											className={isOwn ? styles.messageRowOwn : styles.messageRowOther}
										>
											{!isOwn && (
												<div className={styles.messageAvatar}>
													<StatusAwareAvatar user={senderUser} size={32} disablePresence />
												</div>
											)}
											<div className={styles.messageContent}>
												<div className={styles.messageMeta}>
													<span className={styles.messageSender}>
														{senderUser?.username ?? (isOwn ? t`You` : t`Unknown`)}
													</span>
													<time className={styles.messageTimestamp} dateTime={timestamp.toISOString()} title={DateUtils.getFormattedDateTimeWithSeconds(timestamp)}>
														{timeLabel}
													</time>
												</div>
												<div className={isOwn ? styles.bubbleOwn : styles.bubbleOther}>
													{m.text}
												</div>
											</div>
											{isOwn && (
												<div className={styles.messageAvatar}>
													<StatusAwareAvatar user={senderUser} size={32} disablePresence />
												</div>
											)}
										</div>
									);
								})}
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
		</div>
	);
});
