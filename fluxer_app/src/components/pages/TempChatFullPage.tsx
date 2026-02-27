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
import {Divider} from '@app/components/channel/Divider';
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
import {MESSAGE_GROUP_TIMEOUT} from '@app/utils/MessageGroupingUtils';
import {useLingui} from '@lingui/react/macro';
import {PaperPlaneTiltIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';
import styles from '@app/components/pages/TempChatFullPage.module.css';

/** Legacy id is "lo_hi"; V2 id is numeric (chat_id). Returns other user id from legacy id, or null (use list to resolve). */
function parseOtherUserIdFromLegacyId(tempChatId: string, currentUserId: string): string | null {
	const parts = tempChatId.split('_');
	if (parts.length !== 2) return null;
	const [a, b] = parts;
	if (a === currentUserId) return b;
	if (b === currentUserId) return a;
	return null;
}

type ListItem = { type: 'divider'; date: string } | { type: 'message'; message: DecryptedMessage; grouped: boolean };

function buildMessageListWithDividers(messages: Array<DecryptedMessage>): Array<ListItem> {
	const list: Array<ListItem> = [];
	let lastDate = '';
	for (let i = 0; i < messages.length; i++) {
		const m = messages[i];
		const createdAt = typeof m.createdAt === 'string' ? new Date(m.createdAt) : m.createdAt;
		const dateString = DateUtils.getFormattedFullDate(createdAt);
		if (dateString !== lastDate) {
			list.push({ type: 'divider', date: dateString });
			lastDate = dateString;
		}
		const prev = messages[i - 1];
		const prevTime = prev ? (typeof prev.createdAt === 'string' ? new Date(prev.createdAt).getTime() : prev.createdAt.getTime()) : 0;
		const currTime = createdAt.getTime();
		const grouped = !!prev && prev.senderId === m.senderId && currTime - prevTime <= MESSAGE_GROUP_TIMEOUT;
		list.push({ type: 'message', message: m, grouped });
	}
	return list;
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
	const [recipientKeyAvailable, setRecipientKeyAvailable] = useState<boolean | null>(null);
	const [provisioningForTesting, setProvisioningForTesting] = useState(false);
	const unlockModalPushedRef = useRef(false);
	const scrollerRef = useRef<ScrollerHandle>(null);
	const textareaRef = useRef<HTMLTextAreaElement>(null);
	const inputValueRef = useRef('');

	// Resolve other participant: from legacy id "lo_hi", or from list for V2 (numeric) ids
	useEffect(() => {
		let cancelled = false;
		(async () => {
			const otherIdFromLegacy = parseOtherUserIdFromLegacyId(tempChatId, currentUserId);
			if (otherIdFromLegacy) {
				const user = UserStore.getUser(otherIdFromLegacy);
				if (user && !cancelled) {
					setOtherUser(user);
				} else {
					const list = await TempChatApi.listTempChats();
					if (cancelled) return;
					const chat = list.find((c) => c.id === tempChatId);
					if (chat) {
						const id = chat.participant_ids[0] === currentUserId ? chat.participant_ids[1] : chat.participant_ids[0];
						if (!cancelled) setOtherUser(UserStore.getUser(id) ?? null);
					}
				}
			} else {
				// V2 id (numeric) or unknown: resolve from list
				const list = await TempChatApi.listTempChats();
				if (cancelled) return;
				const chat = list.find((c) => c.id === tempChatId);
				if (chat) {
					const id = chat.participant_ids[0] === currentUserId ? chat.participant_ids[1] : chat.participant_ids[0];
					if (!cancelled) setOtherUser(UserStore.getUser(id) ?? null);
				}
			}
			if (!cancelled) setLoadingUser(false);
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
			// Merge with optimistic messages so sent messages stay visible until server returns them
			setMessages((prev) => {
				const serverIds = new Set(decrypted.map((m) => m.id));
				const optimisticOnly = prev.filter(
					(m) => m.senderId === currentUserId && !serverIds.has(m.id),
				);
				if (optimisticOnly.length === 0) return decrypted;
				const merged = [...decrypted, ...optimisticOnly];
				merged.sort((a, b) => {
					const tA = typeof a.createdAt === 'string' ? new Date(a.createdAt).getTime() : a.createdAt.getTime();
					const tB = typeof b.createdAt === 'string' ? new Date(b.createdAt).getTime() : b.createdAt.getTime();
					return tA - tB;
				});
				return merged;
			});
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

	// Check if recipient has E2E key (so we know whether to show "Enable messaging for testing")
	useEffect(() => {
		if (!otherUser) {
			setRecipientKeyAvailable(null);
			return;
		}
		let cancelled = false;
		TempChatApi.getOtherUserE2EKey(otherUser.id).then((key) => {
			if (!cancelled) setRecipientKeyAvailable(key != null);
		});
		return () => {
			cancelled = true;
		};
	}, [otherUser]);

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
		if (!currentUserId || !otherUser) return;
		if (sending) return;

		// Read from all sources so we never miss the latest text (ref is synced in onChange; DOM/state as fallbacks)
		const fromRef = inputValueRef.current ?? '';
		const fromDom = textareaRef.current?.value ?? '';
		const fromState = inputValue;
		const rawText = fromRef || fromDom || fromState;
		const text = rawText.trim();
		if (!text) return;

		// Clear input immediately so UI feels responsive; we already have `text` captured
		setInputValue('');
		inputValueRef.current = '';
		setSending(true);

		try {
			const recipientKey = await getRecipientPublicKey(otherUser.id);
			if (!recipientKey) {
				const name = otherUser.username ?? otherUser.globalName ?? otherUser.id;
				ToastActionCreators.createToast({
					type: 'error',
					children: t`Ask ${name} to open this chat once to enable messaging`,
				});
				setInputValue(text);
				inputValueRef.current = text;
				setSending(false);
				return;
			}
			const payload = await encryptForRecipient(text, recipientKey);
			const res = await TempChatApi.sendTempChatMessage(tempChatId, {
				ciphertext: payload.ciphertext,
				iv: payload.iv,
				ephemeral_public_key: payload.ephemeralPublicKey,
			});
			setSentPlaintext(tempChatId, res.id, text);
			setMessages((prev) => [
				...prev,
				{
					id: res.id,
					senderId: currentUserId,
					text,
					createdAt: res.created_at,
				},
			]);
			// Defer scroll so it runs after React commits; otherwise scroll uses old content height and the new message stays below viewport
			requestAnimationFrame(() => {
				scrollerRef.current?.scrollToBottom();
			});
		} catch (err) {
			ToastActionCreators.createToast({
				type: 'error',
				children: err instanceof Error ? err.message : t`Failed to send message`,
			});
			setInputValue(text);
			inputValueRef.current = text;
		} finally {
			setSending(false);
		}
	}, [currentUserId, otherUser, sending, tempChatId, inputValue, t]);

	const handleProvisionRecipientForTesting = useCallback(async () => {
		if (!otherUser) return;
		setProvisioningForTesting(true);
		try {
			await TempChatApi.provisionRecipientKeyForTesting(tempChatId);
			const key = await TempChatApi.getOtherUserE2EKey(otherUser.id);
			setRecipientKeyAvailable(key != null);
			ToastActionCreators.createToast({type: 'success', children: t`Messaging enabled for testing. You can send now.`});
		} catch (err) {
			ToastActionCreators.createToast({
				type: 'error',
				children: err instanceof Error ? err.message : t`Failed to enable (dev only)`,
			});
		} finally {
			setProvisioningForTesting(false);
		}
	}, [otherUser, tempChatId, t]);

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
	const listItems = useMemo(() => buildMessageListWithDividers(messages), [messages]);

	// Auto-resize textarea (single line by default, grows with Shift+Enter)
	useEffect(() => {
		const el = textareaRef.current;
		if (!el) return;
		el.style.height = 'auto';
		el.style.height = `${Math.min(el.scrollHeight, 200)}px`;
	}, [inputValue]);

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
						<Button variant="secondary" small onClick={handleRemoveChatPassword}>
							{t`Remove chat password`}
						</Button>
					) : (
						<Button variant="secondary" small onClick={handleSetChatPassword}>
							{t`Set password for this chat`}
						</Button>
					)}
					<Button variant="secondary" small onClick={handleDeleteChat} className={styles.deleteButton}>
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
								{listItems.map((item) => {
									if (item.type === 'divider') {
										return (
											<Divider key={item.date} spacing={16} isDate className={styles.dateDivider}>
												{item.date}
											</Divider>
										);
									}
									const { message: m, grouped } = item;
									const isOwn = m.senderId === currentUserId;
									const senderUser = isOwn ? currentUser : otherUser;
									const timestamp = typeof m.createdAt === 'string' ? new Date(m.createdAt) : m.createdAt;
									const timeLabel = DateUtils.getRelativeDateString(timestamp, i18n);
									const displayName = senderUser?.username ?? (isOwn ? t`You` : t`Unknown`);
									return (
										<div
											key={m.id}
											className={grouped ? styles.messageRowGrouped : styles.messageRow}
										>
											<div className={styles.messageGutterLeft} />
											{grouped ? (
												<time
													className={styles.messageTimestampHover}
													dateTime={timestamp.toISOString()}
													title={DateUtils.getFormattedDateTimeWithSeconds(timestamp)}
												>
													{timeLabel}
												</time>
											) : (
												<div className={styles.messageAvatar}>
													<StatusAwareAvatar user={senderUser} size={40} disablePresence />
												</div>
											)}
											<div className={styles.messageGutterRight} />
											<div className={styles.messageContent}>
												{!grouped && (
													<div className={styles.messageAuthorRow}>
														<span className={styles.messageSender}>{displayName}</span>
														<time
															className={styles.messageTimestamp}
															dateTime={timestamp.toISOString()}
															title={DateUtils.getFormattedDateTimeWithSeconds(timestamp)}
														>
															{' — '}
															{timeLabel}
														</time>
													</div>
												)}
												<div className={styles.messageText}>{m.text}</div>
											</div>
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
						{recipientKeyAvailable === false && (
							<div className={styles.testingBypassRow}>
								<Button
									variant="secondary"
									small
									onClick={handleProvisionRecipientForTesting}
									disabled={provisioningForTesting}
								>
									{provisioningForTesting ? t`Enabling...` : t`Enable messaging for testing`}
								</Button>
								<span className={styles.testingBypassHint}>{t`Bypass: other user doesn't need to open the chat.`}</span>
							</div>
						)}
						<form
							className={styles.inputRow}
							onSubmit={(e) => {
								e.preventDefault();
								handleSend();
							}}
						>
							<textarea
								ref={textareaRef}
								className={styles.input}
								placeholder={otherUser?.username ? t`Message @${otherUser.username}` : t`Message`}
								value={inputValue}
								onChange={(e) => {
									const v = e.target.value;
									inputValueRef.current = v;
									setInputValue(v);
								}}
								onKeyDown={(e) => {
									if (e.key === 'Enter' && !e.shiftKey) {
										e.preventDefault();
										handleSend();
									}
								}}
								disabled={sending}
								rows={1}
								aria-label={otherUser?.username ? t`Message @${otherUser.username}` : t`Message`}
							/>
							<button
								type="submit"
								className={styles.sendButton}
								disabled={sending || !inputValue.trim()}
								aria-label={t`Send message`}
								title={t`Send (Enter)`}
							>
								<PaperPlaneTiltIcon weight="fill" className={styles.sendButtonIcon} />
							</button>
						</form>
					</div>
				)}
			</div>
		</div>
	);
});
