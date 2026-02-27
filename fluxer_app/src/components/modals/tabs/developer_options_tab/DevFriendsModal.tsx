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
import { addDevFriends, fetchDevUsers, fetchRelationships } from '@app/actions/RelationshipActionCreators';
import * as ToastActionCreators from '@app/actions/ToastActionCreators';
import * as Modal from '@app/components/modals/Modal';
import { Button } from '@app/components/uikit/button/Button';
import { Checkbox } from '@app/components/uikit/checkbox/Checkbox';
import RelationshipStore from '@app/stores/RelationshipStore';
import UserStore from '@app/stores/UserStore';
import { RelationshipTypes } from '@fluxer/constants/src/UserConstants';
import { Trans, useLingui } from '@lingui/react/macro';
import { observer } from 'mobx-react-lite';
import { useCallback, useEffect, useState } from 'react';
import styles from './DevFriendsModal.module.css';

type DevUser = { id: string; username: string; discriminator: number; global_name: string | null };

export const DevFriendsModal = observer(() => {
	const { t } = useLingui();
	const currentUserId = UserStore.getCurrentUser()?.id ?? null;
	const [users, setUsers] = useState<Array<DevUser>>([]);
	const [nextAfter, setNextAfter] = useState<string | null>(null);
	const [loading, setLoading] = useState(true);
	const [loadingMore, setLoadingMore] = useState(false);
	const [adding, setAdding] = useState(false);
	const [selected, setSelected] = useState<Set<string>>(new Set());
	const [error, setError] = useState<string | null>(null);

	const loadPage = useCallback(async (after?: string) => {
		try {
			if (after == null) setLoading(true);
			else setLoadingMore(true);
			setError(null);
			const { users: nextUsers, next_after } = await fetchDevUsers(200, after);
			if (after == null) {
				setUsers(nextUsers);
			} else {
				setUsers((prev) => [...prev, ...nextUsers]);
			}
			setNextAfter(next_after);
		} catch (err) {
			setError((err as Error).message);
		} finally {
			setLoading(false);
			setLoadingMore(false);
		}
	}, []);

	useEffect(() => {
		void loadPage();
	}, [loadPage]);

	const isFriend = useCallback(
		(userId: string) => {
			return RelationshipStore.getRelationship(userId)?.type === RelationshipTypes.FRIEND;
		},
		[],
	);

	const toggle = useCallback((userId: string) => {
		if (userId === currentUserId) return;
		setSelected((prev) => {
			const next = new Set(prev);
			if (next.has(userId)) next.delete(userId);
			else next.add(userId);
			return next;
		});
	}, [currentUserId]);

	const toggleAll = useCallback(() => {
		const selectable = users.filter((u) => u.id !== currentUserId && !isFriend(u.id)).map((u) => u.id);
		setSelected((prev) => {
			const next = new Set(prev);
			const allSelected = selectable.every((id) => next.has(id));
			if (allSelected) selectable.forEach((id) => next.delete(id));
			else selectable.forEach((id) => next.add(id));
			return next;
		});
	}, [users, currentUserId, isFriend]);

	const handleAddSelected = useCallback(async () => {
		const ids = Array.from(selected);
		if (ids.length === 0) {
			ToastActionCreators.createToast({ type: 'info', children: t`Select at least one user to add.` });
			return;
		}
		setAdding(true);
		try {
			const { added, errors } = await addDevFriends(ids);
			try {
				await fetchRelationships();
			} catch {
				// Friends were added but refetch failed (e.g. network); still show success
				ToastActionCreators.createToast({
					type: 'info',
					children: t`Friends added; open the Friends tab to refresh the list.`,
				});
			}
			setSelected((prev) => {
				const next = new Set(prev);
				ids.forEach((id) => next.delete(id));
				return next;
			});
			if (added > 0) {
				ToastActionCreators.createToast({ type: 'success', children: t`Added ${added} friend(s). Open Friends to see them.` });
			} else if (errors.length > 0) {
				ToastActionCreators.createToast({
					type: 'info',
					children: t`Some failed: ${errors.map((e) => e.error).join(', ')}`,
				});
			} else {
				ToastActionCreators.createToast({
					type: 'info',
					children: t`No new friends added (they may already be on your list).`,
				});
			}
		} catch (err) {
			const message = err instanceof Error ? err.message : String(err);
			ToastActionCreators.createToast({ type: 'error', children: message });
		} finally {
			setAdding(false);
		}
	}, [selected, t]);

	const selectableCount = users.filter((u) => u.id !== currentUserId && !isFriend(u.id)).length;
	const selectedCount = Array.from(selected).length;

	return (
		<Modal.Root size="medium" centered>
			<Modal.Header title={t`Dev: Manage friends`}>
				<p className={styles.subtitle}>
					<Trans>Select users to add as friends. Only available in development.</Trans>
				</p>
			</Modal.Header>
			<Modal.Content className={styles.content}>
				{error != null && <p className={styles.error}>{error}</p>}
				{loading ? (
					<p className={styles.loading}>{t`Loading users…`}</p>
				) : (
					<>
						{users.length > 0 && selectableCount > 0 && (
							<div className={styles.toolbar}>
								<Button variant="secondary" small onClick={toggleAll}>
									{t`Select all`}
								</Button>
							</div>
						)}
						<ul className={styles.userList}>
							{users.map((u) => {
								const isSelf = u.id === currentUserId;
								const friend = isFriend(u.id);
								const disabled = isSelf || friend;
								const tag = `${u.username}#${String(u.discriminator).padStart(4, '0')}`;
								const displayName = u.global_name ?? u.username;
								return (
									<li key={u.id} className={styles.userRow}>
										<Checkbox
											checked={selected.has(u.id)}
											onChange={() => toggle(u.id)}
											disabled={disabled}
											aria-label={t`Add ${displayName} as friend`}
										/>
										<span className={styles.userInfo}>
											{displayName} <span className={styles.tag}>{tag}</span>
											{isSelf && <span className={styles.badge}>{t`You`}</span>}
											{friend && !isSelf && <span className={styles.badge}>{t`Friend`}</span>}
										</span>
									</li>
								);
							})}
						</ul>
						{nextAfter != null && (
							<div className={styles.loadMore}>
								<Button
									variant="secondary"
									small
									onClick={() => void loadPage(nextAfter)}
									submitting={loadingMore}
									disabled={loadingMore}
								>
									{loadingMore ? t`Loading…` : t`Load more`}
								</Button>
							</div>
						)}
					</>
				)}
			</Modal.Content>
			<Modal.Footer className={styles.footer}>
				<Button variant="secondary" onClick={() => ModalActionCreators.pop()}>
					{t`Cancel`}
				</Button>
				<Button
					onClick={handleAddSelected}
					disabled={selectedCount === 0}
					submitting={adding}
				>
					{t`Add ${selectedCount} as friends`}
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
