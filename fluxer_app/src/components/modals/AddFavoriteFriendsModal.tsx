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
import * as PrivateChannelActionCreators from '@app/actions/PrivateChannelActionCreators';
import * as ToastActionCreators from '@app/actions/ToastActionCreators';
import {FriendSelector} from '@app/components/common/FriendSelector';
import {Input} from '@app/components/form/Input';
import * as Modal from '@app/components/modals/Modal';
import selectorStyles from '@app/components/modals/shared/SelectorModalStyles.module.css';
import {Button} from '@app/components/uikit/button/Button';
import ChannelStore from '@app/stores/ChannelStore';
import FavoritesStore from '@app/stores/FavoritesStore';
import {ME} from '@fluxer/constants/src/AppConstants';
import {Trans, useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo, useState} from 'react';

export const AddFavoriteFriendsModal = observer(() => {
	const {t} = useLingui();
	const [selectedUserIds, setSelectedUserIds] = useState<Array<string>>([]);
	const [searchQuery, setSearchQuery] = useState('');
	const [isSubmitting, setIsSubmitting] = useState(false);

	const excludeUserIds = useMemo(() => {
		const ids: Array<string> = [];
		for (const fav of FavoritesStore.sortedChannels) {
			const channel = ChannelStore.getChannel(fav.channelId);
			if (channel?.isDM() && channel.recipientIds[0]) {
				ids.push(channel.recipientIds[0]);
			}
		}
		return ids;
	}, []);

	const handleToggle = useCallback((userId: string) => {
		setSelectedUserIds((prev) =>
			prev.includes(userId) ? prev.filter((id) => id !== userId) : [...prev, userId],
		);
	}, []);

	const handleAddToFavorites = useCallback(async () => {
		if (selectedUserIds.length === 0) return;
		setIsSubmitting(true);
		try {
			let added = 0;
			for (const userId of selectedUserIds) {
				const channelId = await PrivateChannelActionCreators.ensureDMChannel(userId);
				FavoritesStore.addChannel(channelId, ME, null);
				added += 1;
			}
			ModalActionCreators.pop();
			if (added > 0) {
				ToastActionCreators.createToast({
					type: 'success',
					children: added === 1 ? t`Friend added to favorites` : t`Friends added to favorites`,
				});
			}
		} finally {
			setIsSubmitting(false);
		}
	}, [selectedUserIds, t]);

	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={t`Favorite Friends`}>
				<p className={selectorStyles.subtitle}>
					<Trans>Select friends to pin their DMs to your favorites for quick access.</Trans>
				</p>
				<div className={selectorStyles.headerSearch}>
					<Input
						value={searchQuery}
						onChange={(e) => setSearchQuery(e.target.value)}
						placeholder={t`Search friends`}
						leftIcon={<MagnifyingGlassIcon weight="bold" className={selectorStyles.searchIcon} />}
						className={selectorStyles.headerSearchInput}
					/>
				</div>
			</Modal.Header>
			<Modal.Content className={selectorStyles.selectorContent}>
				<FriendSelector
					selectedUserIds={selectedUserIds}
					onToggle={handleToggle}
					excludeUserIds={excludeUserIds}
					searchQuery={searchQuery}
					onSearchQueryChange={setSearchQuery}
					showSearchInput={false}
				/>
			</Modal.Content>
			<Modal.Footer className={selectorStyles.footer}>
				<div className={selectorStyles.actionRow}>
					<Button variant="secondary" onClick={() => ModalActionCreators.pop()} className={selectorStyles.actionButton}>
						<Trans>Cancel</Trans>
					</Button>
					<Button
						onClick={handleAddToFavorites}
						disabled={selectedUserIds.length === 0 || isSubmitting}
						submitting={isSubmitting}
						className={selectorStyles.actionButton}
					>
						<Trans>Add to Favorites</Trans>
					</Button>
				</div>
			</Modal.Footer>
		</Modal.Root>
	);
});
