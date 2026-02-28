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
import { Input } from '@app/components/form/Input';
import styles from '@app/components/modals/AddFavoriteChannelModal.module.css';
import * as Modal from '@app/components/modals/Modal';
import selectorStyles from '@app/components/modals/shared/SelectorModalStyles.module.css';
import { Button } from '@app/components/uikit/button/Button';
import { Checkbox } from '@app/components/uikit/checkbox/Checkbox';
import { Scroller } from '@app/components/uikit/Scroller';
import { Tooltip } from '@app/components/uikit/tooltip/Tooltip';
import type { ChannelRecord } from '@app/records/ChannelRecord';
import ChannelStore from '@app/stores/ChannelStore';
import FavoritesStore from '@app/stores/FavoritesStore';
import GuildStore from '@app/stores/GuildStore';
import UserGuildSettingsStore from '@app/stores/UserGuildSettingsStore';
import * as ChannelUtils from '@app/utils/ChannelUtils';
import { ME } from '@fluxer/constants/src/AppConstants';
import { ChannelTypes } from '@fluxer/constants/src/ChannelConstants';
import { useLingui } from '@lingui/react/macro';
import { MagnifyingGlassIcon, StarIcon, UsersThreeIcon } from '@phosphor-icons/react';
import clsx from 'clsx';
import { observer } from 'mobx-react-lite';
import React, { useEffect, useMemo, useState } from 'react';

interface ChannelWithCategory {
	channel: ChannelRecord;
	categoryName: string | null;
}

export const AddFavoriteChannelModal = observer(({ categoryId }: { categoryId?: string | null } = {}) => {
	const { t } = useLingui();
	const guilds = GuildStore.getGuilds();
	const firstGuildId = guilds.length > 0 ? guilds[0].id : null;

	const [selectedGuildId, setSelectedGuildId] = useState<string | null>(firstGuildId);
	const [hideMutedChannels, setHideMutedChannels] = useState(false);
	const [searchQuery, setSearchQuery] = useState('');

	useEffect(() => {
		if (!selectedGuildId && guilds.length > 0) {
			setSelectedGuildId(guilds[0].id);
		}
	}, [selectedGuildId, guilds]);

	const selectedGuild = useMemo(
		() => guilds.find((guild) => guild.id === selectedGuildId) || null,
		[guilds, selectedGuildId],
	);

	const channels = useMemo(() => {
		if (!selectedGuild) return [];

		const guildChannels = ChannelStore.getGuildChannels(selectedGuild.id);
		const result: Array<ChannelWithCategory> = [];
		const query = searchQuery.toLowerCase().trim();

		for (const channel of guildChannels) {
			if (
				channel.type !== ChannelTypes.GUILD_TEXT &&
				channel.type !== ChannelTypes.GUILD_VOICE &&
				channel.type !== ChannelTypes.GUILD_LINK
			) {
				continue;
			}

			if (hideMutedChannels && UserGuildSettingsStore.isGuildOrChannelMuted(selectedGuild.id, channel.id)) {
				continue;
			}

			if (query && !channel.name?.toLowerCase().includes(query)) {
				continue;
			}

			let categoryName: string | null = null;
			if (channel.parentId) {
				const category = ChannelStore.getChannel(channel.parentId);
				if (category) {
					categoryName = category.name ?? null;
				}
			}

			result.push({ channel, categoryName });
		}

		return result.sort((a, b) => {
			if (a.categoryName === b.categoryName) {
				return (a.channel.position ?? 0) - (b.channel.position ?? 0);
			}
			if (!a.categoryName) return -1;
			if (!b.categoryName) return 1;
			return a.categoryName.localeCompare(b.categoryName);
		});
	}, [selectedGuild, hideMutedChannels, searchQuery]);

	const handleToggleChannel = (channelId: string) => {
		const isAlreadyFavorite = !!FavoritesStore.getChannel(channelId);
		if (isAlreadyFavorite) {
			FavoritesStore.removeChannel(channelId);
		} else {
			const guildId = selectedGuildId || ME;
			FavoritesStore.addChannel(channelId, guildId, categoryId ?? null);
		}
	};

	const hasGuilds = guilds.length > 0;

	return (
		<Modal.Root size="medium" centered>
			<Modal.Header title={t`Add Favorites`}>
				{hasGuilds && (
					<div className={selectorStyles.headerSearch}>
						<Input
							value={searchQuery}
							onChange={(e: React.ChangeEvent<HTMLInputElement>) => setSearchQuery(e.target.value)}
							placeholder={t`Search channels`}
							leftIcon={<MagnifyingGlassIcon weight="bold" className={selectorStyles.searchIcon} />}
							className={selectorStyles.headerSearchInput}
						/>
					</div>
				)}
			</Modal.Header>
			<div className={styles.modalContent}>
				{!hasGuilds ? (
					<div className={styles.emptyStateFull}>
						<UsersThreeIcon weight="duotone" className={styles.emptyStateIcon} aria-hidden />
						<p className={styles.emptyStateTitle}>{t`No communities yet`}</p>
						<p className={styles.emptyStateDescription}>{t`Join a community to add channels to Favorites.`}</p>
					</div>
				) : (
					<>
						<div className={styles.sidebar}>
							<div className={styles.sidebarLabel} aria-hidden>
								{t`Communities`}
							</div>
							<Scroller className={styles.scrollerContainer}>
								<div className={styles.sidebarIcons}>
									{guilds.map((guild) => (
										<Tooltip key={guild.id} text={guild.name} position="right">
											<button
												type="button"
												className={clsx(styles.sidebarIcon, {
													[styles.sidebarIconSelected]: selectedGuildId === guild.id,
												})}
												onClick={() => setSelectedGuildId(guild.id)}
												style={{ border: 'none' }}
											>
												{guild.icon ? (
													<img src={guild.icon} alt={guild.name} className={styles.communityIcon} />
												) : (
													<span>{guild.name.charAt(0)}</span>
												)}
											</button>
										</Tooltip>
									))}
								</div>
							</Scroller>
						</div>

						<div className={styles.mainContent}>
							{selectedGuild ? (
								<>
									<div className={styles.pinCommunityRow}>
										<div className={styles.channelIconContainer}>
											<StarIcon weight="fill" className={styles.channelIcon} />
										</div>
										<span className={styles.pinCommunityTitle}>{t`Pin this Community`}</span>
										<Button variant="secondary" small disabled>
											{t`Soon`}
										</Button>
									</div>

									<Checkbox
										className={styles.checkboxRow}
										checked={hideMutedChannels}
										onChange={(checked) => setHideMutedChannels(checked)}
									>
										<span className={styles.checkboxText}>{t`Hide muted channels`}</span>
									</Checkbox>

									<Scroller className={styles.scrollerContainer} key="add-favorite-channel-scroller">
										<div className={styles.channelList}>
											{channels.length === 0 ? (
												<div className={styles.emptyState}>{t`No channels available`}</div>
											) : (
												channels.map(({ channel, categoryName }, index) => {
													const prevCategoryName = index > 0 ? channels[index - 1].categoryName : null;
													const showCategoryHeader = categoryName !== prevCategoryName;
													const isAlreadyFavorite = !!FavoritesStore.getChannel(channel.id);

													return (
														<React.Fragment key={channel.id}>
															{showCategoryHeader && (
																<div className={styles.categoryHeader}>{categoryName || t`Uncategorized`}</div>
															)}
															<div className={styles.channelRow}>
																<div className={styles.channelIconContainer}>
																	{ChannelUtils.getIcon(channel, {
																		className: styles.channelIcon,
																	})}
																</div>
																<span className={styles.channelName}>{channel.name}</span>
																<div className={styles.channelActions}>
																	<Button
																		variant={isAlreadyFavorite ? 'secondary' : 'primary'}
																		small={true}
																		onClick={() => handleToggleChannel(channel.id)}
																	>
																		{isAlreadyFavorite ? t`Remove` : t`Add`}
																	</Button>
																</div>
															</div>
														</React.Fragment>
													);
												})
											)}
										</div>
									</Scroller>
								</>
							) : (
								<div className={styles.emptyStateCentered}>
									<UsersThreeIcon weight="duotone" className={styles.emptyStateIcon} aria-hidden />
									<p className={styles.emptyStateMessage}>{t`Select a community from the sidebar`}</p>
									<p className={styles.emptyStateHint}>{t`Choose a server on the left to see its channels.`}</p>
								</div>
							)}
						</div>
					</>
				)}
			</div>
			<Modal.Footer>
				<Button onClick={ModalActionCreators.pop}>{t`Done`}</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
