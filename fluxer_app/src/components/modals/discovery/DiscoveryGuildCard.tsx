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

import type {DiscoveryGuild} from '@app/actions/DiscoveryActionCreators';
import * as DiscoveryActionCreators from '@app/actions/DiscoveryActionCreators';
import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import * as NavigationActionCreators from '@app/actions/NavigationActionCreators';
import * as ToastActionCreators from '@app/actions/ToastActionCreators';
import {GuildBadge} from '@app/components/guild/GuildBadge';
import styles from '@app/components/modals/discovery/DiscoveryGuildCard.module.css';
import {GuildIcon} from '@app/components/popouts/GuildIcon';
import {Button} from '@app/components/uikit/button/Button';
import DiscoveryStore from '@app/stores/DiscoveryStore';
import GuildStore from '@app/stores/GuildStore';
import {getApiErrorMessage} from '@app/utils/ApiErrorUtils';
import {getCurrentLocale} from '@app/utils/LocaleUtils';
import type {DiscoveryCategory} from '@fluxer/constants/src/DiscoveryConstants';
import {DiscoveryCategoryLabels} from '@fluxer/constants/src/DiscoveryConstants';
import {formatNumber} from '@fluxer/number_utils/src/NumberFormatting';
import {useLingui} from '@lingui/react/macro';
import {CaretRightIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useState} from 'react';

/** Gradient stops per category_type (0–8) for card banner. */
function getBannerGradient(categoryType: number): string {
	const gradients: Record<number, [string, string]> = {
		0: ['#2d5016', '#1a3d0a'],
		1: ['#4c1d4c', '#2e1233'],
		2: ['#5c3d1e', '#3d2814'],
		3: ['#1e3a5f', '#0f2744'],
		4: ['#0f4c5c', '#1a3a5f'],
		5: ['#5c1a4c', '#3d1233'],
		6: ['#5c1e1e', '#3d1414'],
		7: ['#2d1a4c', '#1a0f33'],
		8: ['#3d3d3d', '#2a2a2a'],
	};
	const [a, b] = gradients[categoryType] ?? gradients[8];
	return `linear-gradient(135deg, ${a} 0%, ${b} 100%)`;
}

interface DiscoveryGuildCardProps {
	guild: DiscoveryGuild;
}

export const DiscoveryGuildCard = observer(function DiscoveryGuildCard({guild}: DiscoveryGuildCardProps) {
	const {t} = useLingui();
	const [joining, setJoining] = useState(false);
	const isAlreadyMember = GuildStore.getGuild(guild.id) != null;
	const categoryLabel = DiscoveryCategoryLabels[guild.category_type as DiscoveryCategory] ?? '';
	const categoryLabelUpper = categoryLabel ? categoryLabel.toUpperCase() : '';
	const memberCount = formatNumber(guild.member_count, getCurrentLocale());
	const onlineCount = formatNumber(guild.online_count, getCurrentLocale());

	const handleJoin = useCallback(async () => {
		if (joining || isAlreadyMember) return;
		setJoining(true);
		try {
			await DiscoveryActionCreators.joinGuild(guild.id);
			DiscoveryStore.reset();
			ModalActionCreators.pop();
			NavigationActionCreators.selectGuild(guild.id);
		} catch (error) {
			setJoining(false);
			const message = getApiErrorMessage(error) ?? t`Failed to join this community. Please try again.`;
			ToastActionCreators.error(message);
		}
	}, [guild.id, joining, isAlreadyMember, t]);

	return (
		<article className={styles.card}>
			<div
				className={styles.cardBanner}
				style={{background: getBannerGradient(guild.category_type)}}
				aria-hidden
			/>
			{categoryLabelUpper && (
				<span className={styles.cardCategoryBadge}>{categoryLabelUpper}</span>
			)}
			<GuildIcon
				id={guild.id}
				name={guild.name}
				icon={guild.icon}
				className={styles.cardIcon}
			/>
			<div className={styles.cardBody}>
				<div className={styles.cardTitleRow}>
					<h3 className={styles.cardName}>{guild.name}</h3>
					<GuildBadge features={guild.features} />
				</div>
				<p className={styles.cardDescription}>
					{guild.description || t`No description provided.`}
				</p>
				<div className={styles.cardStats}>
					<span className={styles.cardStat}>
						<span className={styles.statDotOnline} aria-hidden />
						{t`${onlineCount} Online`}
					</span>
					<span className={styles.cardStat}>
						<span className={styles.statDotMembers} aria-hidden />
						{guild.member_count === 1 ? t`${memberCount} Member` : t`${memberCount} Members`}
					</span>
				</div>
				<div className={styles.cardAction}>
					<Button
						variant="primary"
						className={styles.joinButton}
						onClick={handleJoin}
						disabled={joining || isAlreadyMember}
						rightIcon={<CaretRightIcon size={16} weight="bold" />}
					>
						{isAlreadyMember
							? t`Joined`
							: (guild as {guild_type?: string}).guild_type === 'server'
								? t`Join Server`
								: t`Join Community`}
					</Button>
				</div>
			</div>
		</article>
	);
});
