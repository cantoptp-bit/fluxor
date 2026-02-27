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

import styles from '@app/components/layout/GuildsLayout.module.css';
import {Tooltip} from '@app/components/uikit/tooltip/Tooltip';
import {useHover} from '@app/hooks/useHover';
import {useMergeRefs} from '@app/hooks/useMergeRefs';
import {useLingui} from '@lingui/react/macro';
import {CaretDownIcon, UsersThreeIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import {useRef} from 'react';

/** Section header for the Communities list in the left sidebar. Click to expand/collapse the list below. */
interface CommunityButtonProps {
	className?: string;
	expanded?: boolean;
	onToggle?: () => void;
	itemCount?: number;
}

export const CommunityButton = observer(
	({className, expanded = true, onToggle, itemCount = 0}: CommunityButtonProps = {}) => {
		const {t} = useLingui();
		const [hoverRef, isHovering] = useHover();
		const iconRef = useRef<HTMLDivElement | null>(null);
		const mergedRef = useMergeRefs([hoverRef, iconRef]);
		const isClickable = typeof onToggle === 'function';
		const tooltipText = itemCount > 0 ? t`Communities (${itemCount})` : t`Communities`;

		const wrapperClassName = clsx(
			styles.fluxerButton,
			styles.guildListSectionHeader,
			isClickable && styles.guildListSectionHeaderClickable,
			isClickable && styles.guildListSectionHeaderButton,
			className,
		);

		const content = (
			<>
				{isHovering && (
					<div className={styles.guildIndicator}>
						<motion.span
							className={styles.guildIndicatorBar}
							initial={false}
							animate={{opacity: 1, scale: 1, height: 20}}
							transition={{duration: 0.2, ease: [0.25, 0.1, 0.25, 1]}}
						/>
					</div>
				)}
				<div className={styles.guildListSectionHeaderInner}>
					<div className={styles.relative}>
						<motion.div
							ref={iconRef}
							className={styles.fluxerButtonIcon}
							animate={{borderRadius: isHovering ? '30%' : '50%'}}
							initial={false}
							transition={{duration: 0.07, ease: 'easeOut'}}
						>
							<UsersThreeIcon weight="fill" className={styles.favoritesIcon} />
						</motion.div>
					</div>
					{isClickable && (
						<motion.span
							className={styles.guildListSectionHeaderChevron}
							initial={false}
							animate={{rotate: expanded ? 0 : -90}}
							transition={{duration: 0.15, ease: 'easeOut'}}
							aria-hidden
						>
							<CaretDownIcon weight="bold" />
						</motion.span>
					)}
				</div>
			</>
		);

		return (
			<Tooltip position="right" size="large" text={tooltipText}>
				{isClickable ? (
					<button
						ref={mergedRef}
						type="button"
						className={wrapperClassName}
						aria-label={t`Communities`}
						aria-expanded={expanded}
						data-guild-list-focus-item="true"
						onClick={(e) => {
							e.preventDefault();
							onToggle?.();
						}}
					>
						{content}
					</button>
				) : (
					<div ref={mergedRef} className={wrapperClassName} data-guild-list-focus-item="true">
						{content}
					</div>
				)}
			</Tooltip>
		);
	},
);
