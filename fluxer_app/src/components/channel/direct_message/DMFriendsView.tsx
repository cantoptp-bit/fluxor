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

import * as RelationshipActionCreators from '@app/actions/RelationshipActionCreators';
import * as UserProfileActionCreators from '@app/actions/UserProfileActionCreators';
import { ActiveNowSidebar } from '@app/components/channel/active_now/ActiveNowSidebar';
import { ChannelHeader } from '@app/components/channel/ChannelHeader';
import { AddFriendView } from '@app/components/channel/direct_message/AddFriendView';
import styles from '@app/components/channel/direct_message/DMFriendsView.module.css';
import type { FriendsTab } from '@app/components/channel/friends/FriendsTypes';
import { FriendsList } from '@app/components/channel/friends/views/FriendsList';
import { PendingFriendsView } from '@app/components/channel/friends/views/PendingFriendsView';
import { Input } from '@app/components/form/Input';
import FocusRing from '@app/components/uikit/focus_ring/FocusRing';
import { MentionBadge } from '@app/components/uikit/MentionBadge';
import { Scroller } from '@app/components/uikit/Scroller';
import { useFluxerDocumentTitle } from '@app/hooks/useFluxerDocumentTitle';
import type { RelationshipRecord } from '@app/records/RelationshipRecord';
import FriendsTabStore from '@app/stores/FriendsTabStore';
import MobileLayoutStore from '@app/stores/MobileLayoutStore';
import RelationshipStore from '@app/stores/RelationshipStore';
import { RelationshipTypes } from '@fluxer/constants/src/UserConstants';
import { useLingui } from '@lingui/react/macro';
import { MagnifyingGlassIcon, UsersThreeIcon, PaintBrushIcon, SlidersHorizontalIcon, XIcon } from '@phosphor-icons/react';
import { clsx } from 'clsx';
import { observer } from 'mobx-react-lite';
import React, { useCallback, useEffect, useMemo, useRef, useState } from 'react';

interface TabButtonProps extends Omit<React.ButtonHTMLAttributes<HTMLButtonElement>, 'onClick'> {
	tab: FriendsTab;
	activeTab: FriendsTab;
	onClick: (tab: FriendsTab) => void;
	label: string;
	badge?: number;
	primary?: boolean;
}

const TabButton = observer(
	React.forwardRef<HTMLButtonElement, TabButtonProps>(
		({ tab, activeTab, onClick, label, badge, primary, ...props }, ref) => {
			const isActive = activeTab === tab && !primary;

			return (
				<FocusRing within offset={-2}>
					<button
						ref={ref}
						type="button"
						role="tab"
						aria-selected={isActive}
						tabIndex={isActive || (primary && activeTab === tab) ? 0 : -1}
						className={clsx(styles.tabButton, {
							[styles.active]: isActive,
							[styles.primary]: primary,
						})}
						onClick={() => onClick(tab)}
						{...props}
					>
						<div className={styles.tabContent}>
							{label}
							{badge !== undefined && badge > 0 && <MentionBadge mentionCount={badge} />}
						</div>
					</button>
				</FocusRing>
			);
		},
	),
);

export const DMFriendsView: React.FC = observer(() => {
	const { t } = useLingui();
	const [activeTab, setActiveTab] = useState<FriendsTab>('all');
	const mobileLayout = MobileLayoutStore;
	const relationships = RelationshipStore.getRelationships();
	const pendingCount = relationships.filter((relation) => relation.type === RelationshipTypes.INCOMING_REQUEST).length;
	const tabRefs = useRef<Array<HTMLButtonElement | null>>([]);
	const [searchQuery, setSearchQuery] = useState('');

	const [backgroundMedia, setBackgroundMedia] = useState<{ url: string; isVideo: boolean } | null>(null);
	const [blurAmount, setBlurAmount] = useState(0);
	const [tintColor, setTintColor] = useState('#000000');
	const [tintOpacity, setTintOpacity] = useState(0);
	const [contentOpacity, setContentOpacity] = useState(1);
	const [isSettingsOpen, setIsSettingsOpen] = useState(false);

	const fileInputRef = useRef<HTMLInputElement>(null);

	const handleFileChange = useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
		const file = e.target.files?.[0];
		if (!file) return;
		const url = URL.createObjectURL(file);
		setBackgroundMedia({ url, isVideo: file.type.startsWith('video/') });
		setIsSettingsOpen(true);
	}, []);

	useEffect(() => {
		const pendingTab = FriendsTabStore.consumeTab();
		if (pendingTab) {
			setActiveTab(pendingTab);
		}
	}, []);

	// Refetch relationships from API when opening Friends view so list is up to date (Ready payload can be stale after seed)
	useEffect(() => {
		void RelationshipActionCreators.fetchRelationships();
	}, []);

	const openProfile = useCallback((userId: string) => {
		UserProfileActionCreators.openUserProfile(userId);
	}, []);

	useFluxerDocumentTitle(t`My Friends`);

	const renderTabContent = () => {
		const relationshipsRecord = relationships.reduce(
			(acc, rel) => {
				acc[rel.id] = rel;
				return acc;
			},
			{} as Record<string, RelationshipRecord>,
		);

		switch (activeTab) {
			case 'add':
				return <AddFriendView />;
			case 'pending':
				return (
					<PendingFriendsView relationships={relationshipsRecord} openProfile={openProfile} searchQuery={searchQuery} />
				);
			case 'online':
				return <FriendsList showOnlineOnly={true} openProfile={openProfile} searchQuery={searchQuery} />;
			case 'all':
				return <FriendsList showOnlineOnly={false} openProfile={openProfile} searchQuery={searchQuery} />;
		}
	};

	const handleKeyDown = (e: React.KeyboardEvent, index: number) => {
		const tabCount = 4;
		if (e.key === 'ArrowRight') {
			e.preventDefault();
			const nextIndex = (index + 1) % tabCount;
			tabRefs.current[nextIndex]?.focus();
		} else if (e.key === 'ArrowLeft') {
			e.preventDefault();
			const prevIndex = (index - 1 + tabCount) % tabCount;
			tabRefs.current[prevIndex]?.focus();
		} else if (e.key === 'Home') {
			e.preventDefault();
			tabRefs.current[0]?.focus();
		} else if (e.key === 'End') {
			e.preventDefault();
			tabRefs.current[tabCount - 1]?.focus();
		}
	};

	const FriendsHeaderContent = (
		<div className={styles.headerContent}>
			{!mobileLayout.enabled && (
				<>
					<div className={styles.titleSection}>
						<UsersThreeIcon weight="fill" className={styles.titleIcon} />
						<span className={styles.titleText}>{t`My Friends`}</span>
					</div>
					<div className={styles.divider} />
				</>
			)}

			<div className={styles.tabsWrapper}>
				<Scroller
					className={styles.tabsScroller}
					orientation="horizontal"
					overflow="auto"
					fade={false}
					key="dm-friends-tabs-scroller"
				>
					<div className={styles.tabsInner} role="tablist">
						<TabButton
							ref={(el) => {
								tabRefs.current[0] = el;
							}}
							tab="online"
							activeTab={activeTab}
							onClick={setActiveTab}
							label={t`Online`}
							onKeyDown={(e) => handleKeyDown(e, 0)}
						/>
						<TabButton
							ref={(el) => {
								tabRefs.current[1] = el;
							}}
							tab="all"
							activeTab={activeTab}
							onClick={setActiveTab}
							label={t`All`}
							onKeyDown={(e) => handleKeyDown(e, 1)}
						/>
						<TabButton
							ref={(el) => {
								tabRefs.current[2] = el;
							}}
							tab="pending"
							activeTab={activeTab}
							onClick={setActiveTab}
							label={t`Pending`}
							badge={pendingCount}
							onKeyDown={(e) => handleKeyDown(e, 2)}
						/>
						<TabButton
							ref={(el) => {
								tabRefs.current[3] = el;
							}}
							tab="add"
							activeTab={activeTab}
							onClick={setActiveTab}
							label={t`Add Friend`}
							primary
							onKeyDown={(e) => handleKeyDown(e, 3)}
						/>
					</div>
				</Scroller>
			</div>
		</div>
	);

	const searchPlaceholder = useMemo(() => {
		switch (activeTab) {
			case 'online':
				return t`Search online friends`;
			case 'all':
				return t`Search friends`;
			case 'pending':
				return t`Search pending requests`;
			default:
				return t`Search friends`;
		}
	}, [activeTab]);

	const showSearchBar = activeTab !== 'add';

	return (
		<div className={styles.container} style={{ position: 'relative', overflow: 'hidden', backgroundColor: backgroundMedia ? 'transparent' : undefined }}>
			{backgroundMedia && (
				backgroundMedia.isVideo ? (
					// biome-ignore lint/a11y/useMediaCaption: background video
					<video
						src={backgroundMedia.url}
						autoPlay
						loop
						muted
						playsInline
						style={{
							position: 'absolute',
							top: 0,
							left: 0,
							width: '100%',
							height: '100%',
							objectFit: 'cover',
							zIndex: 0,
							pointerEvents: 'none',
							filter: `blur(${blurAmount}px)`
						}}
					/>
				) : (
					<img
						src={backgroundMedia.url}
						alt="Custom background"
						style={{
							position: 'absolute',
							top: 0,
							left: 0,
							width: '100%',
							height: '100%',
							objectFit: 'cover',
							zIndex: 0,
							pointerEvents: 'none',
							filter: `blur(${blurAmount}px)`
						}}
					/>
				)
			)}
			{backgroundMedia && (
				<div style={{
					position: 'absolute',
					top: 0,
					left: 0,
					width: '100%',
					height: '100%',
					backgroundColor: tintColor,
					opacity: tintOpacity,
					zIndex: 0,
					pointerEvents: 'none'
				}} />
			)}
			{backgroundMedia && (
				<div style={{
					position: 'absolute',
					top: 0,
					left: 0,
					width: '100%',
					height: '100%',
					backgroundColor: 'var(--background-secondary-lighter)',
					opacity: contentOpacity,
					zIndex: 0,
					pointerEvents: 'none'
				}} />
			)}
			<div style={{ position: 'relative', zIndex: 1, display: 'flex', width: '100%', height: '100%' }}>
				<div
					className={styles.mainColumn}
					style={{
						backgroundColor: backgroundMedia ? 'transparent' : undefined,
						backdropFilter: backgroundMedia && contentOpacity < 1 ? 'blur(10px)' : undefined,
					}}
				>
					<ChannelHeader leftContent={FriendsHeaderContent} showMembersToggle={false} showPins={false} />
					<div className={styles.content}>
						{showSearchBar && (
							<div className={styles.searchWrapper}>
								<Input
									value={searchQuery}
									onChange={(event) => setSearchQuery(event.currentTarget.value)}
									placeholder={searchPlaceholder}
									aria-label={searchPlaceholder}
									spellCheck={false}
									autoComplete="off"
									leftIcon={<MagnifyingGlassIcon weight="bold" className={styles.searchIcon} />}
								/>
							</div>
						)}
						<div className={styles.tabBody}>{renderTabContent()}</div>
					</div>
				</div>
				<ActiveNowSidebar />
			</div>

			<input
				type="file"
				ref={fileInputRef}
				onChange={handleFileChange}
				style={{ display: 'none' }}
				accept="image/*,video/mp4,video/webm"
			/>

			<div style={{
				position: 'absolute',
				bottom: '24px',
				right: '24px',
				zIndex: 2,
				display: 'flex',
				flexDirection: 'column',
				gap: '12px',
				alignItems: 'flex-end'
			}}>
				{isSettingsOpen && backgroundMedia && (
					<div style={{
						backgroundColor: 'var(--background-secondary)',
						borderRadius: '12px',
						padding: '16px',
						width: '240px',
						boxShadow: '0 8px 32px rgba(0,0,0,0.5)',
						display: 'flex',
						flexDirection: 'column',
						gap: '16px',
						border: '1px solid var(--background-modifier-accent)',
						backdropFilter: 'blur(10px)'
					}}>
						<div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
							<span style={{ fontWeight: 600, fontSize: '14px', color: 'var(--text-primary)' }}>{t`Background Settings`}</span>
							<FocusRing offset={-2}>
								<button
									type="button"
									onClick={() => setIsSettingsOpen(false)}
									style={{ background: 'none', border: 'none', cursor: 'pointer', color: 'var(--text-secondary)', padding: '4px' }}
								>
									<XIcon size={16} />
								</button>
							</FocusRing>
						</div>

						<div style={{ display: 'flex', flexDirection: 'column', gap: '8px' }}>
							<div style={{ display: 'flex', justifyContent: 'space-between', fontSize: '12px', color: 'var(--text-secondary)' }}>
								<span>{t`Blur`}</span>
								<span>{blurAmount}px</span>
							</div>
							<input
								type="range"
								min="0"
								max="20"
								value={blurAmount}
								onChange={(e) => setBlurAmount(Number(e.target.value))}
								style={{ width: '100%', accentColor: 'var(--brand-primary)' }}
							/>
						</div>

						<div style={{ display: 'flex', flexDirection: 'column', gap: '8px' }}>
							<div style={{ display: 'flex', justifyContent: 'space-between', fontSize: '12px', color: 'var(--text-secondary)' }}>
								<span>{t`Grey Box Opacity`}</span>
								<span>{Math.round(contentOpacity * 100)}%</span>
							</div>
							<input
								type="range"
								min="0"
								max="1"
								step="0.01"
								value={contentOpacity}
								onChange={(e) => setContentOpacity(Number(e.target.value))}
								style={{ width: '100%', accentColor: 'var(--brand-primary)' }}
							/>
						</div>

						<div style={{ display: 'flex', flexDirection: 'column', gap: '8px' }}>
							<div style={{ display: 'flex', justifyContent: 'space-between', fontSize: '12px', color: 'var(--text-secondary)' }}>
								<span>{t`Color Tint Opacity`}</span>
								<span>{Math.round(tintOpacity * 100)}%</span>
							</div>
							<input
								type="range"
								min="0"
								max="1"
								step="0.01"
								value={tintOpacity}
								onChange={(e) => setTintOpacity(Number(e.target.value))}
								style={{ width: '100%', accentColor: 'var(--brand-primary)' }}
							/>
						</div>

						<div style={{ display: 'flex', flexDirection: 'column', gap: '8px' }}>
							<span style={{ fontSize: '12px', color: 'var(--text-secondary)' }}>{t`Tint Color`}</span>
							<input
								type="color"
								value={tintColor}
								onChange={(e) => setTintColor(e.target.value)}
								style={{ width: '100%', height: '32px', border: 'none', borderRadius: '4px', background: 'none', cursor: 'pointer' }}
							/>
						</div>

						<div style={{ fontSize: '11px', color: 'var(--text-muted)', textAlign: 'center', marginTop: '4px' }}>
							{t`Tip: You can select .mp4 or .webm files for animated backgrounds!`}
						</div>

						<FocusRing offset={-2}>
							<button
								type="button"
								onClick={() => fileInputRef.current?.click()}
								style={{
									padding: '8px',
									borderRadius: '6px',
									backgroundColor: 'var(--background-modifier-accent)',
									color: 'var(--text-primary)',
									border: 'none',
									cursor: 'pointer',
									fontSize: '12px',
									fontWeight: 600
								}}
							>
								{t`Change File`}
							</button>
						</FocusRing>
					</div>
				)}

				<FocusRing offset={-2}>
					<button
						type="button"
						onClick={() => backgroundMedia ? setIsSettingsOpen(!isSettingsOpen) : fileInputRef.current?.click()}
						style={{
							width: '44px',
							height: '44px',
							borderRadius: '50%',
							backgroundColor: 'var(--brand-primary)',
							color: 'white',
							display: 'flex',
							alignItems: 'center',
							justifyContent: 'center',
							cursor: 'pointer',
							border: 'none',
							boxShadow: '0 4px 12px rgba(0,0,0,0.3)',
							transition: 'transform 0.2s',
						}}
						title={backgroundMedia ? t`Edit Settings` : t`Set Background`}
					>
						{backgroundMedia ? <SlidersHorizontalIcon size={22} weight="fill" /> : <PaintBrushIcon size={22} weight="fill" />}
					</button>
				</FocusRing>
			</div>
		</div>
	);
});


