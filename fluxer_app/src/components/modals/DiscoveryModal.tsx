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
import {Input} from '@app/components/form/Input';
import styles from '@app/components/modals/DiscoveryModal.module.css';
import {DiscoveryGuildCard} from '@app/components/modals/discovery/DiscoveryGuildCard';
import * as Modal from '@app/components/modals/Modal';
import {Button} from '@app/components/uikit/button/Button';
import {Spinner} from '@app/components/uikit/Spinner';
import foodPatternUrl from '@app/images/i-like-food.svg';
import DiscoveryStore from '@app/stores/DiscoveryStore';
import {useLingui} from '@lingui/react/macro';
import {
	BookOpenIcon,
	CodeIcon,
	FilmStripIcon,
	GameControllerIcon,
	HouseIcon,
	MagnifyingGlassIcon,
	MusicNotesIcon,
	PaintBrushIcon,
	StarIcon,
	TagIcon,
	TelevisionIcon,
	UserCircleIcon,
	UsersThreeIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useRef} from 'react';

export const DiscoveryModal = observer(function DiscoveryModal() {
	const {t} = useLingui();
	const searchTimerRef = useRef<ReturnType<typeof setTimeout> | null>(null);

	useEffect(() => {
		void DiscoveryStore.loadCategories();
		void DiscoveryStore.search({offset: 0});
		return () => {
			if (searchTimerRef.current) {
				clearTimeout(searchTimerRef.current);
			}
		};
	}, []);

	const handleSearchChange = useCallback((value: string) => {
		DiscoveryStore.setQuery(value);
		if (searchTimerRef.current) {
			clearTimeout(searchTimerRef.current);
		}
		searchTimerRef.current = setTimeout(() => {
			void DiscoveryStore.search({query: value, offset: 0});
		}, 300);
	}, []);

	const handleSearchSubmit = useCallback(() => {
		void DiscoveryStore.search({query: DiscoveryStore.query, offset: 0});
	}, []);

	const handleCategoryClick = useCallback((categoryId: number | null) => {
		void DiscoveryStore.search({category: categoryId, offset: 0});
	}, []);

	const handleLoadMore = useCallback(() => {
		void DiscoveryStore.search({offset: DiscoveryStore.guilds.length});
	}, []);

	const handleRetry = useCallback(() => {
		void DiscoveryStore.loadCategories();
		void DiscoveryStore.search({offset: 0});
	}, []);

	const handleClearAndRefresh = useCallback(() => {
		void DiscoveryStore.search({query: '', category: null, offset: 0});
	}, []);

	const handleSortChange = useCallback((sortBy: string) => {
		void DiscoveryStore.search({sortBy, offset: 0});
	}, []);

	const handleClose = useCallback(() => {
		DiscoveryStore.reset();
		ModalActionCreators.pop();
	}, []);

	const hasMore = DiscoveryStore.guilds.length < DiscoveryStore.total;

	const getCategoryIcon = (categoryId: number) => {
		const iconProps = {size: 18, weight: 'duotone' as const, className: styles.categoryIcon};
		switch (categoryId) {
			case 0:
				return <GameControllerIcon {...iconProps} />;
			case 1:
				return <MusicNotesIcon {...iconProps} />;
			case 2:
				return <FilmStripIcon {...iconProps} />;
			case 3:
				return <BookOpenIcon {...iconProps} />;
			case 4:
				return <CodeIcon {...iconProps} />;
			case 5:
				return <UserCircleIcon {...iconProps} />;
			case 6:
				return <PaintBrushIcon {...iconProps} />;
			case 7:
				return <TelevisionIcon {...iconProps} />;
			case 8:
			default:
				return <TagIcon {...iconProps} />;
		}
	};

	return (
		<Modal.Root size="fullscreen" onClose={handleClose} className={styles.discoveryRoot}>
			<Modal.ScreenReaderLabel text={t`Explore Communities`} />
			<Modal.InsetCloseButton onClick={handleClose} />

			<div className={styles.layout}>
				<aside className={styles.sidebar} aria-label={t`Browse categories`}>
					<div className={styles.sidebarHeader}>
						<span className={styles.sidebarLabel}>{t`BROWSE BY`}</span>
					</div>
					<nav className={styles.categories}>
						<button
							type="button"
							className={clsx(
								styles.categoryItem,
								DiscoveryStore.category === null && styles.categoryItemActive,
							)}
							onClick={() => handleCategoryClick(null)}
						>
							<HouseIcon size={18} weight="duotone" className={styles.categoryIcon} />
							<span>{t`Home`}</span>
						</button>
						{DiscoveryStore.categories.map((cat) => (
							<button
								key={cat.id}
								type="button"
								className={clsx(
									styles.categoryItem,
									DiscoveryStore.category === cat.id && styles.categoryItemActive,
								)}
								onClick={() => handleCategoryClick(cat.id)}
							>
								{getCategoryIcon(cat.id)}
								<span>{cat.name}</span>
							</button>
						))}
					</nav>
				</aside>

				<div className={styles.main}>
					<div className={styles.hero}>
						<div className={styles.heroBackground} aria-hidden>
							<div className={styles.heroPattern} style={{backgroundImage: `url(${foodPatternUrl})`}} />
						</div>
						<div className={styles.heroContent}>
							<button type="button" className={styles.heroPill} aria-label={t`Explore Fluxer`}>
								<StarIcon size={14} weight="fill" className={styles.heroPillIcon} />
								<span>{t`Explore Fluxer`}</span>
							</button>
							<h1 className={styles.heroTitle}>
								{t`Find your `}
								<span className={styles.heroTitleGradient}>{t`community.`}</span>
							</h1>
							<div className={styles.heroSearchRow}>
								<Input
									className={styles.searchInput}
									placeholder={t`Search for topics, tags, or servers...`}
									value={DiscoveryStore.query}
									onChange={(e) => handleSearchChange(e.target.value)}
									onKeyDown={(e) => e.key === 'Enter' && handleSearchSubmit()}
									leftIcon={<MagnifyingGlassIcon size={16} weight="bold" />}
								/>
								<Button variant="primary" className={styles.heroSearchButton} onClick={handleSearchSubmit}>
									{t`Search`}
								</Button>
							</div>
						</div>
					</div>
					<div className={styles.mainBackground} aria-hidden>
						<div className={styles.mainPattern} style={{backgroundImage: `url(${foodPatternUrl})`}} />
					</div>
					<Modal.Content className={styles.mainContent} padding="none">
						{DiscoveryStore.error ? (
							<div className={styles.errorState}>
								<p className={styles.errorStateMessage}>
									{t`Something went wrong loading communities. Please try again.`}
								</p>
								<Button variant="primary" onClick={handleRetry}>
									{t`Try again`}
								</Button>
							</div>
						) : DiscoveryStore.loading && DiscoveryStore.guilds.length === 0 ? (
							<div className={styles.loadingState}>
								<Spinner />
							</div>
						) : DiscoveryStore.guilds.length > 0 ? (
							<>
								<div className={styles.sectionHeader}>
									<h2 className={styles.sectionTitle}>{t`Featured Communities`}</h2>
									{DiscoveryStore.total > 0 && (
										<div className={styles.sectionMeta}>
											<span className={styles.resultCount} aria-live="polite">
												{DiscoveryStore.total === 1
													? t`Showing 1 result`
													: t`Showing ${DiscoveryStore.total} results`}
											</span>
											<select
												id="discovery-sort"
												className={styles.sortSelect}
												value={DiscoveryStore.sortBy}
												onChange={(e) => handleSortChange(e.target.value)}
												aria-label={t`Sort communities by`}
											>
												<option value="member_count">{t`Most members`}</option>
												<option value="online_count">{t`Most online`}</option>
												<option value="relevance">{t`Relevance`}</option>
											</select>
										</div>
									)}
								</div>
								<div className={styles.grid}>
									{DiscoveryStore.guilds.map((guild) => (
										<DiscoveryGuildCard key={guild.id} guild={guild} />
									))}
								</div>
								{hasMore && (
									<div className={styles.loadMore}>
										<Button
											variant="primary"
											onClick={handleLoadMore}
											disabled={DiscoveryStore.loading}
											leftIcon={DiscoveryStore.loading ? <Spinner size="small" className={styles.loadMoreSpinner} /> : undefined}
										>
											{DiscoveryStore.loading ? t`Loading...` : t`Load More`}
										</Button>
									</div>
								)}
							</>
						) : (
							<div className={styles.emptyState}>
								<UsersThreeIcon size={48} weight="duotone" className={styles.emptyStateIcon} aria-hidden />
								<h3 className={styles.emptyStateTitle}>
									{DiscoveryStore.query.trim() || DiscoveryStore.category !== null
										? t`No communities match your search`
										: t`No communities yet`}
								</h3>
								<p className={styles.emptyStateDescription}>
									{DiscoveryStore.query.trim() || DiscoveryStore.category !== null
										? t`Try a different search or category, or view all communities.`
										: t`Check back later or try a different category.`}
								</p>
								{(DiscoveryStore.query.trim() || DiscoveryStore.category !== null) && (
									<Button variant="secondary" onClick={handleClearAndRefresh}>
										{t`View all`}
									</Button>
								)}
							</div>
						)}
					</Modal.Content>
				</div>
			</div>
		</Modal.Root>
	);
});
