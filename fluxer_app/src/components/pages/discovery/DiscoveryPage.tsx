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

import { Input } from '@app/components/form/Input';
import styles from '@app/components/pages/discovery/DiscoveryPage.module.css';
import { DiscoveryGuildCard } from '@app/components/modals/discovery/DiscoveryGuildCard';
import { Button } from '@app/components/uikit/button/Button';
import { Spinner } from '@app/components/uikit/Spinner';
import foodPatternUrl from '@app/images/i-like-food.svg';
import DiscoveryStore from '@app/stores/DiscoveryStore';
import { useLingui } from '@lingui/react/macro';
import { CompassIcon, MagnifyingGlassIcon, StarIcon, FireIcon } from '@phosphor-icons/react';
import { clsx } from 'clsx';
import { observer } from 'mobx-react-lite';
import { useCallback, useEffect, useRef, useState } from 'react';
import { motion } from 'framer-motion';

export const DiscoveryPage = observer(function DiscoveryPage() {
    const { t } = useLingui();
    const searchTimerRef = useRef<ReturnType<typeof setTimeout> | null>(null);
    const [activeCategory, setActiveCategory] = useState<number | 'featured' | null>('featured');

    useEffect(() => {
        void DiscoveryStore.loadCategories();
        void DiscoveryStore.search({ offset: 0 });
        return () => {
            if (searchTimerRef.current) {
                clearTimeout(searchTimerRef.current);
            }
        };
    }, []);

    const handleSearchChange = useCallback((value: string) => {
        if (searchTimerRef.current) {
            clearTimeout(searchTimerRef.current);
        }
        searchTimerRef.current = setTimeout(() => {
            void DiscoveryStore.search({ query: value, offset: 0 });
            if (value && activeCategory === 'featured') {
                setActiveCategory(null);
            }
        }, 300);
    }, [activeCategory]);

    const handleCategoryClick = useCallback((categoryId: number | 'featured' | null) => {
        setActiveCategory(categoryId);
        if (categoryId === 'featured') {
            void DiscoveryStore.search({ category: null, offset: 0 });
        } else {
            void DiscoveryStore.search({ category: categoryId, offset: 0 });
        }
    }, []);

    const handleLoadMore = useCallback(() => {
        void DiscoveryStore.search({ offset: DiscoveryStore.guilds.length });
    }, []);

    const hasMore = DiscoveryStore.guilds.length < DiscoveryStore.total;

    // Featured communities could be a curated list. For now, let's take the top 4 guilds by member count
    // or just the first 4 loaded as 'featured' when featured category is active.
    const featuredGuilds = activeCategory === 'featured' ? DiscoveryStore.guilds.slice(0, 4) : [];
    const regularGuilds = activeCategory === 'featured' ? DiscoveryStore.guilds.slice(4) : DiscoveryStore.guilds;

    return (
        <div className={styles.discoveryRoot}>
            <aside className={styles.sidebar} aria-label={t`Explore categories`}>
                <div className={styles.sidebarHeader}>
                    <CompassIcon size={28} weight="duotone" className={styles.sidebarIcon} />
                    <span className={styles.sidebarTitle}>{t`Discover`}</span>
                </div>
                <nav className={styles.categories}>
                    <button
                        type="button"
                        className={clsx(
                            styles.categoryItem,
                            activeCategory === 'featured' && styles.categoryItemActive,
                        )}
                        onClick={() => handleCategoryClick('featured')}
                    >
                        <StarIcon size={20} weight={activeCategory === 'featured' ? "fill" : "duotone"} className={styles.categoryIcon} />
                        {t`Featured`}
                    </button>
                    <div className={styles.divider} />
                    <button
                        type="button"
                        className={clsx(
                            styles.categoryItem,
                            activeCategory === null && styles.categoryItemActive,
                        )}
                        onClick={() => handleCategoryClick(null)}
                    >
                        <FireIcon size={20} weight={activeCategory === null ? "fill" : "regular"} className={styles.categoryIcon} />
                        {t`All Communities`}
                    </button>
                    {DiscoveryStore.categories.map((cat) => (
                        <button
                            key={cat.id}
                            type="button"
                            className={clsx(
                                styles.categoryItem,
                                activeCategory === cat.id && styles.categoryItemActive,
                            )}
                            onClick={() => handleCategoryClick(cat.id)}
                        >
                            <div className={styles.categoryIconPlaceholder} />
                            {cat.name}
                        </button>
                    ))}
                </nav>
            </aside>

            <main className={styles.main}>
                <div className={styles.hero}>
                    <div className={styles.heroBackground}>
                        <div className={styles.heroGradient} />
                        <div className={styles.heroPattern} style={{ backgroundImage: `url(${foodPatternUrl})` }} />
                    </div>
                    <motion.div
                        initial={{ opacity: 0, y: 20 }}
                        animate={{ opacity: 1, y: 0 }}
                        transition={{ duration: 0.6, ease: "easeOut" }}
                        className={styles.heroContent}
                    >
                        <h1 className={styles.heroTitle}>{t`Find your community`}</h1>
                        <p className={styles.heroSubtitle}>{t`From gaming, to music, to learning, there's a place for you.`}</p>
                        <div className={styles.searchBar}>
                            <Input
                                className={styles.searchInput}
                                placeholder={t`Explore communities...`}
                                defaultValue={DiscoveryStore.query}
                                onChange={(e) => handleSearchChange(e.target.value)}
                                leftIcon={<MagnifyingGlassIcon size={20} weight="bold" className={styles.searchIcon} />}
                            />
                        </div>
                    </motion.div>
                </div>

                <div className={styles.mainContentWrapper}>
                    <div className={styles.mainContent}>
                        {DiscoveryStore.loading && DiscoveryStore.guilds.length === 0 ? (
                            <div className={styles.loadingState}>
                                <Spinner />
                            </div>
                        ) : (
                            DiscoveryStore.guilds.length > 0 && (
                                <>
                                    {activeCategory === 'featured' && featuredGuilds.length > 0 && (
                                        <div className={styles.featuredSection}>
                                            <h2 className={styles.sectionTitle}>{t`Featured Communities`}</h2>
                                            <div className={styles.featuredGrid}>
                                                {featuredGuilds.map((guild) => (
                                                    <DiscoveryGuildCard key={guild.id} guild={guild} />
                                                ))}
                                            </div>
                                            {regularGuilds.length > 0 && (
                                                <h2 className={styles.sectionTitle} style={{ marginTop: '32px' }}>{t`More Communities`}</h2>
                                            )}
                                        </div>
                                    )}

                                    {(regularGuilds.length > 0) && (
                                        <div className={styles.grid}>
                                            {regularGuilds.map((guild) => (
                                                <DiscoveryGuildCard key={guild.id} guild={guild} />
                                            ))}
                                        </div>
                                    )}

                                    {hasMore && (
                                        <div className={styles.loadMore}>
                                            <Button
                                                variant="primary"
                                                onClick={handleLoadMore}
                                                disabled={DiscoveryStore.loading}
                                                className={styles.loadMoreButton}
                                            >
                                                {DiscoveryStore.loading ? t`Loading...` : t`Load More`}
                                            </Button>
                                        </div>
                                    )}
                                </>
                            )
                        )}
                    </div>
                </div>
            </main>
        </div>
    );
});
