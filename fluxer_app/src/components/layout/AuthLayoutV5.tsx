/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 */

import { AuthLayoutContext } from '@app/contexts/AuthLayoutContext';
import { AuthRegisterDraftContext, type AuthRegisterFormDraft } from '@app/contexts/AuthRegisterDraftContext';
import { useSetLayoutVariant } from '@app/contexts/LayoutVariantContext';
import i18n, { initI18n } from '@app/I18n';
import { I18nProvider } from '@lingui/react';
import { observer } from 'mobx-react-lite';
import type { ReactNode } from 'react';
import { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import styles from '@app/components/layout/AuthLayoutV5.module.css';

const noop = (_: unknown) => {};

const AuthLayoutV5Content = observer(function AuthLayoutV5Content({ children }: { children?: ReactNode }) {
	const registerFormDraftsRef = useRef<Map<string, AuthRegisterFormDraft>>(new Map());

	const getRegisterFormDraft = useCallback((draftKey: string): AuthRegisterFormDraft | undefined => {
		const draft = registerFormDraftsRef.current.get(draftKey);
		return draft ? { ...draft, formValues: { ...draft.formValues } } : undefined;
	}, []);

	const setRegisterFormDraft = useCallback((draftKey: string, draft: AuthRegisterFormDraft) => {
		registerFormDraftsRef.current.set(draftKey, { ...draft, formValues: { ...draft.formValues } });
	}, []);

	const clearRegisterFormDraft = useCallback((draftKey: string) => {
		registerFormDraftsRef.current.delete(draftKey);
	}, []);

	const authLayoutContextValue = useMemo(
		() => ({
			setSplashUrl: noop,
			setShowLogoSide: noop,
			setSplashCardAlignment: noop,
		}),
		[],
	);

	const authRegisterDraftContextValue = useMemo(
		() => ({
			getRegisterFormDraft,
			setRegisterFormDraft,
			clearRegisterFormDraft,
		}),
		[clearRegisterFormDraft, getRegisterFormDraft, setRegisterFormDraft],
	);

	useEffect(() => {
		document.documentElement.classList.add('auth-page');
		return () => document.documentElement.classList.remove('auth-page');
	}, []);

	return (
		<AuthRegisterDraftContext.Provider value={authRegisterDraftContextValue}>
			<AuthLayoutContext.Provider value={authLayoutContextValue}>
				<div className={styles.wrapper}>
					<div className={styles.scroll}>
						<div className={styles.bento}>
							<span className={styles.wordmark}>Pegasus</span>
							<span className={styles.tagline}>Sign in to your account</span>
							{children}
						</div>
					</div>
				</div>
			</AuthLayoutContext.Provider>
		</AuthRegisterDraftContext.Provider>
	);
});

export const AuthLayoutV5 = observer(function AuthLayoutV5({ children }: { children?: ReactNode }) {
	const [isI18nInitialized, setIsI18nInitialized] = useState(false);
	const setLayoutVariant = useSetLayoutVariant();

	useEffect(() => {
		setLayoutVariant('auth');
		return () => setLayoutVariant('app');
	}, [setLayoutVariant]);

	useEffect(() => {
		initI18n().then(() => setIsI18nInitialized(true));
	}, []);

	if (!isI18nInitialized) {
		return null;
	}

	return (
		<I18nProvider i18n={i18n}>
			<AuthLayoutV5Content>{children}</AuthLayoutV5Content>
		</I18nProvider>
	);
});
