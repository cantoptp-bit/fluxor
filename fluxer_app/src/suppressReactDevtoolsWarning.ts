/**
 * Suppress known noisy console messages.
 *
 * - "Download the React DevTools": from React in development.
 * - "Uncompiled message detected!" / "Please compile your catalog": from Lingui in production when the deployed catalog is stale (suppressed so users aren't spammed; fix by running lingui:extract && lingui:compile before deploy).
 *
 * Note: "SES Removing unpermitted intrinsics" (lockdown-install.js) is from a browser extension (e.g. MetaMask), not from this app; it cannot be fixed in-app.
 *
 * Strategy 1 – DevTools hook: React 19 checks hook.isDisabled in
 * injectInternals(). Setting it to true makes injectInternals return truthy,
 * so the outer `if (!injectInternals(...))` is false and the prompt is skipped.
 *
 * Strategy 2 – console intercept: Because the Cursor browser extension may
 * inject its own __REACT_DEVTOOLS_GLOBAL_HOOK__ after our code runs (and
 * React fires the warning when its hook fails checkDCE), we also patch
 * console.info and console.warn to drop that specific message.
 *
 * Must run before react-dom/client initialises (i.e. first import in index.tsx).
 */
if (typeof window !== 'undefined') {
	// --- Strategy 1: hook ---
	type ReactDevToolsHook = {
		isDisabled?: boolean;
		checkDCE?: unknown;
		[key: string]: unknown;
	};
	const w = window as unknown as { __REACT_DEVTOOLS_GLOBAL_HOOK__?: ReactDevToolsHook };
	const hook = w.__REACT_DEVTOOLS_GLOBAL_HOOK__;
	if (!hook) {
		w.__REACT_DEVTOOLS_GLOBAL_HOOK__ = { isDisabled: true };
	} else if (!hook.checkDCE && hook.isDisabled !== false) {
		hook.isDisabled = true;
	}

	// --- Strategy 2: console intercept ---
	const DEVTOOLS_PATTERN = /Download the React DevTools/;
	const LINGUI_UNCOMPILED_PATTERN = /Uncompiled message detected!|Please compile your catalog/;

	const _origInfo = console.info.bind(console);
	console.info = (...args: Array<unknown>): void => {
		if (typeof args[0] === 'string' && DEVTOOLS_PATTERN.test(args[0])) return;
		_origInfo(...args);
	};

	const _origWarn = console.warn.bind(console);
	console.warn = (...args: Array<unknown>): void => {
		if (typeof args[0] === 'string' && DEVTOOLS_PATTERN.test(args[0])) return;
		// In production, suppress Lingui "Uncompiled message" so users aren't spammed (fix: run lingui:extract && lingui:compile before deploy).
		if (process.env.NODE_ENV === 'production' && args.some((a) => typeof a === 'string' && LINGUI_UNCOMPILED_PATTERN.test(a))) return;
		_origWarn(...args);
	};
}
