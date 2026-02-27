#!/usr/bin/env node
/**
 * Remove compiled Lingui catalogs (messages.mjs) so that `lingui compile` always
 * runs from the current .po files. Prevents stale compiled messages in CI/Vercel.
 */
import { readdirSync, unlinkSync, existsSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const localesDir = join(__dirname, '..', 'src', 'locales');

for (const locale of readdirSync(localesDir)) {
	const mjs = join(localesDir, locale, 'messages.mjs');
	if (existsSync(mjs)) {
		unlinkSync(mjs);
	}
}
