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

const isCanary = process.env.BUILD_CHANNEL === 'canary';

const productName = isCanary ? 'Fluxer Canary' : 'Fluxer';
const appId = isCanary ? 'app.fluxer.canary' : 'app.fluxer';
const iconDir = isCanary ? 'icons-canary' : 'icons-stable';
const packageName = isCanary ? 'fluxer_desktop_canary' : 'fluxer_desktop';

/** @type {import('electron-builder').Configuration} */
module.exports = {
	appId,
	productName,
	// biome-ignore lint/suspicious/noTemplateCurlyInString: electron-builder placeholder syntax
	artifactName: '${productName}-${version}-${os}-${arch}.${ext}',

	directories: {
		buildResources: 'build_resources',
		output: 'dist-electron',
	},

	files: [
		'dist/main/**/*',
		'dist/preload/**/*',
		'package.json'
	],

	extraMetadata: {
		name: packageName,
	},

	extraResources: [
		{
			from: `build_resources/${iconDir}/badges/`,
			to: 'badges',
			filter: ['**/*'],
		},
	],

	asar: false,
	compression: 'maximum',

	win: {
		// icon: `build_resources/${iconDir}/icon.ico`,
		target: [
			{
				target: 'nsis',
				arch: ['x64', 'arm64'],
			},
			{
				target: 'squirrel',
				arch: ['x64'],
			},
		],
	},

	nsis: {
		oneClick: false,
		perMachine: false,
		allowToChangeInstallationDirectory: true,
		deleteAppDataOnUninstall: false,
		createDesktopShortcut: true,
		createStartMenuShortcut: true,
	},

	squirrelWindows: {
		iconUrl: `https://fluxerstatic.com/desktop/${iconDir}/icon.ico`,
		name: packageName,
	},

	publish: null,
};
