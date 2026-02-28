#!/usr/bin/env node
/**
 * One-off script: create a real account with email admin@gmail.com.
 * Run with: node scripts/create_admin_account.mjs
 * Requires the API server to be running (e.g. pnpm dev:full).
 */

import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const root = join(__dirname, '..');

function randomString(length, chars = 'abcdefghijklmnopqrstuvwxyz0123456789') {
	let s = '';
	for (let i = 0; i < length; i++) s += chars[Math.floor(Math.random() * chars.length)];
	return s;
}

function randomUsername() {
	return 'Admin_' + randomString(8, 'abcdefghjkmnpqrstuvwxyz23456789');
}

function randomPassword() {
	return randomString(16, 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%');
}

async function main() {
	let port = 49319;
	try {
		const configPath = join(root, 'config.json');
		const config = JSON.parse(readFileSync(configPath, 'utf8'));
		port = config?.services?.server?.port ?? port;
	} catch (_) {}

	const email = 'admin@gmail.com';
	const password = randomPassword();
	const username = randomUsername();
	const apiBase = `http://127.0.0.1:${port}/api`;

	const body = {
		email,
		username,
		global_name: username,
		password,
		date_of_birth: '1990-01-15',
		consent: true,
	};

	console.log('Creating account...');
	console.log('  Email:', email);
	console.log('  Username:', username);
	console.log('  Request:', apiBase + '/auth/register');

	const res = await fetch(apiBase + '/auth/register', {
		method: 'POST',
		headers: {
			'Content-Type': 'application/json',
			'X-Forwarded-For': '127.0.0.1',
		},
		body: JSON.stringify(body),
	});

	const text = await res.text();
	if (!res.ok) {
		console.error('Registration failed:', res.status, res.statusText);
		console.error(text);
		process.exit(1);
	}

	let data;
	try {
		data = JSON.parse(text);
	} catch (_) {
		data = {};
	}

	const token = data.token;
	const userId = data.user_id ?? data.userId;

	// Fetch profile to get server-assigned username and discriminator (tag)
	let displayUsername = username;
	let displayTag = username + '#????';
	if (token) {
		const meRes = await fetch(apiBase + '/users/@me', {
			headers: {
				Authorization: `Bearer ${token}`,
				'X-Forwarded-For': '127.0.0.1',
			},
		});
		if (meRes.ok) {
			const me = await meRes.json();
			displayUsername = me.username ?? username;
			const disc = me.discriminator != null ? String(me.discriminator).padStart(4, '0') : '0000';
			displayTag = displayUsername + '#' + disc;
		}
	}

	console.log('\nAccount created successfully.');
	console.log('---');
	console.log('Email:    ', email);
	console.log('Password: ', password);
	console.log('Username: ', displayUsername);
	console.log('Tag:      ', displayTag);
	console.log('User ID:  ', userId ?? '');
	console.log('---');
	console.log('Save the password above; it was generated randomly and is not stored in this script.');
}

main().catch((err) => {
	console.error(err);
	process.exit(1);
});
