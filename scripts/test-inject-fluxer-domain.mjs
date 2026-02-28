#!/usr/bin/env node
/**
 * Test script for inject-fluxer-domain.mjs: run with a temp dist, assert output.
 * Usage: node scripts/test-inject-fluxer-domain.mjs
 */
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(__dirname, '..');
const distDir = path.join(root, 'fluxer_app', 'dist');
const indexPath = path.join(distDir, 'index.html');
const configPath = path.join(distDir, 'backend-config.json');

const originalEnv = process.env.FLUXER_PUBLIC_DOMAIN;
const originalHtml = fs.existsSync(indexPath) ? fs.readFileSync(indexPath, 'utf8') : null;

function cleanup() {
	if (originalHtml !== null) {
		fs.writeFileSync(indexPath, originalHtml);
	} else if (fs.existsSync(indexPath)) {
		fs.unlinkSync(indexPath);
	}
	if (fs.existsSync(configPath)) {
		fs.unlinkSync(configPath);
	}
	if (originalEnv !== undefined) process.env.FLUXER_PUBLIC_DOMAIN = originalEnv;
	else delete process.env.FLUXER_PUBLIC_DOMAIN;
}

try {
	// Setup: ensure dist exists and has a minimal index.html
	if (!fs.existsSync(distDir)) {
		fs.mkdirSync(distDir, { recursive: true });
	}
	const minimalHtml = '<!DOCTYPE html><html><head><title>Test</title></head><body></body></html>';
	fs.writeFileSync(indexPath, minimalHtml);

	// Run the inject script in a child process so env is applied
	const injectPath = path.join(root, 'scripts', 'inject-fluxer-domain.mjs');
	const { execSync } = await import('node:child_process');
	execSync(`node "${injectPath}"`, {
		env: { ...process.env, FLUXER_PUBLIC_DOMAIN: 'test.ngrok-free.app' },
		cwd: root,
		stdio: 'pipe',
	});

	// Assert
	const htmlAfter = fs.readFileSync(indexPath, 'utf8');
	if (!htmlAfter.includes('__FLUXER_PUBLIC_DOMAIN__') || !htmlAfter.includes('test.ngrok-free.app')) {
		throw new Error('index.html missing injected domain');
	}

	const config = JSON.parse(fs.readFileSync(configPath, 'utf8'));
	if (config.base_domain !== 'test.ngrok-free.app' || config.api !== 'https://test.ngrok-free.app/api') {
		throw new Error('backend-config.json invalid: ' + JSON.stringify(config));
	}

	console.log('inject-fluxer-domain test: OK');
} finally {
	cleanup();
}
