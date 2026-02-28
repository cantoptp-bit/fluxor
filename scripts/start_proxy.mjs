#!/usr/bin/env node
/**
 * Start the Fluxer app proxy on port 48763 (no Caddy required).
 * Uses config/config.prod.json (production config). Run after pnpm dev:servers.
 * Open http://localhost:48763
 */

import path from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const ROOT = path.resolve(__dirname, '..');
process.env.FLUXER_CONFIG = path.join(ROOT, 'config', 'config.prod.json');
process.env.FLUXER_PROJECT_ROOT = ROOT;
process.env.FORCE_COLOR = '1';

const { spawn } = await import('node:child_process');
const isWindows = process.platform === 'win32';
const [exec, args] = isWindows
  ? [process.env.ComSpec || 'cmd.exe', ['/c', 'pnpm', '--filter', 'fluxer_app_proxy', 'dev']]
  : ['pnpm', ['--filter', 'fluxer_app_proxy', 'dev']];

const child = spawn(exec, args, {
  cwd: ROOT,
  env: process.env,
  stdio: 'inherit',
  shell: false,
  windowsHide: true,
});
child.on('error', (err) => {
  console.error('[proxy] failed to start:', err.message);
  process.exit(1);
});
child.on('exit', (code, sig) => {
  if (code !== null && code !== 0) process.exit(code ?? 1);
  if (sig) process.exit(1);
});
