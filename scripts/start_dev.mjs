#!/usr/bin/env node
/**
 * Start Fluxer dev servers (proxy, server, app, marketing).
 * Uses config/config.json for local dev when present, else config/config.prod.json.
 *
 * Automatically starts:
 * - Docker backing services (Valkey, Meilisearch, NATS, Mailpit) if available
 * - Reverse proxy on port 48763 (single URL for app + API)
 * - fluxer_server (49319), fluxer_app (49427), fluxer_marketing (49531)
 *
 * Open http://localhost:48763 when ready. Emails at http://localhost:8025.
 */

import { execSync, spawn } from 'node:child_process';
import fs from 'node:fs';
import net from 'node:net';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const ROOT = path.resolve(__dirname, '..');

const configJson = path.join(ROOT, 'config', 'config.json');
const configProdJson = path.join(ROOT, 'config', 'config.prod.json');
const configPath = fs.existsSync(configJson) ? configJson : configProdJson;

function waitForPort(host, port, maxWaitMs = 90000) {
  const start = Date.now();
  return new Promise((resolve) => {
    function tryConnect() {
      const socket = new net.Socket();
      socket.setTimeout(2000);
      socket.on('connect', () => {
        socket.destroy();
        resolve(true);
      });
      socket.on('error', () => {
        socket.destroy();
        if (Date.now() - start >= maxWaitMs) resolve(false);
        else setTimeout(tryConnect, 2000);
      });
      socket.connect(port, host);
    }
    tryConnect();
  });
}

function isCassandraBackend() {
  try {
    const raw = fs.readFileSync(configPath, 'utf8');
    const config = JSON.parse(raw);
    return config?.database?.backend === 'cassandra';
  } catch {
    return false;
  }
}

function runCassandraMigrations() {
  try {
    execSync('pnpm exec tsx scripts/CassandraMigrate.tsx --host 127.0.0.1 up', {
      cwd: path.join(ROOT, 'fluxer_api'),
      stdio: 'pipe',
      encoding: 'utf8',
    });
  } catch (_) {
    // Migrate may fail if Cassandra not ready or already applied; continue
  }
}
const env = {
  ...process.env,
  FLUXER_CONFIG: path.isAbsolute(configPath) ? configPath : path.join(ROOT, configPath),
  FLUXER_PROJECT_ROOT: ROOT,
  FORCE_COLOR: '1',
};

const isWindows = process.platform === 'win32';
const PORTS_TO_FREE = [48763, 49319, 49427, 49107, 49531];

function freePortOnWindows(port) {
  try {
    const out = execSync(`netstat -ano | findstr ":${port} "`, {
      encoding: 'utf8',
      shell: true,
      stdio: ['pipe', 'pipe', 'pipe'],
    });
    const lines = out.split(/\r?\n/).filter((line) => line.includes('LISTENING'));
    for (const line of lines) {
      const m = line.trim().split(/\s+/);
      const pid = m[m.length - 1];
      if (pid && pid !== '0') {
        // /T = kill process tree so parent Node/pnpm and all children exit (avoids leftover processes in Task Manager)
        execSync(`taskkill /F /T /PID ${pid}`, { stdio: 'ignore', shell: true });
      }
    }
  } catch (_) {
    // netstat or taskkill failed (e.g. no process on port)
  }
}

function run(name, cmd, args, opts = {}) {
  const cwd = opts.cwd || ROOT;
  const [exec, execArgs] = isWindows
    ? [process.env.ComSpec || 'cmd.exe', ['/c', cmd, ...args]]
    : [cmd, args];
  const child = spawn(exec, execArgs, {
    cwd,
    env: { ...env, ...opts.env },
    stdio: 'inherit',
    shell: false,
    windowsHide: true,
  });
  child.on('error', (err) => {
    console.error(`[${name}] failed to start:`, err.message);
  });
  child.on('exit', (code, sig) => {
    if (code !== null && code !== 0) console.error(`[${name}] exited ${code}`);
    if (sig) console.error(`[${name}] killed by ${sig}`);
  });
  return child;
}

if (!fs.existsSync(configPath)) {
  console.error('Config file not found:', configPath);
  console.error('Create config/config.json or config/config.prod.json. See config/config.prod.json for a template.');
  process.exit(1);
}
console.log('FLUXER_CONFIG=', configPath);

// Start Docker backing services (Valkey, Meilisearch, NATS, Mailpit, Cassandra)
const composePath = path.join(ROOT, 'compose.dev.yaml');
try {
  execSync(`docker compose -f "${composePath}" up -d`, {
    stdio: 'ignore',
    cwd: ROOT,
  });
} catch (_) {
  // Docker not running or compose failed; continue without backing services
}

// If using Cassandra, wait for it and run migrations so temp chat works
async function ensureCassandraReady() {
  if (!isCassandraBackend()) return;
  console.log('Cassandra backend: waiting for Cassandra (port 9042)...');
  const ready = await waitForPort('127.0.0.1', 9042);
  if (ready) {
    console.log('Running Cassandra migrations for temp chat...');
    runCassandraMigrations();
  }
}

// Run synchronously via then to avoid restructuring; migrations must complete before servers start
await ensureCassandraReady();

if (isWindows) {
  for (const port of PORTS_TO_FREE) {
    freePortOnWindows(port);
  }
  // Give the OS a moment to release ports before binding
  await new Promise((r) => setTimeout(r, 1500));
}

/** Wait for app dev server to serve assets (first compile can take 1–2 min). */
async function waitForAppDevReady(appPort, maxWaitMs = 180000) {
  const start = Date.now();
  const url = `http://127.0.0.1:${appPort}/assets/main.js`;
  while (Date.now() - start < maxWaitMs) {
    try {
      const controller = new AbortController();
      const t = setTimeout(() => controller.abort(), 15000);
      const res = await fetch(url, { signal: controller.signal });
      clearTimeout(t);
      if (res.ok) {
        return true;
      }
    } catch (_) {
      // not ready yet
    }
    await new Promise((r) => setTimeout(r, 3000));
  }
  return false;
}

// Start proxy first so http://localhost:48763 is reachable immediately after restart
// (avoids ERR_CONNECTION_REFUSED while app compiles). Proxy returns 502/static fallback until app is up.
console.log('Starting proxy first (so port 48763 is reachable right away)...\n');
const proxyProcess = run('proxy', 'pnpm', ['--filter', 'fluxer_app_proxy', 'dev']);
const proxyPortReady = await waitForPort('127.0.0.1', 48763, 60000);
if (!proxyPortReady) {
  console.error('Proxy did not bind to port 48763 in 60s. Check config (services.app_proxy.port) and that the port is free.');
  proxyProcess.kill('SIGTERM');
  process.exit(1);
}
console.log('Proxy is up at http://localhost:48763. Starting app (first compile may take 1–2 min)...\n');

try {
  execSync('pnpm --filter @fluxer/marketing run build:css', { cwd: ROOT, stdio: 'pipe' });
} catch (_) {
  console.warn('Marketing CSS build failed; /terms and other pages may render without styles.');
}

const appProcess = run('fluxer_app', 'pnpm', ['--filter', 'fluxer_app', 'dev'], {
  env: { ...env, FLUXER_APP_DEV_PORT: '49427' },
});
const serverProcess = run('fluxer_server', 'pnpm', ['--filter', 'fluxer_server', 'dev']);
const marketingProcess = run('marketing', 'pnpm', ['--filter', 'fluxer_marketing', 'dev']);

// Wait for API server and app in parallel so we know both are up (API can be slow after Cassandra migrations)
const [appPortReady, serverPortReady] = await Promise.all([
  waitForPort('127.0.0.1', 49427, 120000),
  waitForPort('127.0.0.1', 49319, 90000),
]);

if (!serverPortReady) {
  console.error('fluxer_server did not bind to port 49319 in time. Check FLUXER_CONFIG and terminal for errors.');
  appProcess.kill('SIGTERM');
  proxyProcess.kill('SIGTERM');
  serverProcess.kill('SIGTERM');
  marketingProcess.kill('SIGTERM');
  process.exit(1);
}
console.log('API server is up on port 49319.');

if (!appPortReady) {
  console.error('App dev server did not bind to port 49427 in time. Proxy, server, and marketing are still running.');
  console.error('Open http://localhost:48763 — you may see 502 or a static build until the app is run separately.');
  appProcess.kill('SIGTERM');
}

const marketingPortReady = await waitForPort('127.0.0.1', 49531, 20000);
if (!marketingPortReady) {
  console.warn('Marketing (49531) did not bind in time; /help may not work until it is up.');
}

let appAssetsReady = false;
if (appPortReady) {
  console.log('App dev server port open, waiting for assets to compile...');
  appAssetsReady = await waitForAppDevReady(49427);
  if (appAssetsReady) {
    console.log('App dev server ready.\n');
  } else {
    console.warn('App assets not ready in time; page may load slowly on first open.\n');
  }
}

const processes = appPortReady
  ? [proxyProcess, appProcess, serverProcess, marketingProcess]
  : [proxyProcess, serverProcess, marketingProcess];
console.log('Open http://localhost:48763 when ready. Stop with Ctrl+C.\n');

processes.forEach((p) => {
  p.on('exit', (code) => {
    if (code !== null && code !== 0) {
      processes.forEach((q) => {
        if (q !== p && q.exitCode === null) q.kill('SIGTERM');
      });
      process.exit(1);
    }
  });
});

process.on('SIGINT', () => {
  processes.forEach((p) => p.kill('SIGTERM'));
  process.exit(0);
});
process.on('SIGTERM', () => {
  processes.forEach((p) => p.kill('SIGTERM'));
  process.exit(0);
});
