# Starting Fluxer for local development

Everything is configured for **localhost**. Use this to run the full stack and test at **http://localhost:48763**.

## Option A: Full stack with Docker (recommended on Windows)

### One command (PowerShell)

```powershell
pnpm dev:full
```

Starts Docker (Docker Desktop is started automatically if needed), backing services, and dev servers. Open **http://localhost:48763**.

**From the IDE (Cursor/VS Code):** Run the task **Fluxer: Full Dev** (Terminal → Run Task…) so Docker starts first, then the full stack.

**Docker won't start or nothing happens?** See [docs/DOCKER_TROUBLESHOOTING.md](docs/DOCKER_TROUBLESHOOTING.md) for fixes (WSL, Task Manager, reinstall, etc.).

**Test accounts for messaging and features:** See [docs/DEV_ACCOUNTS.md](docs/DEV_ACCOUNTS.md) for the official admin account and five test accounts (alice, bob, carol, dave, eve) with login credentials.

### Step by step

1. **Start backing services** (Valkey, Meilisearch, NATS, Mailpit, Cassandra):
   ```bash
   docker compose -f compose.dev.yaml up -d
   ```

2. **Start app servers** (uses `config/config.json`; domain and API are already set):
   ```bash
   pnpm dev:servers
   ```
   This starts: **proxy (48763)**, fluxer_server (49319), fluxer_app (49427), fluxer_marketing (49531). Open **http://localhost:48763**. When using Cassandra, it automatically waits for Cassandra and runs migrations (so temp chat works).

3. **Optional:** If you prefer Caddy instead of the built-in Node proxy, stop the script and run `caddy run --config dev/Caddyfile.dev --adapter caddyfile`, then run `pnpm dev:servers` again (or run the proxy separately: `pnpm dev:proxy` in another terminal). Without any proxy, open **http://localhost:49427** (app only; API at localhost:49319).

4. **Open in browser**
   - **http://localhost:48763** (use this when you ran `pnpm dev:servers` or `pnpm dev:full`).
   - With Caddy on 48763: **http://home.auroraplayer.com:48763** if DNS/hosts points to your machine.
   - Without proxy: **http://localhost:49427** (app only; API at localhost:49319).
## Option B: With devenv (Linux/macOS or WSL)

```bash
devenv up
```

Then open **http://localhost:48763** (or your configured domain).

## Config (local)

- **config/config.json** uses `base_domain: "localhost"`, `public_scheme: "http"`, `public_port: 48763`, and `app_public.bootstrap_api_endpoint: "/api"`.
- The Node proxy (or Caddy) on **48763** serves the built app from `fluxer_app/dist` and proxies `/api` and `/gateway` to the backend. Run `pnpm --filter fluxer_app build` so the proxy has assets to serve.

### Cassandra backend (default)

Cassandra is the default database for development. When you run `pnpm dev:servers` or `pnpm dev:full`, the script automatically waits for Cassandra (port 9042) and runs migrations—so temp chat works without any extra steps. If you start Docker separately, Cassandra needs ~60–90s to boot before migrations succeed.

### Optional: Cloudflare Tunnel (https://home.auroraplayer.com)

To have the app at **https://home.auroraplayer.com/channels/@me** (and everywhere else):

1. **Terminal 1:** `pnpm dev:servers`
2. **Terminal 2:** `pnpm dev:proxy` (must run on 48763 — tunnel points here)
3. **Terminal 3:** `pnpm tunnel` or `.\scripts\start_tunnel.ps1`

One-time tunnel setup: see `dev/cloudflared-config.yml.example` — run `cloudflared tunnel login`, create tunnel, route DNS to `home.auroraplayer.com`, then copy and edit the config.

The server uses `X-Forwarded-Host` and `X-Forwarded-Proto` from the tunnel so the app gets the right API/gateway URLs.

## Deploy frontend to Vercel

- **Backend:** Run on your machine (e.g. `pnpm dev:servers`). Expose it with a tunnel (ngrok, Cloudflare Tunnel, etc.) if the Vercel app should call it from the internet.
- **Frontend:** Deploy **fluxer_app** to Vercel (e.g. set root to `fluxer_app`, build `pnpm build`, output `dist`). Configure the app’s API/base URL (e.g. via env or your bootstrap endpoint) to point at your exposed backend so the deployed app can reach `/api` and `/gateway`.

## Verification and password reset emails (local dev)

Password reset and verification emails are sent over SMTP. In development we use **Mailpit** so nothing goes to a real inbox.

1. **Start the dev stack** so Mailpit is running (`docker compose -f compose.dev.yaml up -d` or `pnpm dev:full`). Mailpit is in `compose.dev.yaml`.
2. **Enable email in config:** `config/config.json` includes an `integrations.email` block that points SMTP at Mailpit (port **49621**). With that in place, “Forgot password” and verification flows send email.
3. **View emails:** Open **http://localhost:8025** — that’s the Mailpit UI. Every email the app sends (password reset, verification, etc.) appears there. Use the latest message to get the reset or verification link.

If you don’t see emails, confirm Mailpit is up and `integrations.email.enabled` is `true` with `provider: "smtp"` and `smtp.port: 49621`. For production, switch to a real SMTP provider and set `from_email` (and optionally `smtp`) accordingly.

#### Sending to your real Gmail inbox

To have password reset (and other) emails delivered to your actual Gmail inbox:

1. **Use a Gmail account for sending** (e.g. your own Gmail or a dedicated address like `yourapp.noreply@gmail.com`).
2. **Turn on 2-Step Verification** for that Google account: Google Account → Security → 2-Step Verification.
3. **Create an App Password**: Google Account → Security → 2-Step Verification → App passwords. Choose "Mail" and your device, then generate. Copy the 16-character password (no spaces).
4. **Update `config/config.json`** — replace the `integrations.email` block with Gmail SMTP:

```json
"email": {
  "enabled": true,
  "provider": "smtp",
  "from_email": "your-sending@gmail.com",
  "from_name": "Fluxer",
  "smtp": {
    "host": "smtp.gmail.com",
    "port": 587,
    "username": "your-sending@gmail.com",
    "password": "your-16-char-app-password",
    "secure": true
  }
}
```

Use your real Gmail for `from_email` and `username`, and the App Password for `password`. Restart the app server so it reloads config. After that, "Forgot password" will send the reset link to the account's email (including your Gmail) and it will arrive in your Gmail inbox.

#### Using Hostinger email

To send password reset and other emails via your Hostinger email (e.g. `noreply@yourdomain.com`):

1. In **Hostinger hPanel** go to **Emails** → your domain → create or use an existing mailbox. Note the full address and password.
2. **SMTP details:** Hostinger uses `smtp.hostinger.com`. You can use **port 587** (STARTTLS) or **port 465** (SSL).
3. **Update `config/config.json`** — set `integrations.email` to your Hostinger mailbox:

```json
"email": {
  "enabled": true,
  "provider": "smtp",
  "from_email": "noreply@yourdomain.com",
  "from_name": "Fluxer",
  "smtp": {
    "host": "smtp.hostinger.com",
    "port": 587,
    "username": "noreply@yourdomain.com",
    "password": "your-email-account-password",
    "secure": false
  }
}
```

- Replace `noreply@yourdomain.com` with your Hostinger email address (and use the same for `username`).
- Replace `your-email-account-password` with that mailbox’s password.
- **Port 587:** use `"port": 587` and `"secure": false` (STARTTLS).  
- **Port 465:** use `"port": 465` and `"secure": true` (SSL).

Restart the app server after saving config. Password reset and verification emails will then be sent from your Hostinger address. Hostinger limits (e.g. 100/day for free email, 1000–3000/day for Business) apply per mailbox.

## See changes without restarting servers

You usually **don’t need to restart** the whole stack to see code changes:

- **Backend (fluxer_server, fluxer_app_proxy)**  
  Both use `tsx watch` and **restart automatically** when you save a file. No manual restart.

- **Frontend (fluxer_app)**  
  The dev server (rspack) rebuilds on save. To see changes:
  - **Refresh the browser** (F5 or Ctrl+R). Prefer a **hard refresh** (Ctrl+Shift+R / Cmd+Shift+R) if the change doesn’t show up.
  - You do **not** need to restart `pnpm dev:servers` for React/UI changes.

- **When to restart**  
  Restart (`pnpm dev:restart` or restart `pnpm dev:servers`) when you change env, config, or something that only runs on startup. For normal code edits, save + refresh is enough.

- **Restart frees RAM**  
  `pnpm dev:restart` kills the **entire process tree** (parent Node + all dev server children), so old processes don’t stay in Task Manager and use memory. If you used to see many Node/pnpm processes after restart, they should now be fully terminated.

- **After restart: "Connection refused" is fixed**  
  The proxy starts first and binds to port 48763 right away, so **http://localhost:48763** is reachable immediately. You may see a 502 or the static app (if built) until the app dev server finishes compiling (1–2 min); then the live app loads. If you had `ERR_CONNECTION_REFUSED` before, that was because the old order started the app first and only then the proxy, so 48763 had no listener for several minutes.

- **Console: "SES Removing unpermitted intrinsics"**  
  This comes from a **browser extension** (e.g. MetaMask or another Secure EcmaScript/lockdown extension), not from Fluxer. You can ignore it or disable that extension on localhost to hide it.

## Ports

| Port   | Service        |
|--------|----------------|
| 48763  | App proxy (or Caddy)   |
| 49319  | fluxer_server  |
| 49427  | fluxer_app (dev) |
| 49531  | fluxer_marketing |
| 49107  | fluxer_gateway (mock when test_mode_enabled) |
| 8025   | Mailpit (view dev emails) |
| 49621  | SMTP → Mailpit (used by app) |
| 6379   | Valkey         |
| 7700   | Meilisearch    |
| 4222   | NATS core      |
| 4223   | NATS JetStream |
| 9042   | Cassandra      |