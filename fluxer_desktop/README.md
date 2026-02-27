# Pegasus Desktop

Electron desktop client for Pegasus. Wraps the web application with native platform integrations including notifications, global shortcuts, screen sharing, passkeys, and auto-updates.

## Configuration

The desktop client reads an optional `settings.json` file from the user data directory on startup. If the file does not exist, defaults are used.

### User data directory locations

| Platform | Stable                                               | Canary                                                     |
| -------- | ---------------------------------------------------- | ---------------------------------------------------------- |
| Windows  | `%APPDATA%\fluxer\settings.json`                     | `%APPDATA%\fluxercanary\settings.json`                     |
| macOS    | `~/Library/Application Support/fluxer/settings.json` | `~/Library/Application Support/fluxercanary/settings.json` |
| Linux    | `~/.config/fluxer/settings.json`                     | `~/.config/fluxercanary/settings.json`                     |

### Available options

| Key       | Type   | Default (Stable)         | Default (Canary)                | Description                         |
| --------- | ------ | ------------------------ | ------------------------------- | ----------------------------------- |
| `app_url` | string | `https://web.fluxer.app` | `https://web.canary.fluxer.app` | URL of the web application to load. |

### Example

```json
{
  "app_url": "https://my-instance.example.com"
}
```

When `app_url` is set, the desktop client loads that URL instead of the default and treats its origin as trusted for permissions, navigation, and the local RPC server.

## Tests (EXE and Cloudflare error)

Run unit tests to ensure the desktop app URL is correct and the deprecated tunnel is never used:

```bash
cd fluxer_desktop
pnpm test
```

This runs:

- **Constants.test.ts** – `STABLE_APP_URL` and `CANARY_APP_URL` are `https://fluxor-rust.vercel.app` (no home.auroraplayer.com).
- **DesktopConfig.test.ts** – `getAppUrl()` never returns the deprecated tunnel URL (avoids Error 1033 when launching the EXE).

To verify the EXE target URL loads and is not a Cloudflare error page (run from repo root):

```bash
pnpm --filter fluxer_app test -- vercelLoginPage.e2e.test.ts
```

That suite includes **EXE target URL (no Cloudflare error)** tests: home and login pages load and do not contain "Error 1033" or "Cloudflare Tunnel error".

## Building the Windows EXE

From the repo root (or from `fluxer_desktop` with dependencies installed):

```bash
cd fluxer_desktop
pnpm install
pnpm pack
```

This builds the Electron main/preload code and runs electron-builder for Windows. Output goes to `fluxer_desktop/dist-electron/`:

- **Portable**: `Pegasus X.X.X.exe` — single executable, no installer; run from anywhere.
- **NSIS installer**: `Pegasus Setup X.X.X.exe` — installs to Program Files and Start Menu.

Use `pnpm pack:dir` to produce only an unpacked app directory (no installer/portable exe), useful for development.

## Sharing the app with people outside your network

**Yes — they can use the exe**, as long as the app can reach the **backend (API)** that the desktop client is configured to use.

The desktop app is a wrapper: it loads the Pegasus web app from a **URL** (see `app_url` above). All data and auth go to that server. So:

- If the exe uses the **default URL** (e.g. a public instance like `https://home.auroraplayer.com` or your deployed backend), anyone with the exe and internet access can use it; they don’t need to be on your network.
- If the exe is configured to use **localhost** or a **local IP** (e.g. your machine), only people who can reach that address (same PC or same LAN with your server running) can use it.

To have people off your network use the exe, either:

1. Point the app at a **publicly hosted** Pegasus backend (set `app_url` in `settings.json` to that URL, or build with that as the default), or  
2. Run your backend somewhere reachable on the internet and configure the app to use it.

The exe itself does not include the server; it only needs to connect to a reachable backend URL.
