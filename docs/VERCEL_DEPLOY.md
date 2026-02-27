# Deploy the frontend to Vercel (backend on your machine)

You can host **only the Fluxer web app (frontend)** on Vercel and keep the API, gateway, and other services running on your own computer. Your friends use the Vercel URL; the app talks to your backend.

## 1. Expose your backend with a public URL

The frontend needs to reach your API and WebSocket gateway over the internet. Options:

- **[ngrok](https://ngrok.com)** – e.g. `ngrok http 8080` and use the `https://xxxx.ngrok-free.app` URL
- **[Cloudflare Tunnel](https://developers.cloudflare.com/cloudflare-one/connections/connect-apps)** – free tunnel to your machine
- Your own domain pointing at your machine (if you have a static IP or DDNS)

Use **HTTPS** so the built app can use `wss://` for the gateway. Note the hostname (e.g. `xxxx.ngrok-free.app`).

## 2. Create a Vercel project and set env

1. Push this repo to GitHub (if you haven’t already) and [import it in Vercel](https://vercel.com/new).
2. In the Vercel project **Settings → Environment Variables**, add:

   - **Name:** `FLUXER_PUBLIC_DOMAIN`  
   - **Value:** your backend hostname only (no `https://`), e.g. `xxxx.ngrok-free.app`  
   - Apply to **Production** (and Preview if you want).

   **First time:** Redeploy once so the runtime endpoint `/api/fluxer-config` is live (it reads `FLUXER_PUBLIC_DOMAIN` at request time). After that, when your ngrok URL changes, run `.\scripts\set_vercel_backend_from_ngrok.ps1` with ngrok running, then **refresh the Vercel site** — no redeploy needed.

3. Leave **Root Directory** as the repo root. The existing `vercel.json` is set up to build the frontend and output from `fluxer_app/dist`.

## 3. Deploy

Trigger a deploy (e.g. push to `main` or “Redeploy” in Vercel). The build will:

- Use `config/config.vercel.json`
- Override the backend domain with `FLUXER_PUBLIC_DOMAIN`
- Run `build:vercel` (no WASM) and output to `fluxer_app/dist`

**Easiest:** Push to GitHub (`git push origin main`). The project is already linked to Vercel; it will build from the repo and avoid CLI upload limits. Or use **Redeploy** in the [Vercel dashboard](https://vercel.com/dashboard).

After deploy, open the Vercel URL. The app will load and use your public backend URL for API and gateway.

## 4. CORS and gateway on your backend

Your backend must:

- Allow requests from the Vercel frontend origin (e.g. `https://your-project.vercel.app`) in CORS.
- Be reachable at `https://<FLUXER_PUBLIC_DOMAIN>/api` and `wss://<FLUXER_PUBLIC_DOMAIN>/gateway` (or whatever paths your stack uses).

If you use a tunnel (ngrok, Cloudflare), the tunnel must forward to the same ports your API and gateway use locally.

## "Connection Issue" on the Vercel site

If the app shows **"Connection Issue – We're having trouble connecting to Fluxer's servers"**:

1. **Backend and tunnel must be running**
   - On your machine: `pnpm dev:servers` (so the API and gateway are up on 48763).
   - Then start ngrok: `npx ngrok http 48763` (or your proxy port).

2. **Point Vercel at that backend**
   - Run: `.\scripts\set_vercel_backend_from_ngrok.ps1` (reads ngrok’s current URL and sets `FLUXER_PUBLIC_DOMAIN` in Vercel).
   - Refresh the Vercel site in the browser. The app fetches `/api/fluxer-config`, gets the current domain, and connects.

3. **If it still fails**
   - Redeploy the project once so the serverless function `api/fluxer-config.js` is deployed (if you never deployed after it was added).
   - Confirm in Vercel **Settings → Environment Variables** that `FLUXER_PUBLIC_DOMAIN` is set for Production (and Preview if you use it).
   - Ensure ngrok is still running and the URL matches what you set (free ngrok gives a new URL each time you restart it — run the script again and refresh).

## Summary

| Where        | What runs                          |
|-------------|-------------------------------------|
| **Vercel**  | Static frontend (React app) + `/api/fluxer-config` (runtime backend URL) |
| **Your PC** | API, gateway, database, etc.        |

Set `FLUXER_PUBLIC_DOMAIN` to the public hostname of that backend. The app gets it at runtime from `/api/fluxer-config`, so updating the env and refreshing is enough after the first deploy.
