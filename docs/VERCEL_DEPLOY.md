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

   **Redeploy after setting or changing it.** The build writes `backend-config.json` and injects the domain into `index.html` so the app can connect. In Vercel, ensure the variable is available for **Production** (and **Preview** if needed); it is used at **build time**. Run `.\scripts\set_vercel_backend_from_ngrok.ps1` with ngrok running to sync the env, then redeploy.

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

## Summary

| Where        | What runs                          |
|-------------|-------------------------------------|
| **Vercel**  | Static frontend (React app)        |
| **Your PC** | API, gateway, database, etc.        |

Set `FLUXER_PUBLIC_DOMAIN` to the public hostname of that backend so the frontend knows where to connect.
