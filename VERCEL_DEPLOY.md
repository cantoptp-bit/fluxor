# Deploy the Fluxer frontend to Vercel

Use this guide to deploy the **fluxer_app** frontend to Vercel so you can share a live link (e.g. with your boss).

## Prerequisites

- A [Vercel account](https://vercel.com/signup)
- This repo pushed to **GitHub**, **GitLab**, or **Bitbucket** (Vercel deploys from git)

## One-time setup: build and commit WASM (required for Vercel)

The app uses a small Rust/WASM module. Vercel’s build doesn’t run Rust, so we commit the built WASM and use a “no WASM” build on Vercel.

**Do this once** (with Node and pnpm installed, and [Rust](https://rustup.rs/) installed if you don’t have it):

1. From the **repo root**:
   ```bash
   pnpm install
   cd fluxer_app && pnpm wasm:codegen && cd ..
   ```
2. If Rust isn’t installed, install it first: <https://rustup.rs/> then run the command above again.
3. Commit the built WASM and related files:
   ```bash
   git add fluxer_app/pkgs/libfluxcore/libfluxcore.js fluxer_app/pkgs/libfluxcore/libfluxcore.d.ts fluxer_app/pkgs/libfluxcore/libfluxcore_bg.wasm.d.ts fluxer_app/pkgs/libfluxcore/libfluxcore_bg.wasm
   git commit -m "chore: add built WASM for Vercel deploy"
   git push
   ```

After this, Vercel can build the frontend without installing Rust.

## Deploy to Vercel

### Option A: Deploy from the Vercel dashboard (recommended)

1. Go to [vercel.com](https://vercel.com) and sign in.
2. Click **Add New…** → **Project**.
3. **Import** your fluxor repo (GitHub/GitLab/Bitbucket).
4. Use these settings:
   - **Root Directory:** leave as **`.`** (repo root).
   - **Framework Preset:** leave as **Other** (or “None”).
   - **Build Command:** leave empty (the repo’s `vercel.json` sets it).
   - **Output Directory:** leave empty (set in `vercel.json`).
   - **Install Command:** leave empty (uses `pnpm install` from `vercel.json`).
5. Click **Deploy**.

Vercel will use the `vercel.json` in the repo, which:

- Runs `pnpm install` and builds the frontend with `build:vercel` (no Rust step).
- Uses **fluxer_app/dist** as the output.
- Serves the app as an SPA (all routes → `index.html`).

### Option B: Deploy with the Vercel CLI

1. Install the CLI: `npm i -g vercel`
2. From the **repo root**:
   ```bash
   vercel
   ```
3. Follow the prompts (link to your Vercel account and repo if asked).

Subsequent deploys: run `vercel --prod` for production, or push to git for automatic preview/production deploys.

## After deploy

- **Preview/Production URL:** Vercel will show a URL like `https://your-project.vercel.app`.
- The **UI will load**, but **login and real data will not work** unless the frontend is pointed at a running Fluxer API. The build uses `config/config.test.json`, which points at localhost. To use your own backend:
  - Run your API elsewhere (e.g. another host or service).
  - Add a Vercel config that uses a different config file or build-time env for API URL (e.g. a `config/config.vercel.json` that reads from env), then set the env in the Vercel project settings.

For a “show the UI” demo, the default deploy is enough; for a “working app” demo, you’ll need to wire the deployed frontend to your backend and possibly adjust config as above.

## Troubleshooting

- **Build fails with “FLUXER_CONFIG”:** Ensure you’re deploying from the repo root and not changing the root directory in Vercel. The build expects `config/config.test.json` at repo root.
- **Build fails on “wasm” or “libfluxcore”:** Make sure you’ve run the one-time WASM steps above and committed the listed files under `fluxer_app/pkgs/libfluxcore/`.
- **Blank page or 404 on refresh:** The `vercel.json` rewrites should send all non-asset routes to `index.html`. If not, check that the **Output Directory** in the Vercel project is not overridden and remains **fluxer_app/dist** (via `vercel.json`).
