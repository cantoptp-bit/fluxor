# Point home.auroraplayer.com to your tunnel (one-time)

Do these steps once. It takes about 2 minutes.

---

## Step 1: Open Cloudflare

1. In your browser, go to: **https://dash.cloudflare.com**
2. Log in if asked.

---

## Step 2: Open your domain’s DNS

1. On the main page you’ll see a list of your sites/domains.
2. **Click the name** of the domain that has “auroraplayer” in it (e.g. **auroraplayer.com**).
3. In the left sidebar, click **DNS** (under “DNS” or “Records”).

You should now see a list of DNS records (names like “www”, “@”, “home”, etc.).

---

## Step 3: Fix the “home” record

You need one record so that **home.auroraplayer.com** goes to your tunnel.

**Option A – There is already a “home” record**

1. Find the row where **Name** is **home** (or **home.auroraplayer.com**).
2. Click **Edit** (pencil icon) on that row.
3. Change:
   - **Type** → choose **CNAME**
   - **Target** → delete what’s there and paste exactly:  
     `63b3436d-a67c-49c7-b7be-f9819dbed56f.cfargotunnel.com`
4. Leave **Proxy status** as **Proxied** (orange cloud) if you see it.
5. Click **Save**.

**Option B – There is no “home” record (or you’re not sure)**

1. Click the **Add record** (or **Add an record**) button.
2. Fill in:
   - **Type:** choose **CNAME**
   - **Name:** type `home`
   - **Target:** paste exactly `63b3436d-a67c-49c7-b7be-f9819dbed56f.cfargotunnel.com`
   - **Proxy status:** leave as **Proxied** (orange cloud) if shown.
3. Click **Save**.

---

## Step 4: Wait and test

1. Wait 1–2 minutes.
2. Open: **https://home.auroraplayer.com**

If the tunnel and your app are running, the site should load.

---

## Quick copy-paste

- **Name:** `home`
- **Target:** `63b3436d-a67c-49c7-b7be-f9819dbed56f.cfargotunnel.com`

That’s all you need for DNS.

---

## If you see Error 1033 (Cloudflare Tunnel error)

**Error 1033** means Cloudflare cannot reach any running **cloudflared** connector. The tunnel is not connected.

**Note:** The script must invoke cloudflared as `tunnel --config <path> run fluxor` (the `--config` flag must come *before* `run`). If you see "flag provided but not defined: -config", the argument order is wrong.

1. **Start the tunnel**  
   From the project root run:  
   `.\scripts\start_tunnel.ps1`  
   (Do **not** use `-Background` the first time so you can see any errors in the terminal.)

2. **Keep it running**  
   Leave that terminal open. If you close it or the process exits, the tunnel disconnects and 1033 returns.

3. **Check the origin**  
   The tunnel sends traffic to `http://127.0.0.1:48763`. Ensure something is listening there, e.g. run `pnpm dev:proxy` in another terminal.

4. **Check connectivity**  
   Ensure your machine can reach the internet (no firewall blocking outbound HTTPS). In Cloudflare dashboard: **Networks** → **Connectors** → **Cloudflare Tunnels** and confirm the tunnel shows as **Active** when the script is running.
