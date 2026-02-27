# Deploy Fluxer backend on AWS EC2

This guide walks you through hosting the Fluxer backend on a single AWS EC2 instance so it runs 24/7 without using your PC. **You do the steps yourself** (no need to share any passwords or keys).

**Time:** about 20–30 minutes.  
**Cost:** Free tier (t2.micro) for 12 months, then low cost if you keep one small instance.

---

## Quick Start (do these 6 things)

Do the steps below in order. You only need to make a few choices; the rest is copy-paste.

| # | Where | What to do |
|---|--------|------------|
| 1 | AWS website | Create one small server (EC2) and download a key file. |
| 2 | Your PC | Open PowerShell or Terminal and paste 2 commands to connect and install Docker. |
| 3 | Same terminal (on the server) | Paste 3 commands to clone Fluxer and create config. |
| 4 | Same terminal | Paste 1 command to start Fluxer. |
| 5 | Browser | Open `http://YOUR_SERVER_IP:8080` to test. |
| 6 | (Optional) | Point your domain at the server later. |

---

### Step 1: Create the server in AWS (one time)

1. Open **[AWS Console → EC2](https://console.aws.amazon.com/ec2)** and sign in.
2. Click the orange **Launch instance** button.
3. Leave most defaults. Change only:
   - **Name:** `fluxer` (or any name).
   - **Application and OS Images:** pick **Ubuntu**.
   - **Instance type:** **t2.micro** (free tier).
   - **Key pair:** click **Create new key pair**, name it `fluxer-key`, type **RSA**, format **.pem**. Click **Create key pair** — a file will download. Put it somewhere safe (e.g. `Downloads`).
4. Under **Network settings**, click **Edit**. Then:
   - **Allow SSH:** from **My IP** (or **Anywhere** only for quick testing).
   - Click **Add security group rule** three times and add:
     - Type **HTTP**, Source **Anywhere**
     - Type **HTTPS**, Source **Anywhere**
     - Type **Custom TCP**, Port **8080**, Source **Anywhere**
5. Click **Launch instance**.
6. Wait ~1 minute. On the EC2 page, click your instance and copy its **Public IPv4 address** (e.g. `54.123.45.67`). You need this and the key file for the next step.

---

### Step 2: Connect and install Docker (copy-paste on your PC)

Open **PowerShell** (Windows) or **Terminal** (Mac/Linux). Replace the two placeholders and run the commands.

**Replace:**
- `C:\Users\YourName\Downloads\fluxer-key.pem` → path to your downloaded `.pem` file.
- `54.123.45.67` → your instance’s **Public IPv4 address** from Step 1.

**Command 1 — fix key permission (Mac/Linux only; skip on Windows):**
```bash
chmod 400 /path/to/fluxer-key.pem
```

**Command 2 — connect and install Docker (run from your PC):**
```bash
ssh -i "C:\Users\YourName\Downloads\fluxer-key.pem" ubuntu@54.123.45.67
```

After you’re connected, you’ll see something like `ubuntu@ip-172-...`. Then run this **on the server** (one block):

```bash
curl -fsSL https://get.docker.com | sudo sh
sudo usermod -aG docker ubuntu
exit
```

Then connect again (same SSH command as above). After that, Docker is installed.

---

### Step 3: Clone Fluxer and create config (copy-paste on the server)

You should be connected over SSH (you see `ubuntu@...`). Paste these **one block at a time**:

```bash
sudo apt-get update && sudo apt-get install -y git
git clone https://github.com/fluxerapp/fluxor.git
cd fluxer
```

Then create a minimal config (replace `54.123.45.67` with your server’s public IP if you want the app to use it):

```bash
bash scripts/aws-ec2-minimal-config.sh 54.123.45.67
```

That creates `config/config.json` for you. No need to edit it for a first test.

---

### Step 4: Start Fluxer (one command)

Still in the `fluxor` folder on the server:

```bash
sudo docker compose up -d
```

Wait ~30 seconds, then check:

```bash
sudo docker compose ps
curl -s http://127.0.0.1:8080/_health
```

If you see `ok` or a healthy JSON response, the backend is running.

---

### Step 5: Open it in your browser

On your PC, open a browser and go to:

**`http://YOUR_SERVER_IP:8080`**

(Use the same IP you used in the SSH command.) If the security group allows port 8080, you should see the API or health response.

---

### Step 6 (optional): Use your own domain

When you’re ready, add an **A record** in your domain’s DNS: point `chat.yourdomain.com` (or any name) to your server’s **Public IP**. Then you can put a reverse proxy (e.g. Caddy) on the server for HTTPS — see **Part 4** below.

---

## Prerequisites

- AWS account ([create one](https://aws.amazon.com))
- A domain (optional; you can use the EC2 public IP to test first)
- SSH client (built into macOS/Linux; on Windows use PowerShell or [OpenSSH](https://docs.microsoft.com/en-us/windows-server/administration/openssh/openssh_install_firstuse))

---

## Part 1: Create an EC2 instance (detailed)

1. **Log in** to the [AWS Console](https://console.aws.amazon.com) and open **EC2** (search “EC2” in the top bar).

2. **Launch instance**
   - Click **Launch instance**.
   - **Name:** e.g. `fluxer-backend`.
   - **AMI:** **Ubuntu Server 22.04 LTS**.
   - **Instance type:** **t2.micro** (1 vCPU, 1 GB RAM) – free tier eligible.
   - **Key pair:** Create new or use existing. Download the `.pem` file and keep it safe (e.g. `~/.ssh/fluxer-aws.pem`). You need it to SSH.
   - **Network / Security group:** Create or use a security group that allows:
     - **SSH (22)** from your IP (or `0.0.0.0/0` only for testing; restrict later).
     - **HTTP (80)** and **HTTPS (443)** from `0.0.0.0/0` so the app and API are reachable.
     - **Custom TCP 8080** from `0.0.0.0/0` if you will access the API on port 8080 before putting a reverse proxy in front.
   - **Storage:** 20–30 GB is enough.
   - Click **Launch instance**.

3. **Allocate an Elastic IP (recommended)**  
   So the public IP does not change after reboot:
   - EC2 → **Elastic IPs** → **Allocate Elastic IP address** → **Allocate**.
   - Select the new IP → **Actions** → **Associate Elastic IP address** → choose your instance → **Associate**.

4. **Note the public IP** (or Elastic IP) of the instance. You will use it to SSH and later for DNS.

---

## Part 2: Connect and prepare the server

1. **SSH into the instance** (from your PC):

   ```bash
   ssh -i /path/to/your-key.pem ubuntu@YOUR_EC2_PUBLIC_IP
   ```

   If you get “key too open”:

   ```bash
   chmod 400 /path/to/your-key.pem
   ```

2. **Run the bootstrap script** to install Docker and Docker Compose and prepare for Fluxer:

   From your **local machine** (where you have the Fluxer repo), copy the script to the server and run it:

   ```bash
   scp -i /path/to/your-key.pem scripts/aws-ec2-bootstrap.sh ubuntu@YOUR_EC2_PUBLIC_IP:~/
   ssh -i /path/to/your-key.pem ubuntu@YOUR_EC2_PUBLIC_IP 'chmod +x aws-ec2-bootstrap.sh && ./aws-ec2-bootstrap.sh'
   ```

   Or, if you already cloned the repo on the EC2 instance:

   ```bash
   cd /path/to/fluxor
   chmod +x scripts/aws-ec2-bootstrap.sh
   ./scripts/aws-ec2-bootstrap.sh
   ```

   The script installs Docker and Docker Compose. After it finishes, continue below.

---

## Part 3: Clone Fluxer and add config

On the EC2 instance (over SSH):

1. **Clone the repo** (or copy it from your machine with `scp` / rsync):

   ```bash
   sudo apt-get update && sudo apt-get install -y git
   git clone https://github.com/fluxerapp/fluxor.git
   cd fluxor
   ```

   If you use your own fork or private repo, clone that and adjust paths below.

2. **Create config directory and production config:**

   ```bash
   mkdir -p config
   ```

   Copy the production template and edit it with your domain and secrets:

   ```bash
   cp config/config.production.template.json config/config.json
   nano config/config.json   # or vim
   ```

   Set at least:

   - **`domain.base_domain`** – Your domain (e.g. `chat.yourdomain.com`) or the EC2 public IP for testing (e.g. `ec2-1-2-3-4.compute-1.amazonaws.com`).
   - **`domain.public_scheme`** – `"https"` if you will use a reverse proxy with TLS; `"http"` for first test with IP.
   - **`domain.public_port`** – `443` for HTTPS, or `80` / `8080` for HTTP.
   - **`database`** – Use Cassandra (and run Cassandra, e.g. from [fluxer_devops/cassandra/compose.yaml](../fluxer_devops/cassandra/compose.yaml)) or, if your setup supports it, SQLite for a minimal single-node.
   - **Secrets** – Replace every `GENERATE_A_64_CHAR_HEX_SECRET` and similar placeholder with random values (e.g. `openssl rand -hex 32`).
   - **`internal.kv`** – Must point to Valkey. With the root [compose.yaml](../compose.yaml) use `redis://valkey:6379/0`.
   - **`services.server.port`** – `8080` to match the compose mapping.

   Save and exit.

3. **Start the stack:**

   From the repo root on the server:

   ```bash
   docker compose up -d
   ```

   This starts Valkey and the Fluxer server (see [compose.yaml](../compose.yaml)). Check:

   ```bash
   docker compose ps
   curl -s http://127.0.0.1:8080/_health
   ```

   If you see a healthy response, the API is running.

---

## Part 4: Point your domain at the server (optional)

1. In your **domain registrar** or **DNS** (e.g. Route 53, Cloudflare, your registrar’s DNS):
   - Add an **A record**: name `chat` (or whatever subdomain you use), value = your EC2 **Elastic IP** (or public IP).

2. **HTTPS with Caddy (recommended):**
   - On the EC2 instance, install Caddy and proxy HTTP/HTTPS to `http://127.0.0.1:8080`.
   - Caddy will obtain a Let’s Encrypt certificate for your domain.
   - Example Caddyfile (adjust domain and upstream):

     ```text
     chat.yourdomain.com {
         reverse_proxy 127.0.0.1:8080
     }
     ```

   Then set in `config.json`: `domain.base_domain` = `chat.yourdomain.com`, `public_scheme` = `"https"`, `public_port` = `443`.

3. If you keep using the EC2 IP and port 8080, open in the browser: `http://YOUR_EC2_IP:8080` (and ensure security group allows TCP 8080).

---

## Part 5: Full stack (gateway, app proxy, NATS)

The root [compose.yaml](../compose.yaml) runs only **fluxer_server** and **Valkey**. For the full chat experience (real-time gateway, web app) you also need:

- **Gateway** (Erlang)
- **NATS**
- **App proxy** (or a reverse proxy serving the built frontend and proxying `/api` and `/gateway`)

The official [Fluxer self-hosting guide](https://docs.fluxer.app/self-hosting) describes the full architecture. You can:

- Run those components on the same EC2 instance (e.g. 2 GB RAM for a small instance), or
- Use the repo’s Dockerfiles and deploy workflows as reference to add gateway, NATS, and app proxy to your compose or run them as separate services.

---

## Troubleshooting

- **Cannot SSH:** Check security group allows port 22 from your IP; check the key pair and `chmod 400` on the `.pem` file.
- **API not reachable:** Open port 8080 (and 80/443 if using Caddy) in the instance security group; confirm `docker compose ps` shows the server running and `curl http://127.0.0.1:8080/_health` works.
- **Config errors:** Ensure `config/config.json` is valid JSON and paths (e.g. Valkey, database) match the services you run (e.g. `valkey` hostname in compose).

---

## Summary

| Step | What you do |
|------|-------------|
| 1 | Create EC2 (Ubuntu 22.04, t2.micro), key pair, security group (22, 80, 443, 8080). |
| 2 | (Optional) Allocate and associate an Elastic IP. |
| 3 | SSH in and run `scripts/aws-ec2-bootstrap.sh` to install Docker. |
| 4 | Clone Fluxer, create `config/config.json` from production template, set domain and secrets. |
| 5 | Run `docker compose up -d` and check `/_health`. |
| 6 | Point DNS A record to the Elastic IP; add Caddy for HTTPS if desired. |

You never need to share AWS credentials; you do everything from your own machine and the EC2 instance.
