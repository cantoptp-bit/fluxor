# Start Fluxer dev stack and Cloudflare quick tunnel.
# Requires: Docker (for Valkey, Meilisearch, NATS), Caddy, cloudflared.
# Run from repo root: .\scripts\start_all_and_tunnel.ps1

$ErrorActionPreference = "Continue"
$RepoRoot = Split-Path -Parent (Split-Path -Parent $PSCommandPath)
Set-Location $RepoRoot

$env:FLUXER_CONFIG = Join-Path $RepoRoot "config\config.json"
$env:FORCE_COLOR = "1"
$env:FLUXER_APP_DEV_PORT = "49427"

Write-Host "=== Fluxer dev stack + Cloudflare tunnel ===" -ForegroundColor Cyan
Write-Host ""

# 1. Backing services (Docker)
Write-Host "[1/5] Backing services (Valkey, Meilisearch, NATS)..." -ForegroundColor Yellow
$docker = Get-Command docker -ErrorAction SilentlyContinue
if ($docker) {
    docker compose -f compose.dev.yaml up -d 2>&1
    if ($LASTEXITCODE -eq 0) {
        Write-Host "    Backing services started. Waiting 5s..." -ForegroundColor Green
        Start-Sleep -Seconds 5
    } else {
        Write-Host "    Docker compose failed. Backend may not start." -ForegroundColor Red
    }
} else {
    Write-Host "    Docker not found. Install Docker Desktop and run this script again." -ForegroundColor Red
    Write-Host "    Or run: docker compose -f compose.dev.yaml up -d" -ForegroundColor Gray
}

# 2. App servers (server, app, marketing)
Write-Host "[2/5] Starting fluxer_server, fluxer_app, fluxer_marketing..." -ForegroundColor Yellow
$serversJob = Start-Job -ScriptBlock {
    Set-Location $using:RepoRoot
    $env:FLUXER_CONFIG = $using:env:FLUXER_CONFIG
    $env:FORCE_COLOR = "1"
    $env:FLUXER_APP_DEV_PORT = "49427"
    pnpm dev:servers 2>&1
}
Write-Host "    Servers starting in background (Job $($serversJob.Id)). Waiting 15s..." -ForegroundColor Gray
Start-Sleep -Seconds 15

# 3. Caddy
Write-Host "[3/5] Starting Caddy on :48763..." -ForegroundColor Yellow
$caddy = Get-Command caddy -ErrorAction SilentlyContinue
if ($caddy) {
    $caddyJob = Start-Job -ScriptBlock {
        Set-Location $using:RepoRoot
        caddy run --config dev/Caddyfile.dev --adapter caddyfile 2>&1
    }
    Write-Host "    Caddy started (Job $($caddyJob.Id)). Waiting 3s..." -ForegroundColor Green
    Start-Sleep -Seconds 3
} else {
    Write-Host "    Caddy not in PATH. Install from https://caddyserver.com/docs/install" -ForegroundColor Red
    Write-Host "    Tunnel will point to 48763; start Caddy manually in another terminal." -ForegroundColor Gray
}

# 4. Cloudflare quick tunnel
Write-Host "[4/5] Starting Cloudflare quick tunnel to http://127.0.0.1:48763..." -ForegroundColor Yellow
$cloudflared = "C:\Program Files (x86)\cloudflared\cloudflared.exe"
if (-not (Test-Path $cloudflared)) { $cloudflared = "cloudflared" }
$tunnelJob = Start-Job -ScriptBlock {
    & $using:cloudflared tunnel --url http://127.0.0.1:48763 2>&1
}
Start-Sleep -Seconds 8

# 5. Show tunnel URL from job output
Write-Host "[5/5] Tunnel output:" -ForegroundColor Yellow
Receive-Job $tunnelJob | ForEach-Object { Write-Host $_ }
Write-Host ""
Write-Host "=== Share the HTTPS URL above (trycloudflare.com) with others. ===" -ForegroundColor Green
Write-Host "Press Enter to stop all jobs and exit..."
Read-Host

Get-Job | Stop-Job
Get-Job | Remove-Job
