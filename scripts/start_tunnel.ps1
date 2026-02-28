# Start Cloudflare Tunnel for home.auroraplayer.com
#
# Prerequisites:
# - cloudflared installed (winget install cloudflare.cloudflared)
# - Dev stack: pnpm dev:servers in one terminal, pnpm dev:proxy in another (proxy on 48763)
# - Docker backing services running
#
# One-time setup for named tunnel (home.auroraplayer.com):
#   1. cloudflared tunnel login
#   2. cloudflared tunnel create fluxor
#   3. cloudflared tunnel route dns fluxor home.auroraplayer.com
#   4. Copy dev/cloudflared-config.yml.example to dev/cloudflared-config.yml
#   5. Edit dev/cloudflared-config.yml: set tunnel UUID and credentials-file path
#
# Usage:
#   .\scripts\start_tunnel.ps1           # named tunnel (foreground)
#   .\scripts\start_tunnel.ps1 -Background # named tunnel (background)
#   .\scripts\start_tunnel.ps1 -Quick     # quick tunnel (random URL)

param(
    [switch]$Quick,
    [switch]$Background
)

$ErrorActionPreference = "Stop"
$Root = Split-Path -Parent $PSScriptRoot
$configPath = Join-Path $Root "dev\cloudflared-config.yml"

if ($Quick) {
    Write-Host "Starting Cloudflare quick tunnel -> http://127.0.0.1:48763" -ForegroundColor Cyan
    Write-Host "Ensure something is running on 48763 (pnpm dev:proxy). You get a random trycloudflare.com URL.`n" -ForegroundColor Yellow
    cloudflared tunnel --url http://127.0.0.1:48763
    exit
}

if (-not (Test-Path $configPath)) {
    Write-Host "Config not found: $configPath" -ForegroundColor Red
    Write-Host ""
    Write-Host "For https://home.auroraplayer.com do one-time setup:"
    Write-Host "  1. cloudflared tunnel login"
    Write-Host "  2. cloudflared tunnel create fluxor"
    Write-Host "  3. cloudflared tunnel route dns fluxor home.auroraplayer.com"
    Write-Host "  4. Copy dev/cloudflared-config.yml.example to dev/cloudflared-config.yml"
    Write-Host "  5. Edit dev/cloudflared-config.yml: set tunnel UUID and credentials-file path"
    Write-Host ""
    Write-Host "  See dev/TUNNEL_DNS_SETUP.md for the full tunnel + DNS steps."
    Write-Host ""
    Write-Host "Then run: Terminal 1: pnpm dev:servers   Terminal 2: pnpm dev:proxy   Terminal 3: .\scripts\start_tunnel.ps1"
    exit 1
}

Write-Host "Tunnel target: http://127.0.0.1:48763 (ensure pnpm dev:proxy is running)" -ForegroundColor Gray
Write-Host "If you see Error 1033 in the browser: cloudflared is not connected. Run this script without -Background to see errors." -ForegroundColor Gray
Write-Host "Starting Cloudflare Tunnel -> https://home.auroraplayer.com" -ForegroundColor Cyan

# cloudflared requires: tunnel --config <path> run <name>  (--config must come before "run")
if ($Background) {
    Start-Process -FilePath "cloudflared" -ArgumentList "tunnel", "--config", $configPath, "run", "fluxor" -WorkingDirectory $Root -WindowStyle Hidden
    Write-Host "Tunnel started in background. Open https://home.auroraplayer.com" -ForegroundColor Green
} else {
    & cloudflared tunnel --config $configPath run fluxor
}
