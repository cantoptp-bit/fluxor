# Set FLUXER_PUBLIC_DOMAIN in Vercel to your current ngrok hostname so the
# Vercel-deployed app can reach your local backend. Uses runtime /api/fluxer-config.
#
# Prereqs:
#   1. Backend running: pnpm dev:servers (proxy on 48763)
#   2. Ngrok running:   npx ngrok http 48763
#   3. Vercel CLI linked (vercel link)
#
# Usage: .\scripts\set_vercel_backend_from_ngrok.ps1
#
# After running: refresh your Vercel site in the browser (no redeploy needed).
# If you still see "Connection Issue", redeploy once so api/fluxer-config.js is live.

$ErrorActionPreference = "Stop"
$Root = Split-Path -Parent $PSScriptRoot
Set-Location $Root

try {
    $tunnels = Invoke-RestMethod -Uri "http://127.0.0.1:4040/api/tunnels"
} catch {
    Write-Host "Ngrok API not reachable. Start ngrok first, e.g: npx ngrok http 48763" -ForegroundColor Red
    exit 1
}

$tunnel = $tunnels.tunnels | Where-Object { $_.public_url -match "^https://" } | Select-Object -First 1
if (-not $tunnel) {
    Write-Host "No HTTPS tunnel found." -ForegroundColor Red
    exit 1
}

$url = $tunnel.public_url
$hostname = ([System.Uri]$url).Host
Write-Host "Using hostname: $hostname" -ForegroundColor Cyan

npx vercel env add FLUXER_PUBLIC_DOMAIN production --value $hostname --yes --force
Write-Host "Done. Refresh your Vercel site in the browser." -ForegroundColor Green
Write-Host "If you still see Connection Issue, redeploy once so /api/fluxer-config is live." -ForegroundColor Yellow
