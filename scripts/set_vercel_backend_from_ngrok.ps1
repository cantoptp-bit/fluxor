# Set FLUXER_PUBLIC_DOMAIN in Vercel to your current ngrok hostname.
# Prereqs: ngrok running (e.g. "npx ngrok http 48763") and Vercel CLI linked.
# Usage: .\scripts\set_vercel_backend_from_ngrok.ps1

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
Write-Host "Done. Refresh your Vercel site (no redeploy needed)." -ForegroundColor Green
