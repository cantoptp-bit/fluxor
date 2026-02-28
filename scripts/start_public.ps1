# Start all Fluxer services and expose the site via Cloudflare quick tunnel
# so people not on your network can view it.
#
# Run from repo root: .\scripts\start_public.ps1
#
# You get two windows: (1) dev servers + proxy, (2) tunnel with the public HTTPS URL.
# Share the URL from the tunnel window (e.g. https://xxx.trycloudflare.com).

$ErrorActionPreference = "Stop"
$RepoRoot = Split-Path -Parent (Split-Path -Parent $PSCommandPath)
Set-Location $RepoRoot

$env:FLUXER_CONFIG = Join-Path $RepoRoot "config\config.json"
$env:FORCE_COLOR = "1"

Write-Host "=== Starting Fluxer for public access ===" -ForegroundColor Cyan

# 1. Docker backing services
Write-Host "`n[1/4] Docker backing services..." -ForegroundColor Yellow
$docker = Get-Command docker -ErrorAction SilentlyContinue
if ($docker) {
    $ErrorActionPreference = "Continue"
    docker compose -f compose.dev.yaml up -d 2>$null
    $ErrorActionPreference = "Stop"
    if ($LASTEXITCODE -eq 0) {
        Write-Host "    Done. Waiting 5s for Cassandra/Valkey/NATS/Meilisearch." -ForegroundColor Green
        Start-Sleep -Seconds 5
    } else {
        Write-Host "    Docker compose failed. Start Docker Desktop and run again." -ForegroundColor Red
        exit 1
    }
} else {
    Write-Host "    Docker not found. Install Docker Desktop." -ForegroundColor Red
    exit 1
}

# 2. Dev servers (proxy on 48763, API, app, marketing) in a new window
Write-Host "`n[2/4] Starting dev servers in new window (proxy + API + app)..." -ForegroundColor Yellow
Start-Process powershell -ArgumentList @(
    "-NoExit",
    "-Command",
    "Set-Location '$RepoRoot'; `$env:FLUXER_CONFIG='$env:FLUXER_CONFIG'; `$env:FORCE_COLOR='1'; pnpm dev:servers"
)
Write-Host "    Waiting for proxy on port 48763 (first compile can take 1-2 min)..." -ForegroundColor Gray

# 3. Wait for port 48763 (TCP connect; no admin required)
$maxWait = 180
$waited = 0
$portOpen = $false
while ($waited -lt $maxWait) {
    try {
        $tcp = New-Object System.Net.Sockets.TcpClient
        $tcp.Connect("127.0.0.1", 48763)
        $tcp.Close()
        $portOpen = $true
        break
    } catch {}
    Start-Sleep -Seconds 5
    $waited += 5
}
if (-not $portOpen) {
    Write-Host "    Port 48763 did not open in time. Check the dev servers window for errors." -ForegroundColor Red
    exit 1
}
Write-Host "    Proxy is up." -ForegroundColor Green

# 4. Start Cloudflare quick tunnel in a new window
$cloudflared = "C:\Program Files (x86)\cloudflared\cloudflared.exe"
if (-not (Test-Path $cloudflared)) { $cloudflared = "cloudflared" }
Write-Host "`n[4/4] Starting Cloudflare tunnel in new window..." -ForegroundColor Yellow
Start-Process powershell -ArgumentList @(
    "-NoExit",
    "-Command",
    "& '$cloudflared' tunnel --url http://127.0.0.1:48763"
)

Write-Host "`n=== Done ===" -ForegroundColor Green
Write-Host "  - Dev stack: see the first window (stop with Ctrl+C there)."
Write-Host "  - Public URL: see the second window (copy the https://...trycloudflare.com URL to share)."
Write-Host "  - Local: http://localhost:48763"
Write-Host ""
