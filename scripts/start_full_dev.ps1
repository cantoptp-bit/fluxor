# Start the full Fluxer dev stack: Docker, Caddy, and dev servers
# Usage: .\scripts\start_full_dev.ps1
# Then open http://localhost:48763
# Ctrl+C stops dev servers; Docker and Caddy keep running

$ErrorActionPreference = "Stop"
$Root = Split-Path -Parent $PSScriptRoot
Set-Location $Root

Write-Host "Starting Fluxer dev stack..." -ForegroundColor Cyan

# 0. Ensure Docker Desktop is running (start it if needed)
& "$PSScriptRoot/ensure_docker.ps1" 2>&1 | Out-Null
if (-not $?) {
    Write-Host "  Warning: Docker may not be ready. Start Docker Desktop manually if compose fails." -ForegroundColor Yellow
}

# 1. Docker backing services (Valkey, Meilisearch, NATS, Mailpit for dev emails)
Write-Host "`n[1/3] Docker backing services (Valkey, Meilisearch, NATS, Mailpit)..." -ForegroundColor Yellow
try {
    docker compose -f compose.dev.yaml up -d 2>&1 | Out-Null
    Write-Host "  Done (emails at http://localhost:8025)" -ForegroundColor Green
} catch {
    Write-Host "  Warning: Docker failed. Ensure Docker is running." -ForegroundColor Red
}

# 2. Reverse proxy on 48763 (Caddy if installed, otherwise Node app proxy)
Write-Host "`n[2/3] Reverse proxy on 48763..." -ForegroundColor Yellow
$caddyPath = $null
foreach ($p in @(
    "$env:ProgramFiles\Caddy\caddy.exe",
    "${env:ProgramFiles(x86)}\Caddy\caddy.exe",
    "$env:LOCALAPPDATA\Programs\caddy\caddy.exe"
)) {
    if (Test-Path $p) {
        $caddyPath = $p
        break
    }
}
if (-not $caddyPath) {
    $cmd = Get-Command caddy -ErrorAction SilentlyContinue
    if ($cmd) { $caddyPath = $cmd.Source }
}
if ($caddyPath) {
    Start-Process -FilePath $caddyPath -ArgumentList "run", "--config", "dev/Caddyfile.dev", "--adapter", "caddyfile" -WorkingDirectory $Root -WindowStyle Hidden
    Write-Host "  Started (Caddy)" -ForegroundColor Green
} else {
    Start-Process -FilePath "node" -ArgumentList "scripts/start_proxy.mjs" -WorkingDirectory $Root -WindowStyle Hidden -PassThru | Out-Null
    Start-Sleep -Seconds 2
    Write-Host "  Started (Node proxy - no Caddy needed)" -ForegroundColor Green
}

# 3. Dev servers (runs in foreground; Ctrl+C stops)
Write-Host "`n[3/3] Dev servers (fluxer_server, fluxer_app, marketing)..." -ForegroundColor Yellow
Write-Host "  Open http://localhost:48763 when ready. Stop with Ctrl+C.`n" -ForegroundColor Green
& node scripts/start_dev.mjs
