# Ensure Docker Desktop is running. Start it if needed and wait for the daemon.
# Usage: .\scripts\ensure_docker.ps1
# Used by IDE tasks so "Full Dev" starts Docker automatically.

$ErrorActionPreference = "Stop"
$Root = Split-Path -Parent $PSScriptRoot
Set-Location $Root

function Test-DockerReady {
    try {
        $null = & docker info 2>&1
        return $LASTEXITCODE -eq 0
    } catch {
        return $false
    }
}

if (Test-DockerReady) {
    Write-Host "Docker is already running." -ForegroundColor Green
    exit 0
}

Write-Host "Starting Docker Desktop..." -ForegroundColor Yellow

# Try multiple ways to start Docker so something works even if path or CLI differs
$started = $false
$exePaths = @(
    "C:\Program Files\Docker\Docker\Docker Desktop.exe",
    "$env:ProgramFiles\Docker\Docker\Docker Desktop.exe",
    "${env:ProgramFiles(x86)}\Docker\Docker\Docker Desktop.exe"
)

# 1. Prefer Docker Desktop CLI if available (4.37+)
try {
    $out = & docker desktop start 2>&1
    if ($LASTEXITCODE -eq 0) { $started = $true }
} catch { }

# 2. Launch the executable (visible window so you see it start)
if (-not $started) {
    foreach ($exe in $exePaths) {
        if (Test-Path $exe) {
            Write-Host "  Launching: $exe" -ForegroundColor Gray
            Start-Process -FilePath $exe -WindowStyle Normal
            $started = $true
            break
        }
    }
}

if (-not $started) {
    Write-Host "Docker Desktop not found. Install it from https://www.docker.com/products/docker-desktop/" -ForegroundColor Red
    Write-Host "If it is installed but won't start, see docs/DOCKER_TROUBLESHOOTING.md" -ForegroundColor Yellow
    exit 1
}

# Wait for daemon (up to 60 seconds)
$maxWait = 60
$elapsed = 0
while (-not (Test-DockerReady)) {
    Start-Sleep -Seconds 2
    $elapsed += 2
    if ($elapsed -ge $maxWait) {
        Write-Host "Docker did not become ready in time." -ForegroundColor Red
        Write-Host "If Docker never starts, see docs/DOCKER_TROUBLESHOOTING.md" -ForegroundColor Yellow
        exit 1
    }
    Write-Host "  Waiting for Docker... ($elapsed s)" -ForegroundColor Gray
}

Write-Host "Docker is ready." -ForegroundColor Green
exit 0
