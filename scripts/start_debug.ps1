# Fluxer Debug Startup Script
# This script starts all services and the Python Debug Terminal

Write-Host "--- Fluxer Debug Startup ---" -ForegroundColor Cyan

# 1. Cleanup old processes
Write-Host "Cleaning up old processes on ports 8080, 48763, 49427, 8082..." -ForegroundColor Gray
Get-NetTCPConnection -LocalPort 8080, 48763, 49427, 8082 -ErrorAction SilentlyContinue | ForEach-Object { 
    try { Stop-Process -Id $_.OwningProcess -Force -ErrorAction SilentlyContinue } catch {}
}

# 2. Start Services
Write-Host "Starting fluxer_server..." -ForegroundColor Yellow
$serverJob = Start-Process powershell -ArgumentList "-NoExit", "-Command", "`$env:FLUXER_CONFIG='c:\Users\canto\Desktop\fluxor\config.json'; `$env:FLUXER_LITE_MODE='1'; pnpm --filter fluxer_server run dev" -WindowStyle Minimized -PassThru

Write-Host "Starting fluxer_app..." -ForegroundColor Yellow
$appJob = Start-Process powershell -ArgumentList "-NoExit", "-Command", "`$env:FLUXER_CONFIG='c:\Users\canto\Desktop\fluxor\config.json'; pnpm --filter fluxer_app run dev" -WindowStyle Minimized -PassThru

Write-Host "Starting fluxer_app_proxy..." -ForegroundColor Yellow
$proxyJob = Start-Process powershell -ArgumentList "-NoExit", "-Command", "`$env:FLUXER_CONFIG='c:\Users\canto\Desktop\fluxor\config.json'; pnpm --filter fluxer_app_proxy run dev" -WindowStyle Minimized -PassThru

# 3. Wait for services to be ready
Write-Host "Waiting for services to be ready..."
$portsToWait = @(49427, 48763, 8082)
foreach ($port in $portsToWait) {
    Write-Host "Checking port $port..."
    while (!(Test-NetConnection -ComputerName 127.0.0.1 -Port $port -InformationLevel Quiet)) {
        Start-Sleep -Seconds 1
    }
}

# 4. Start Python Debug Terminal in a new window
Write-Host "Launching Debug Terminal..." -ForegroundColor Green
Start-Process powershell -ArgumentList "-NoExit", "-Command", "python c:\Users\canto\Desktop\fluxor\scripts\debug_terminal.py"

# 5. Open Login Page
Write-Host "Opening Login Page..." -ForegroundColor Blue
Start-Process "http://localhost:48763/login"

Write-Host "--- Startup Complete ---" -ForegroundColor Green
Write-Host "All services are running in background windows."
Write-Host "Monitor logs in the active Debug Terminal window."
