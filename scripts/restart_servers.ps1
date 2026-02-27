# Restart Fluxer dev servers: kill entire process trees on dev ports, then start the stack.
# This prevents old Node/pnpm processes from staying in Task Manager and using RAM.
# Usage (from repo root): pnpm dev:restart
# Or: powershell -ExecutionPolicy Bypass -File scripts/restart_servers.ps1

$ErrorActionPreference = "SilentlyContinue"
$ports = @(48763, 49319, 49427, 49531, 49107)
$root = (Get-Item $PSScriptRoot).Parent.FullName

Write-Host "Stopping dev servers on ports $($ports -join ', ') (killing full process trees)..."

# Collect all PIDs listening on our ports (Get-NetTCPConnection + netstat for reliability)
$pidsOnPorts = [System.Collections.Generic.HashSet[int]]::new()
foreach ($p in $ports) {
    $conn = Get-NetTCPConnection -LocalPort $p -State Listen -ErrorAction SilentlyContinue | Select-Object -First 1
    if ($conn -and $conn.OwningProcess -ne 0) { [void]$pidsOnPorts.Add($conn.OwningProcess) }
}
foreach ($p in $ports) {
    try {
        $matches = netstat -ano | Select-String ":$p\s+.*LISTENING"
        foreach ($m in $matches) {
            $str = $m.Line -replace '\s+', ' '
            $parts = $str.Trim().Split(' ')
            $pidStr = $parts[-1]
            if ($pidStr -match '^\d+$' -and $pidStr -ne '0') { [void]$pidsOnPorts.Add([int]$pidStr) }
        }
    } catch { }
}

# Walk up parent chain to find root PID (so we kill the whole dev stack, not just one child)
function Get-RootProcessId {
    param([int]$ChildPid)
    $visited = [System.Collections.Generic.HashSet[int]]::new()
    $current = $ChildPid
    $maxDepth = 20
    $depth = 0
    while ($current -gt 0 -and $depth -lt $maxDepth) {
        if ($visited.Contains($current)) { break }
        [void]$visited.Add($current)
        $proc = Get-CimInstance Win32_Process -Filter "ProcessId = $current" -ErrorAction SilentlyContinue
        if (-not $proc) { return $current }
        $parent = $proc.ParentProcessId
        if ($parent -eq 0 -or -not $parent) { return $current }
        $current = $parent
        $depth++
    }
    return $current
}

# Don't kill the process that's running this script or its ancestors
$myAncestors = [System.Collections.Generic.HashSet[int]]::new()
$cur = $PID
$depth = 0
while ($cur -gt 0 -and $depth -lt 15) {
    [void]$myAncestors.Add($cur)
    $proc = Get-CimInstance Win32_Process -Filter "ProcessId = $cur" -ErrorAction SilentlyContinue
    if (-not $proc -or -not $proc.ParentProcessId) { break }
    $cur = $proc.ParentProcessId
    $depth++
}

$rootsToKill = [System.Collections.Generic.HashSet[int]]::new()
foreach ($pidVal in $pidsOnPorts) {
    $rootPid = Get-RootProcessId -ChildPid $pidVal
    if ($rootPid -gt 0 -and -not $myAncestors.Contains($rootPid)) { [void]$rootsToKill.Add($rootPid) }
}

foreach ($rootPid in $rootsToKill) {
    try {
        # /T = kill process tree (all descendants); /F = force
        $null = cmd /c "taskkill /F /T /PID $rootPid 2>nul"
        Write-Host "  Killed process tree (root PID $rootPid)"
    } catch { }
}

# Brief pause so OS releases ports and frees RAM before we start new processes
Start-Sleep -Seconds 3

Write-Host "Starting dev servers... Open http://localhost:48763 when ready.`n"
Set-Location $root
& pnpm dev:servers
