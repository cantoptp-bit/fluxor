# Kill processes listening on Fluxer dev stack ports
$ports = 49319, 49427, 49531, 48763, 8082
foreach ($p in $ports) {
    try {
        $conn = Get-NetTCPConnection -LocalPort $p -ErrorAction SilentlyContinue | Where-Object { $_.State -eq 'Listen' }
        if ($conn) {
            $pid = $conn.OwningProcess | Select-Object -First 1
            if ($pid) {
                Stop-Process -Id $pid -Force -ErrorAction SilentlyContinue
                Write-Host "Killed PID $pid on port $p"
            }
        }
    } catch {}
}
Write-Host "Port cleanup done."
