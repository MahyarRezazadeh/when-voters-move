# Geocode queue (2026-11-16): waits for the May 2024 test run (cmd pid given as argument),
# proceeds only if that run wrote its summary, then geocodes the remaining baselines one at a
# time (8 GB machine; one DuckDB job at a time leaves room for the OS).
param([int]$WaitPid)
Set-Location (Split-Path $PSScriptRoot -Parent)
$log = "logs\geocode_queue.log"
"$(Get-Date -Format s) queue started; waiting for pid $WaitPid" | Out-File $log -Append -Encoding utf8
if ($WaitPid) { try { Wait-Process -Id $WaitPid -ErrorAction Stop } catch {} }
if (-not (Select-String -Path "logs\geocode_20240514.log" -Pattern '"seconds"' -Quiet)) {
  "$(Get-Date -Format s) ABORT: 20240514 run did not finish cleanly" | Out-File $log -Append -Encoding utf8
  exit 1
}
foreach ($d in @('20180508','20230101','20170101','20220517','20150101','20210101','20160607')) {
  "$(Get-Date -Format s) START $d" | Out-File $log -Append -Encoding utf8
  & py -I code\04_geocode.py $d *> "logs\geocode_$d.log"
  "$(Get-Date -Format s) END exit=$LASTEXITCODE $d" | Out-File $log -Append -Encoding utf8
}
"$(Get-Date -Format s) queue finished" | Out-File $log -Append -Encoding utf8
