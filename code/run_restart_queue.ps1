# Sequential restart queue (2026-11-14): waits for snapshot worker B to finish so that at most
# two heavy jobs run at once on the 8 GB machine, then runs each job one at a time.
Set-Location (Split-Path $PSScriptRoot -Parent)
$log = "logs\restart_queue.log"
"$(Get-Date -Format s) queue started; waiting for worker B" | Out-File $log -Append -Encoding utf8
$b = Get-CimInstance Win32_Process -Filter "name='python.exe'" | Where-Object { $_.CommandLine -match '01_download_snapshots.py 20180508' }
if ($b) { Wait-Process -Id $b.ProcessId }
$jobs = @(
  @('code\05_flood_assign.py','matthew','addressnc'),
  @('code\05_flood_assign.py','helene','addressnc'),
  @('code\05_flood_assign.py','florence','addressnc','raw'),
  @('code\05_flood_assign.py','matthew','addressnc','raw'),
  @('code\01_download_snapshots.py','20230912','20250101','20260303','20190101','20200101','20171010')
)
foreach ($j in $jobs) {
  "$(Get-Date -Format s) START $($j -join ' ')" | Out-File $log -Append -Encoding utf8
  & py -I @j *>> "logs\restart_queue_jobs.log"
  "$(Get-Date -Format s) END exit=$LASTEXITCODE $($j -join ' ')" | Out-File $log -Append -Encoding utf8
}
"$(Get-Date -Format s) queue finished" | Out-File $log -Append -Encoding utf8
