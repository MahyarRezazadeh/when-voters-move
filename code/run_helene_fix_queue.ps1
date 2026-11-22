# Rebuild the Helene storm cohort after the status_cd padding fix, then re-run Helene main
# estimates, combine and figures. One job at a time. Log: logs/helene_fix_queue.log
Set-Location (Split-Path $PSScriptRoot -Parent)
$R = "C:\Program Files\R\R-4.6.1\bin\Rscript.exe"
$L = "logs\helene_fix_queue.log"
function Log($m) { "$(Get-Date -Format s) $m" | Out-File -Append -Encoding ascii $L }
$old = "C:\voting_data\processed\cohort_helene_20240514.parquet"
if (Test-Path $old) { Move-Item $old "C:\voting_data\processed\cohort_helene_20240514_statusbug.parquet" -Force }
Log "start cohort helene 20240514"
py -I code\06_build_cohort.py helene 20240514 20241105 20250101 20250909 20260303 20261103 *> logs\cohort_helene_20240514_fix.log
Log "exit $LASTEXITCODE cohort"
if (-not (Test-Path $old)) { Log "cohort file missing; stop"; exit 1 }
foreach ($bw in "500", "250", "1000", "2000") {
  Log "start helene $bw"
  & $R code\07_estimate.R helene main $bw *> "logs\main_helene_bw$bw.log"
  Log "exit $LASTEXITCODE helene $bw"
}
& $R code\07_estimate.R combine *> logs\main_combine.log
Log "exit $LASTEXITCODE combine"
& $R code\plot_event_study.R *> logs\plot_event_study.log
Log "exit $LASTEXITCODE plot"
Log "done"
