# Cohort build queue (2026-11-16): one 06_build_cohort.py job at a time (8 GB machine).
# Windows follow ANALYSIS_PLAN_2026-11-14.md Section 2. Post-storm outcomes are built here but
# not tabulated until the MDE from pre-storm cohorts is recorded (plan Section 8).
Set-Location (Split-Path $PSScriptRoot -Parent)
$log = "logs\cohort_queue.log"
"$(Get-Date -Format s) queue started" | Out-File $log -Append -Encoding utf8
$jobs = @(
  @('florence','20150101','20160607'),
  @('florence','20170101','20180508'),
  @('helene','20210101','20220517'),
  @('helene','20220517','20230912'),
  @('helene','20230101','20240514'),
  @('florence','20180508','20181106','20190101','20190910','20200101','20201103'),
  @('helene','20240514','20241105','20250101','20250909','20260303','20261103'),
  @('matthew','20160607','20171010')
)
foreach ($j in $jobs) {
  if (Test-Path "C:\voting_data\processed\cohort_$($j[0])_$($j[1]).parquet") {
    "$(Get-Date -Format s) SKIP (exists) $($j -join ' ')" | Out-File $log -Append -Encoding utf8; continue }
  "$(Get-Date -Format s) START $($j -join ' ')" | Out-File $log -Append -Encoding utf8
  & py -I code\06_build_cohort.py @j *>> "logs\cohort_jobs.log"
  "$(Get-Date -Format s) END exit=$LASTEXITCODE $($j -join ' ')" | Out-File $log -Append -Encoding utf8
}
"$(Get-Date -Format s) queue finished" | Out-File $log -Append -Encoding utf8
