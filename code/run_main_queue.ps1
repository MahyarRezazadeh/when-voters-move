# Main estimation queue: one R job at a time (8 GB RAM). Log: logs/main_queue.log
Set-Location (Split-Path $PSScriptRoot -Parent)
$R = "C:\Program Files\R\R-4.6.1\bin\Rscript.exe"
$jobs = @(@("florence","500"), @("helene","500"),
          @("florence","250"), @("helene","250"), @("florence","1000"), @("helene","1000"),
          @("florence","2000"), @("helene","2000"))
foreach ($j in $jobs) {
  $log = "logs\main_$($j[0])_bw$($j[1]).log"
  "$(Get-Date -Format s) start $($j -join ' ')" | Add-Content logs\main_queue.log
  & $R code\07_estimate.R $j[0] main $j[1] *> $log
  "$(Get-Date -Format s) exit $LASTEXITCODE $($j -join ' ')" | Add-Content logs\main_queue.log
}
& $R code\07_estimate.R combine *> logs\main_combine.log
& $R code\plot_event_study.R *> logs\plot_event_study.log
"$(Get-Date -Format s) done" | Add-Content logs\main_queue.log
