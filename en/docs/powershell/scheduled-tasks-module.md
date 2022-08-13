---
title: "Scheduled tasks module"
slug: "scheduled-tasks-module"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Examples of how to use the Scheduled Tasks module available in Windows 8/Server 2012 and on.

## Run PowerShell Script in Scheduled Task
Creates a scheduled task that executes immediately, then on start up to run `C:\myscript.ps1` as `SYSTEM`
```
$ScheduledTaskPrincipal = New-ScheduledTaskPrincipal -UserId "SYSTEM" -LogonType ServiceAccount
$ScheduledTaskTrigger1 = New-ScheduledTaskTrigger -AtStartup
$ScheduledTaskTrigger2 = New-ScheduledTaskTrigger -Once -At $(Get-Date) -RepetitionInterval "00:01:00" -RepetitionDuration $([timeSpan] "24855.03:14:07")
$ScheduledTaskActionParams = @{
    Execute = "PowerShell.exe" 
    Argument = '-executionpolicy Bypass -NonInteractive -c C:\myscript.ps1 -verbose >>  C:\output.log 2>&1"'
}
$ScheduledTaskAction = New-ScheduledTaskAction @ScheduledTaskActionParams
Register-ScheduledTask -Principal $ScheduledTaskPrincipal -Trigger @($ScheduledTaskTrigger1,$ScheduledTaskTrigger2) -TaskName "Example Task" -Action $ScheduledTaskAction
```

