---
title: "PowerShell Background Jobs"
slug: "powershell-background-jobs"
draft: false
images: []
weight: 9924
type: docs
toc: true
---

Jobs were introduced in PowerShell 2.0 and helped to solve a problem inherent in the command-line tools. In a nutshell, if you start a long running task, your prompt is unavailable until the task finishes. As an example of a long running task, think of this simple PowerShell command:

*Get-ChildItem -Path c:\ -Recurse*

It will take a while to fetch full directory list of your C: drive.
If you run it as Job then the console will get the control back and you can capture the result later on.

PowerShell Jobs run in a new process.  This has pros and cons which are related.

Pros:

 1. The job runs in a clean process, including environment.
 1. The job can run asynchronously to your main PowerShell process


Cons:

 1. Process environment changes will not be present in the job.
 2. Parameters pass to and returned results are serialized.  
    - This means if you change a parameter object while the job is running it will not be reflected in the job.
    - This also means if an object cannot be serialized you cannot pass or return it (although PowerShell may Copy any parameters and pass/return a PSObject.)



## Basic job creation
Start a Script Block as background job:
    
    $job = Start-Job -ScriptBlock {Get-Process}

Start a script as background job:

    $job = Start-Job -FilePath "C:\YourFolder\Script.ps1"

Start a job using `Invoke-Command` on a remote machine:

    $job = Invoke-Command -ComputerName "ComputerName" -ScriptBlock {Get-Service winrm} -JobName "WinRM" -ThrottleLimit 16 -AsJob

Start job as a different user (Prompts for password): 

    Start-Job -ScriptBlock {Get-Process} -Credential "Domain\Username"
Or

    Start-Job -ScriptBlock {Get-Process} -Credential (Get-Credential)

Start job as a different user (No prompt): 

    $username = "Domain\Username" 
    $password = "password"
    $secPassword = ConvertTo-SecureString -String $password -AsPlainText -Force
    $credentials = New-Object System.Management.Automation.PSCredential -ArgumentList @($username, $secPassword)
    Start-Job -ScriptBlock {Get-Process} -Credential $credentials

## Basic job management
Get a list of all jobs in the current session:

    Get-Job

Waiting on a job to finish before getting the result:

    $job | Wait-job | Receive-Job 

Timeout a job if it runs too long (10 seconds in this example)

    $job | Wait-job -Timeout 10

Stopping a job (completes all tasks that are pending in that job queue before ending):

    $job | Stop-Job 

Remove job from current session's background jobs list:

    $job | Remove-Job

----------

**Note**: The following will only work on `Workflow` Jobs.

Suspend a `Workflow` Job (Pause):

    $job | Suspend-Job 

Resume a `Workflow` Job: 

    $job | Resume-Job 

