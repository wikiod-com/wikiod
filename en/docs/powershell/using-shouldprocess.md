---
title: "Using ShouldProcess"
slug: "using-shouldprocess"
draft: false
images: []
weight: 9823
type: docs
toc: true
---

## Syntax
- $PSCmdlet.ShouldProcess("Target")
- $PSCmdlet.ShouldProcess("Target", "Action")



## Parameters
| Parameter | Details |
| --------- | ------- |
| Target    | The resource being changed. |
| Action    | The operation being performed. Defaults to the name of the cmdlet.

`$PSCmdlet.ShouldProcess()` will also automatically write a message to the verbose output.

```
PS> Invoke-MyCmdlet -Verbose
VERBOSE: Performing the operation "Invoke-MyCmdlet" on target "Target of action"
```

## Full Usage Example 
Other examples couldn't clearly explain to me how to trigger the conditional logic.

This example also shows that underlying commands will also listen to the -Confirm flag!

    <#
    Restart-Win32Computer
    #>

    function Restart-Win32Computer 
    {
        [CmdletBinding(SupportsShouldProcess=$true,ConfirmImpact="High")]
        param (
        [parameter(Mandatory=$true,ValueFromPipeline=$true,ValueFromPipelineByPropertyName=$true)]
        [string[]]$computerName,
        [parameter(Mandatory=$true)]
        [string][ValidateSet("Restart","LogOff","Shutdown","PowerOff")] $action,
        [boolean]$force = $false
    )
    BEGIN {
    # translate action to numeric value required by the method
    switch($action) {
        "Restart"
        {
            $_action = 2
            break
        }
        "LogOff"
        {
            $_action = 0
            break
        }
        "Shutdown"
        {
            $_action = 2
            break
        }
        "PowerOff"
        {
            $_action = 8
            break
        }
    }
    # to force, add 4 to the value
    if($force) 
    {
        $_action += 4
    }
    write-verbose "Action set to $action"
    }
    PROCESS {
        write-verbose "Attempting to connect to $computername"
        # this is how we support -whatif and -confirm
        # which are enabled by the SupportsShouldProcess
        # parameter in the cmdlet bindnig
        if($pscmdlet.ShouldProcess($computername)) {
            get-wmiobject win32_operatingsystem -computername $computername | invoke-wmimethod -name Win32Shutdown -argumentlist $_action
        }
    }
    } 
    #Usage:
    #This will only output a description of the actions that this command would execute if -WhatIf is removed.
    'localhost','server1'| Restart-Win32Computer -action LogOff -whatif 

    #This will request the permission of the caller to continue with this item.
    #Attention: in this example you will get two confirmation request because all cmdlets called by this cmdlet that also support ShouldProcess, will ask for their own confirmations...
    'localhost','server1'| Restart-Win32Computer -action LogOff -Confirm
    




## Adding -WhatIf and -Confirm support to your cmdlet
    function Invoke-MyCmdlet {
        [CmdletBinding(SupportsShouldProcess = $true)]
        param()
        # ...
    }

## Using ShouldProcess() with one argument
    if ($PSCmdlet.ShouldProcess("Target of action")) {
        # Do the thing
    }

When using `-WhatIf`:

`What if: Performing the action "Invoke-MyCmdlet" on target "Target of action"`

When using `-Confirm`:

```Confirm
Are you sure you want to perform this action?
Performing operation "Invoke-MyCmdlet" on target "Target of action"
[Y] Yes  [A] Yes to All  [N] No  [L] No to All  [S] Suspend  [?] Help (default is "Y"):
```

