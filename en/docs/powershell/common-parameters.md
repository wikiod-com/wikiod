---
title: "Common parameters"
slug: "common-parameters"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Common parameters can be used with any cmdlet (that means as soon as you mark your function as cmdlet [see `CmdletBinding()`], you get all of these parameters for free).  

Here is the list of all common parameters (alias is in parenthesis after corresponding parameter):

    -Debug (db)
    -ErrorAction (ea)
    -ErrorVariable (ev)
    -InformationAction (ia) # introduced in v5
    -InformationVariable (iv) # introduced in v5
    -OutVariable (ov)
    -OutBuffer (ob)
    -PipelineVariable (pv)
    -Verbose (vb) 
    -WarningAction (wa)
    -WarningVariable (wv)
    -WhatIf (wi)
    -Confirm (cf)

## ErrorAction parameter
Possible values are `Continue | Ignore | Inquire | SilentlyContinue | Stop | Suspend`.

Value of this parameter will determine how the cmdlet will handle non-terminating errors (those generated from Write-Error for example; to learn more about error handling see [*topic not yet created*]).

Default value (if this parameter is omitted) is `Continue`.

# -ErrorAction Continue #

This option will produce an error message and will continue with execution.

    PS C:\> Write-Error "test" -ErrorAction Continue ; Write-Host "Second command"
[![-ErorrAction Continue][1]][1]

# -ErrorAction Ignore #

This option will not produce any error message and will continue with execution. Also no errors will be added to `$Error` automatic variable.  
This option was introduced in v3.

    PS C:\> Write-Error "test" -ErrorAction Ignore ; Write-Host "Second command"
[![-ErorrAction Ignore][2]][2]

# -ErrorAction Inquire #

This option will produce an error message and will prompt user to choose an action to take.

    PS C:\> Write-Error "test" -ErrorAction Inquire ; Write-Host "Second command"
[![-ErorrAction Inquire][3]][3]

# -ErrorAction SilentlyContinue #

This option will not produce an error message and will continue with execution. All errors will be added to `$Error` automatic variable.  

    PS C:\> Write-Error "test" -ErrorAction SilentlyContinue ; Write-Host "Second command"
[![-ErorrAction SilentlyContinue][4]][4]

# -ErrorAction Stop #

This option will produce an error message and will not continue with execution.

    PS C:\> Write-Error "test" -ErrorAction Stop ; Write-Host "Second command"
[![-ErorrAction Stop][5]][5]


# -ErrorAction Suspend #

Only available in Powershell Workflows. When used, if the command runs into an error, the workflow is suspended. This allows investigation of such error and gives a possibility to resume the workflow. To learn more about Workflow system, see [topic not yet created].

  [1]: http://i.stack.imgur.com/r9jzQ.png
  [2]: http://i.stack.imgur.com/sLtQW.png
  [3]: http://i.stack.imgur.com/ewOoW.png
  [4]: http://i.stack.imgur.com/cfTx7.png
  [5]: http://i.stack.imgur.com/50WP7.png

