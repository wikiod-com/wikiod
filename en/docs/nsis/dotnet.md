---
title: "DotNET"
slug: "dotnet"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

When the .NET Framework is being installed on a computer the .NET installer writes registry keys when installation is successful. You can test whether the .NET Framework 4.5 or later is installed by checking the registry key `HKLM\SOFTWARE\Microsoft\NET Framework Setup\NDP\v4\Full` for a `DWORD` value named _Release_. The existence of this key indicates that the .NET Framework 4.5 or later has been installed on that computer.

[.NET Values][2]
================
Below is a chart that shows the versions number corresponding `DWORD` value in the _Release_ key. See below this chart for an example on how to use this in action. I will do my best to try and keep this page up-to-date with the latest values but you can visit [this page](https://msdn.microsoft.com/en-us/library/hh925568(v=vs.110).aspx) for an updated list if the version you need isn't listed here.
>Take special notice how there are two rows for `4.7`, `4.6.2`, `4.6.1`, and `4.6`. These versions require you to check for both values as the operating systems vary.

| **Version** | **Operating System** | **Value of Release** |
| --- | --- | --- |
| 4.7 | Windows 10 Creator's Update | 460798 |
| 4.7 | All except Windows 10 Creator's Update | 460805 |
| 4.6.2 | Windows 10 Anniversary Update | 394802 |
| 4.6.2 | All except Windows 10 Anniversary Update | 394806 |
| 4.6.1 | Windows 10 November Update | 394254 |
| 4.6.1 | All except Windows 10 November Update | 394271 |
| 4.6 | Windows 10 | 393295 |
| 4.6 | All except Windows 10 | 393297 |
| 4.5.2 | All | 379893 |
| 4.5.1 | Windows 8.1 or Windows Server 2012 R2 | 378675 |
| 4.5.1 | Windows 8 and Wiindows 7 | 378758 |
| 4.5 | All | 378389 |

Conclusion
==========
>I haven't fully tested these functions completely so if you run into problems with one of them let me know and I'll do my best to help. You can see this function working in action with [SharpDevelop Portable][1].

Like I said above I will do my best to try and keep these up-to-date but if you need a version that these functions aren't checking for, let me know so I can revise them and update this page.

[1]:http://softables.tk/depository/development/SharpDevelop-Portable
[2]:http://softables.tk/docs/advanced/dotnet#values

## Sample Example
Here's an example on how to use the values in the registry to check for dotNET 4.5 or higher. I'd recommend putting this snippet of code somewhere like `.OnInit` as this will execute before anything else happens; this way it checks for .NET before any files get copied or registry changes take place.

```
;=#
;= Define the registry key we're looking for.
!define DOTNET  `SOFTWARE\Microsoft\NET Framework Setup\NDP\v4\Full`

Section "Main"
    ClearErrors ;= Clear any errors we may have encountered before hand.
    ReadRegDWORD $0 HKLM `${DOTNET}` `Release` ;= Read the value of Release.
    IfErrors +3 ;= If there is an error than there is no .NET installed so we jump 3 lines down which lands on MessageBox.
    IntCmp $0 394806 +5 0 +5    ;= Compares the value for v4.6.2 if it matches then we jump 5 lines and avoids the MessageBox
    IntCmp $0 394802 +4 0 +4    ;= Remember to check for Windows 10's value aswell as the above line won't.
    MessageBox MB_ICONSTOP|MB_TOPMOST `You must have v4.6.2 or greater of the .NET Framework installed. Launcher aborting!` ;= If the check failed then we alert the user the required version wasn't found.
    Call Unload ;= We call the Unload function here because we failed the .NET check.
    Quit ;= Closes the Launcher
SectionEnd
```

## Function w/ Macro

#### Function dotNETCheck

Alternatively you can use the function I wrote below. This one makes use of _LogicLib.nsh_. It should work out-of-the-box without having to know the .NET versions value from the _Release_ key in the registry. As it is written right now it only checks for versions between `4.5`&ndash;`4.7`.

----------

<!-- language: lang-nsis -->
``` 
##
# This one requires the use of LogicLib.nsh
# Copy and paste this code somewhere like .OnInit
Function dotNETCheck
    !define CheckDOTNET "!insertmacro _CheckDOTNET"
    !macro _CheckDOTNET _RESULT _VALUE
        Push `${_VALUE}`
        Call dotNETCheck
        Pop ${_RESULT}
    !macroend
    Exch $1
    Push $0
    Push $1
    
    ${If} $1 == "4.7"
        StrCpy $R1 460798
    ${ElseIf} $1 == "4.6.2"
        StrCpy $R1 394802
    ${ElseIf} $1 == "4.6.1"
        StrCpy $R1 394254
    ${ElseIf} $1 == "4.6"
        StrCpy $R1 393295
    ${ElseIf} $1 == "4.5.2"
        StrCpy $R1 379893
    ${ElseIf} $1 == "4.5.1"
        StrCpy $R1 378675
    ${ElseIf} $1 == "4.5"
        StrCpy $R1 378389
    ${Else}
        Goto dotNET_FALSE
    ${EndIf}
    
    ReadRegDWORD $R0 HKLM `SOFTWARE\Microsoft\NET Framework Setup\NDP\v4\Full` `Release`
    IfErrors dotNET_FALSE
    
    IntCmp $R0 $R1 dotNET_TRUE dotNET_FALSE
    
    dotNET_TRUE:
    StrCpy $0 true
    Goto dotNET_END
    
    dotNET_FALSE:
    StrCpy $0 false
    SetErrors
    
    dotNET_END:    
    Pop $1
    Exch $0
FunctionEnd

##
# USAGE
# ${CheckDOTNET} $0 "Version Number"
##
# $0 Will hold the version number of the installed .NET
# If $0 is empty ($0 == "") then the error flag is set.
${CheckDOTNET} $0 "4.5"
IfErrors 0 +4
MessageBox MB_ICONSTOP|MB_TOPMOST `You must have v4.5 or greater of the .NET Framework installed. Launcher aborting!`
Call Unload
Quit
StrCmpS $0 true 0 -3
```

