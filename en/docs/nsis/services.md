---
title: "Services"
slug: "services"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

When installing a new program or updating an installation, it's good practice for you to stop an installed application and anything related with it before overwriting any of its files. The same goes for services. We need to be sure that the locally run service (if any) is stopped before we can install or upgrade our program. In this topic I'll share a few macros ranging from creating a service to querying a service to removing one all together. 

## Syntax
- ${Service::Create} "NAME" "PATH" "TYPE" "START" "DEPEND" /DISABLEFSR $0 $1
- ${Service::Remove} "NAME" "" $0 $1
- ${Service::QueryConfig} "NAME" /DISABLEFSR $0 $1

## Parameters
| **${Service::Create}**| **Description** |
| ------ | ------ |
| NAME| The service name|
| PATH| BinaryPathName to the .exe file|
| TYPE| own, share, interact, kernel, filesys, rec |
| START| boot, system, auto, demand, disabled, delayed-auto |
| DEPEND| Dependencies(separated by / (forward slash)) |
| /DISABLEFSR| Disables redirection if x64. Use "" to skip.|
| $0 | Return after call |
| $1 | Return after call |

## Service::Create


<!-- language: lang-nsis -->
```
##=
# The variable $Bit will hold either 64 or 32 depending on system architecture 
# This is used with all the ${Service::} macros
# Use this in the beginning of your code.
# This snippet should only be used once.
#
Var Bit
System::Call "kernel32::GetCurrentProcess()i.s"
System::Call "kernel32::IsWow64Process(is,*i.r0)"
StrCmpS $0 0 +3
StrCpy $Bit 64
Goto +2
StrCpy $Bit 32

##= 
#= Service::Create
#
# USAGE:
# ${Service::Create} "NAME" "PATH" "TYPE" "START" "DEPEND" /DISABLEFSR $0 $1
#
#    ::Create    = Creates a service entry in the registry and Service Database
#    NAME        = The Service name
#    PATH        = BinaryPathName to the .exe file
#    TYPE        = own|share|interact|kernel|filesys|rec
#    START       = boot|system|auto|demand|disabled|delayed-auto
#    DEPEND      = Dependencies(separated by / (forward slash))
#    /DISABLEFSR = Disables redirection if x64. Use "" to skip.
#    $0          = Return after call
#    $1          =   ''    ''    ''
#
!define Service::Create `!insertmacro _Service::Create`
!macro _Service::Create _SVC _PATH _TYPE _START _DEPEND _FSR _ERR1 _ERR2
    StrCmpS $Bit 64 0 +7
    StrCmp "${_FSR}" /DISABLEFSR 0 +6
        StrCmp "${_DEPEND}" "" 0 +3
        ExecDos::Exec /TOSTACK /DISABLEFSR `"${SC}" create "${_SVC}" DisplayName= "${FULLNAME}" binpath= "${_PATH}" type= "${_TYPE}" start= "${_START}"`
        Goto +7
        ExecDos::Exec /TOSTACK /DISABLEFSR `"${SC}" create "${_SVC}" DisplayName= "${FULLNAME}" binpath= "${_PATH}" type= "${_TYPE}" start= "${_START}" depend= ""${_DEPEND}""`
        Goto +5
    StrCmp "${_DEPEND}" "" 0 +3
    ExecDos::Exec /TOSTACK `"${SC}" create "${_SVC}" DisplayName= "${FULLNAME}" binpath= "${_PATH}" type= "${_TYPE}" start= "${_START}"`
    Goto +2
    ExecDos::Exec /TOSTACK `"${SC}" create "${_SVC}" DisplayName= "${FULLNAME}" binpath= "${_PATH}" type= "${_TYPE}" start= "${_START}" depend= ""${_DEPEND}""`
    Pop ${_ERR1}
    Pop ${_ERR2}
!macroend
```
> The /DISABLEFSR parameter should only be used on x64 machines. However, if you mess up there's a failsafe in the macros that will dodge this bullet for you. This applies to al the service macros listed here.

## Service::QueryConfig
<!-- language: lang-nsis -->
```
##= 
#= Service::QueryConfig
#
# USAGE:
# ${Service::QueryConfig} "NAME" /DISABLEFSR $0 $1
#
#    ::QueryConfig = The service's binary path is returned. 
#    NAME          = The Service name
#    /DISABLEFSR   = Disables redirection if x64. Use "" to skip.
#    $0            = Return after call | 1 = success
#    $1            =   ''    ''    ''  | Should be the file path
#
# $1 will now hold the path to it's binary executable or an error
#
!define Service::QueryConfig `!insertmacro _Service::QueryConfig`
!macro _Service::QueryConfig _SVC _FSR _ERR1 _ERR2
    ReadEnvStr $R0 COMSPEC
    StrCmpS $Bit 64 0 +4
    StrCmp "${_FSR}" /DISABLEFSR 0 +3
    ExecDos::Exec /TOSTACK /DISABLEFSR `"$R0" /c "${SC} qc "${_SVC}" | FIND "BINARY_PATH_NAME""`
    Goto +2
    ExecDos::Exec /TOSTACK `"$R0" /c "${SC} qc "${_SVC}" | FIND "BINARY_PATH_NAME""`
    Pop ${_ERR1}
    Pop ${_ERR2}
!macroend
```

## Service::State
<!-- language: lang-nsis -->
```
##= 
#= Service::State
#
# USAGE:
# ${Service::State} "NAME" /DISABLEFSR $0 $1
#
#    ::State     = The service's status is returned. 
#    NAME        = The Service name
#    /DISABLEFSR = Disables redirection if x64. Use "" to skip.
#    $0          = Return after call | 1 = success
#    $1          =   ''    ''    ''  | 1 = running
#
# $1 will now hold "1" if running or "0" if not
#
!define Service::State `!insertmacro _Service::State`
!macro _Service::State _SVC _FSR _ERR1 _ERR2
    ReadEnvStr $R0 COMSPEC
    StrCmpS $Bit 64 0 +4
    StrCmp "${_FSR}" /DISABLEFSR 0 +3
    ExecDos::Exec /TOSTACK /DISABLEFSR `"$R0" /c "${SC} query "${_SVC}" | find /C "RUNNING""`
    Goto +2
    ExecDos::Exec /TOSTACK `"$R0" /c "${SC} query "${_SVC}" | find /C "RUNNING""`
    Pop ${_ERR1}
    Pop ${_ERR2}
!macroend
```

## Service::Start
<!-- language: lang-nsis -->
```
##= 
#= Service::Start
#
# USAGE:
# ${Service::Start} "NAME" /DISABLEFSR $0 $1
#
#    ::Start     = Start a service. 
#    NAME        = The Service name
#    /DISABLEFSR = Disables redirection if x64. Use "" to skip.
#    $0          = Return after call
#    $1          =   ''    ''    ''
#
# $1 will now hold "1" if running or "0" if not
#
!define Service::Start `!insertmacro _Service::Start`
!macro _Service::Start _SVC _FSR _ERR1 _ERR2
    StrCmpS $Bit 64 0 +4
    StrCmp "${_FSR}" /DISABLEFSR 0 +3
    ExecDos::Exec /TOSTACK /DISABLEFSR `"${SC}" start "${_SVC}"`
    Goto +2
    ExecDos::Exec /TOSTACK `"${SC}" start "${_SVC}"`
    Pop ${_ERR1}
    Pop ${_ERR2}
!macroend
```

## Service::Stop
<!-- language: lang-nsis -->
```
##= 
#= Service::Stop
#
# USAGE:
# ${Service::Stop} "NAME" /DISABLEFSR $0 $1
#
#    ::Stop      = Sends a STOP control request to a service. 
#    NAME        = The Service name
#    /DISABLEFSR = Disables redirection if x64. Use "" to skip.
#    $0          = Return after call
#    $1          =   ''    ''    ''
#
!define Service::Stop `!insertmacro _Service::Stop`
!macro _Service::Stop _SVC _FSR _ERR1 _ERR2
    StrCmpS $Bit 64 0 +4
    StrCmp "${_FSR}" /DISABLEFSR 0 +3
    ExecDos::Exec /TOSTACK /DISABLEFSR `"${SC}" stop "${_SVC}"`
    Goto +2
    ExecDos::Exec /TOSTACK `"${SC}" stop "${_SVC}"`
    Pop ${_ERR1}
    Pop ${_ERR2}
!macroend
```

## Service::Remove
<!-- language: lang-nsis -->
```
##= 
#= Service::Remove
#
# USAGE:
# ${Service::Remove} "NAME" /DISABLEFSR $0 $1
#
#    ::Remove    = Deletes a service entry from the registry. 
#    NAME        = The Service name
#    /DISABLEFSR = Disables redirection if x64. Use "" to skip.
#    $0          = Return after call
#    $1          =   ''    ''    ''
#
# Be sure to stop a service first if it's running.
#
!define Service::Remove `!insertmacro _Service::Remove`
!macro _Service::Remove _SVC _FSR _ERR1 _ERR2
    StrCmpS $Bit 64 0 +4
    StrCmp "${_FSR}" /DISABLEFSR 0 +3
    ExecDos::Exec /TOSTACK /DISABLEFSR `"${SC}" delete "${_SVC}"`
    Goto +2
    ExecDos::Exec /TOSTACK `"${SC}" delete "${_SVC}"`
    Pop ${_ERR1}
    Pop ${_ERR2}
!macroend
```

