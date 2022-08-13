---
title: "Registering Libraries (RegDLL)"
slug: "registering-libraries-regdll"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

In short, a DLL is a collection of small executable code, which can be called upon when needed by a program that's running. The DLL lets the executable communicate with a specific device such as a printer or may contain code to do any number of particular functions. As there are several methods of implementations to do this, in this topic I'll be showing you how to register and unregister any DLL that your application calls for; and we'll be doing so using the `RegSrv32.exe` command line.

## Syntax
 - `${Register::DLL} "codec.ocx" "" $1 $0`
 - `${UnRegister::DLL} "volumeCtrl.cpl" /DISABLEFSR $1 $0`
 - `${RegisterDLL} "print.drv"`

## Parameters
**Switch**|**Discription**
-------- | ---
| `/u` | Unregisters server.
| `/s` | Specifies `regsvr32` to run silently and to not display any message boxes.
| `/n` | Specifies not to call *DllRegisterServer*. You must use this option with `/i`.
| `/i` **:cmdline** | Calls *DllInstall* passing it an optional **[cmdline]**. When used with `/u`, it calls dll uninstall.
|**dllname** | Specifies the name of the dll file that will be registered.
|`/?`   | Displays help at the command prompt.

RegSrv32.exe
======
Using the `RegSvr32.exe` command line is my preferred method on getting you libraries registered so let's start here first. Windows PCs coupled with Internet Explorer 3.0 or later come stock with `RegSvr32.exe`. So there's a good chance your PC comes standard with this utility. Now if you are running on a 64-bit machine, there are two variants you can consider. They can be found in either `$WINDIR\system32` or  `$WINDIR\SysWow32`.

The parameters you can use with RegSrv32 are `/u` `/s` `/i` `/n`. The `/u` command switch will unregister the file. The `/i` switch can be used with `/u` to call for DLL uninstallation. The `/n` parameter will not call _DllRegisterServer_; it's used with `/i` which is the install switch. If you use `/s`, which means silent, no message boxes will be displayed on Windows XP or later.

When using `RegSvr32.exe` from the command line you'll get message boxes after calling it. The *DLLSelfRegister* function will be invoked unless using the aforementioned switch of course; if successful an alert box will be shown denoting its success&mdash;as the same for failure which throws an error message.

>It's been my experience that the x64 `RegSvr32.exe` registers x86 DLL's properly on Windows Vista and above (excludes XP; visit this <a href="http://support.microsoft.com/kb/282747">article</a> for more) and above so I use it when installing on x64 systems even when registering a x86 file. Besides, Windows XP is a dying art; bless it's heart. =)

## RegSrv32.exe Simple Usage
<!-- language: lang-nsis -->
```
#= Regardless of architecture we're using just the following

!define REGSVR `$SYSDIR\regsvr32.exe` #= define where RegSrv32 is
!define DLL `$AppDirectory\App\MyLegalProgram\myLegit.dll` #= define the file to register

##=
#= Command line usage is the same for both variants of RegSrv32 as follows
#= regsvr32 [/u] [/s] [/n] [/i[:cmdline]] DLL
#=
##= So in our .nsi file it would be similar to the following:

Exec `"${REGSVR}" /s "${DLL}"`

#= Moreover, you may also use the following

ExecWait `"${REGSVR}" /s "${DLL}"` $0 #= The $0 will contain the error code if any

#= The above will wait for exe to quit it's process before continuing
```

## Macro: Register::DLL
<!-- language: lang-nsis -->
```
##=
# The variable $Bit will hold either 64 or 32 depending on system architecture 
# This is used with ${Register::DLL} and ${UnRegister::DLL}
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
#= Register::DLL
#
# USAGE:
# ${Register::DLL} "DLL Filename" /DISABLEFSR $0 $1
#
#        ::DLL       = Registers a DLL file. 
#        /DISABLEFSR = Disables redirection if x64. Use "" to skip.
#        $0          = Return after call
#        $1          =  ''      ''    ''
#
!define Register::DLL `!insertmacro _Register::DLL`
!macro _Register::DLL _DLL _FSR _ERR1 _ERR2
    StrCmpS $Bit 64 0 +4
    StrCmp ${_FSR} /DISABLEFSR 0 +3
    ExecDos::Exec /TOSTACK /DISABLEFSR `"${REGSVR}" /s "${_DLL}"`
    Goto +2
    ExecDos::Exec /TOSTACK `"${REGSVR}" /s "${_DLL}"`
    Pop ${_ERR1}
    Pop ${_ERR2}
!macroend

##= 
# Alternatively you can use this macro found in my travels 
# but you should include x64.nsh as this macro makes use
# of it's function.
#
#= USAGE:
# ${RegisterDLL} "SomeLibrary.dll"
#
!define RegisterDLL "!insertmacro _RegisterDLL"
!macro _RegisterDLL _DLL
    ${If} ${RunningX64}
        ${DisableX64FSRedirection}
        ExecWait '"$SYSDIR\regsvr32.exe" /s "${_DLL}"'
        ${EnableX64FSRedirection}
    ${Else}
        RegDLL "${DLL}"
    ${EndIf}
!macroend
```
>The /DISABLEFSR parameter should only be used on x64 machines. However, if you mess up there's a failsafe in the macros that will dodge this bullet for you. 

## Macro: UnRegister::DLL
<!-- language: lang-nsis -->
```
##=
# The variable $Bit will hold either 64 or 32 depending on system architecture 
# This is used with ${Register::DLL} and ${UnRegister::DLL}
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

#=# 
#= UnRegister::DLL
#
# USAGE:
# ${UnRegister::DLL} "DLL Filename" /DISABLEFSR $0 $1
#
#        ::DLL       = Unregisters a DLL file. 
#        /DISABLEFSR = Disables redirection if x64. Use "" to skip.
#        $0          = Return after call
#        $1          =  ''      ''    ''
#
!define UnRegister::DLL `!insertmacro _UnRegister::DLL`
!macro _UnRegister::DLL _DLL _FSR _ERR1 _ERR2
    StrCmpS $Bit 64 0 +4
    StrCmp ${_FSR} /DISABLEFSR 0 +3
    ExecDos::Exec /TOSTACK /DISABLEFSR `"${REGSVR}" /s /u "${_DLL}"`
    Goto +2
    ExecDos::Exec /TOSTACK `"${REGSVR}" /s /u "${_DLL}"`
    Pop ${_ERR1}
    Pop ${_ERR2}
!macroend

##= 
# Alternatively you can use this macro I found in my travels 
# but be sure to include the x64 plugin as this macro makes 
# use of it's function.
#
#= USAGE:
# ${UnregisterDLL} "SomeLibrary.dll"
#
!define UnregisterDLL "!insertmacro _UnregisterDLL"
!macro _UnregisterDLL _DLL
    ${If} ${RunningX64}
        ${DisableX64FSRedirection}
        ExecWait '"$SYSDIR\regsvr32.exe" /s /u "${_DLL}"'
        ${EnableX64FSRedirection}
    ${Else}
        UnRegDLL "${DLL}"
    ${EndIf}
!macroend
```

>The /DISABLEFSR parameter should only be used on x64 machines. However, if you mess up there's a failsafe in the macros that will dodge this bullet for you. 

