---
title: "Elevated Privileges in Batch Files"
slug: "elevated-privileges-in-batch-files"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Requesting Elevate Privileges in a Shortcut
Many tasks require elevated privileges. You can elevate user privileges to Administrator level for your batch runtime using a shortcut:

1. Press <kbd>alt</kbd>+<kbd> and the batch file to a selected folder to create a shortcut. 

2. Right click and select "Properties".

3. Select the "Shortcut" tab.

4. Click "Advanced".

   [![enter image description here][1]][1]

5. Enable "Run as Administrator".

   [![enter image description here][2]][2]

6. Click "OK" twice.


  [1]: http://i.stack.imgur.com/9Fbi5.png
  [2]: http://i.stack.imgur.com/IFNbI.png

## Requesting Elevated Privileges at Runtime
The following batch file will popup a UAC Prompt allowing you to accept elevated Administrator privileges for the batch session. Add your tasks code to `:usercode` section of the batch, so they run with elevated privileges.

    @echo off
    setlocal EnableDelayedExpansion
    :: test and acquire admin rights
    cd /d %~dp0 & echo/
    if not "%1"=="UAC" (
        >nul 2>&1 net file && echo Got admin rights || (echo No admin rights & ^
    MSHTA "javascript: var shell = new ActiveXObject('shell.application'); shell.ShellExecute("%~snx0", 'UAC', '', 'runas', 1);close();"))
    :: re-test admin rights
    echo/ & >nul 2>&1 net file && (echo Got admin rights & echo/) || (echo No admin rights. Exiting... & goto :end)
    
    :usercode
    :: add your code here
    echo Performing admin tasks
    echo Hello >C:\test.txt
    
    :end
    timeout /t 5 >nul
    exit /b    

## Requesting runtime elevated privileges without UAC prompt
As previous example, this script request elevation if needed. We ask the user for credentials avoiding the UAC prompt.

    @echo off
    
    cls & set "user=" & set "pass="
    
    :: check if we have administrative access
    :: ------------------------------------------------------------------------
    whoami /groups | find "S-1-5-32-544">NUL 2>&1 && goto:gotAdmin
    
    
    echo/
    echo/ Testing administrative privileges
    echo/
    echo/ Please enter administrative credentials
    echo/
    
    :: get user
    :: ------------------------------------------------------------------------
    set/P user="*     User:: "
    if "%user%" EQU "" exit/B 1
    :: get password
    :: ------------------------------------------------------------------------
    <NUL set/p=* password& call:getPass pass || exit/B 1
    if "%pass%" EQU "" exit/B 1
    
    :: check if credential has administrative privileges 
    :: this call can be removed
    :: ------------------------------------------------------------------------
    call:askIsAdmin "%user%", "%pass%" || goto:notAdmin
    
    :: run elevated without UAC prompt
    :: with the previous call enabled, we are sure credentials are right
    :: without it, this will fail if credentials are invalid
    :: ------------------------------------------------------------------------
    call:elevateScript "%user%", "%pass%" 
    
    exit/B 0
    
    :gotAdmin
    echo(
    echo( You have administrative rights.
    echo( 
    timeout /T 7 
    exit/B 0
    
    :notAdmin
    echo(
    echo( Invalid credential or non administrative account privileges
    echo( 
    timeout /T 7 
    exit/B 1
    
    :: invoke powershell to get the password
    :: ------------------------------------------------------------------------
    :getPass
    SetLocal
    set "psCmd=powershell -Command "$pwd = read-host ':' -AsSecureString; $BSTR=[System.Runtime.InteropServices.Marshal]::SecureStringToBSTR($pwd); [System.Runtime.InteropServices.Marshal]::PtrToStringAuto($BSTR)""
    for /F "usebackq delims=" %%# in (`%psCmd%`) do set "pwd=%%#"
    if "%pwd%" EQU "" EndLocal & exit/B 1
    EndLocal & set "%1=%pwd%"
    doskey /listsize=0 >NUL 2>&1 & doskey /listsize=50 >NUL 2>&1        & rem empty keyboard buffer
    exit/B 0
    
    :: check if credential has administrative privileges
    :: ------------------------------------------------------------------------
    :askIsAdmin
    SetLocal & set "u=%~1" & set "p=%~2" & set/A ret=1
    set "psCmd=powershell -Command "$p='%p%'^|convertto-securestring -asplaintext -force;$c=new-object -typename system.management.automation.pscredential('%u%',$p^);start-process 'powershell' '-Command "write-host ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent(^)^).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator^)"' -credential $c -passthru -wait;""
    for /F "usebackq delims=" %%# in (`%psCmd%`) do @set "result=%%#"
    echo %result% | find /I "true">NUL 2>&1 && set/A ret=0
    EndLocal & exit/B %ret%
    exit/B 1
    
    :: run elevated without UAC prompt
    :: ------------------------------------------------------------------------
    :elevateScript
    SetLocal & set "u=%~1" & set "p=%~2" 
    set "_vbs_file_=%TEMP%\runadmin.vbs"
    echo set oWS ^= CreateObject^("wScript.Shell"^)>"%_vbs_file_%"
    echo strcmd="C:\Windows\system32\runas.exe /user:%COMPUTERNAME%\%u% " + """%~dpnx0""">>"%_vbs_file_%"
    echo oWS.run strcmd, 2, false>>"%_vbs_file_%"
    echo wScript.Sleep 100>>%_vbs_file_%
    echo oWS.SendKeys "%p%{ENTER}">>%_vbs_file_%
    if exist "%TEMP%\runadmin.vbs" (set "_spawn_=%TEMP%\runadmin.vbs") else (set "_spawn_=runadmin.vbs")
    ping 1.1.1.1 -n 1 -w 50 >NUL
    start /B /WAIT cmd /C "cls & "%_spawn_%" & del /F /Q "%_spawn_%" 2>NUL"
    EndLocal & set "pass="
    exit/B 0



