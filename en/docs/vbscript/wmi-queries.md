---
title: "WMI queries"
slug: "wmi-queries"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

VBScript can query Windows Management Instrumentation (WMI) for various vital info related to local and remote PC . We can use WMI queries to perform various tasks such as extracting PC's name , getting screen resolution , getting info about user and username , extracting vital info about any process , modifying core system settings,etc .

Below are some examples which use WMI queries to carry out specific tasks .

## Extracting Local PC's name
    strComputer = "."
    Set objWMIService = GetObject("winmgmts:" _& "{impersonationLevel=impersonate}!\\" & strComputer & "\root\cimv2")

    Set colSettings = objWMIService.ExecQuery _("Select * from Win32_ComputerSystem")

    For Each objComputer in colSettings
    wscript.echo objComputer.Name
    Next

This code will echo PC's name in which it's executed .

## Getting number of instances of any process
    strComputer = "."
    instances = 0
    processName = "chrome.exe"

    Set objWMIService = GetObject("winmgmts:\\" & strComputer & "\root\cimv2")
    Set colProcess = objWMIService.ExecQuery("Select * from Win32_Process")

    For Each objProcess in colProcess
    If objProcess.Name = processName Then instances = instances + 1
    Next

    wscript.echo "Process - "&processName&" has "&instances&" instances running."

## Getting Active Monitor's Screen Resolution
    strComputer = "."
    Set objWMIService = GetObject("winmgmts:\\" & strComputer & "\root\cimv2")
    Set colItems = objWMIService.ExecQuery("Select * from Win32_DesktopMonitor",,48)

    For Each objItem in colItems
    WScript.Echo "ScreenHeight: " & objItem.ScreenHeight
    WScript.Echo "ScreenWidth: " & objItem.ScreenWidth
    Next

