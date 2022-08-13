---
title: "Using WMI in VBScript"
slug: "using-wmi-in-vbscript"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Getting a WMI Object and listing some of its properties
This example will list the preferred resolution for all the connected monitors. 

The Code:

    On Error Resume Next
    strComputer = "."
    strQuery = "SELECT PreferredMonitorSourceModeIndex, MonitorSourceModes " & _
               "FROM WmiMonitorListedSupportedSourceModes"
    
    Set objWMIService = GetObject("winmgmts:\\" & strComputer & "\ROOT\WMI")
    Set colItems = objWMIService.ExecQuery(strQuery, , 48)
    
    For Each objItem In colItems
      i = objItem.PreferredMonitorSourceModeIndex
      wscript.stdout.writeline "InstanceName: " & objItem.instancename
      wscript.stdout.writeline "Horizontal: " & objItem.MonitorSourceModes(i).HorizontalActivePixels
      wscript.stdout.writeline "Vertical: " & objItem.MonitorSourceModes(i).VerticalActivePixels
    Next

We first get the WMI Service. It is not creatable.

    Set objWMIService = GetObject("winmgmts:\\" & strComputer & "\ROOT\WMI")

Next, set up our query using [WQL](https://msdn.microsoft.com/en-us/library/aa394606%28v=vs.85%29.aspx). WQL is very similar to SQL.

    strQuery = "SELECT PreferredMonitorSourceModeIndex, MonitorSourceModes " & _
               "FROM WmiMonitorListedSupportedSourceModes"

The WMI class [WmiMonitorListedSupportedSourceModes](https://msdn.microsoft.com/en-us/library/windows/desktop/aa394544%28v=vs.85%29.aspx) has 5 properties: InstanceName, Active, MonitorSourceModes, NumOfMonitorSourceModes, and PreferredMonitorSourceModeIndex. MonitorSourceModes is an array, and we must query PreferredMonitorSourceModeIndex to determine which element of the array contains the information we seek.

Now let's execute our query
    
    Set colItems = objWMIService.ExecQuery(strQuery, , 48)

and loop through the results:

    For Each objItem In colItems
      i = objItem.PreferredMonitorSourceModeIndex
      wscript.stdout.writeline "InstanceName: " & objItem.instancename
      wscript.stdout.writeline "Horizontal: " & objItem.MonitorSourceModes(i).HorizontalActivePixels
      wscript.stdout.writeline "Vertical: " & objItem.MonitorSourceModes(i).VerticalActivePixels
    Next

## Executing a WMI Method
Some WMI Classes expose Methods that allow you to *do* something with that object. For example, the [Win32_Printer](https://msdn.microsoft.com/en-us/library/aa394363(v=vs.85).aspx) class has 11 methods for interacting with a printer, one of which is the PrintTestPage method. The following code demonstrates how to select a specific printer and print a test page.

    'Specify the name of the target computer
    strComputer = "."
    'Note: Backslash is a special character that must be escaped with a backslash
    'This means the UNC \\Network\Path\PrinterName must be written like the following
    strQuery = "SELECT * FROM Win32_Printer WHERE DeviceID='\\\\Network\\Path\\PrinterName'"
    
    Set objWMIService = GetObject("winmgmts:\\" & strComputer & "\ROOT\cimv2")
    Set colItems = objWMIService.ExecQuery(strQuery)
    
    'ExecQuery returns a collection object, even when there's only 1 item in the collection
    For Each objItem In colItems
        'The PrintTestPage method takes no parameters and returns a UINT32
        intTestPageReturnCode = objItem.PrintTestPage
    Next
    'PrintTestPage will return 0 (Successs) or 5 (Failure)
    Select Case intTestPageReturnCode
        Case 0
            WScript.StdOut.WriteLine "Test page successfully printed"
        Case 5
            WScript.StdOut.WriteLine "Test page printing failed"
        Case Else
            WScript.StdOut.WriteLine "An unknown error occurred while printing a test page"
    End Select


