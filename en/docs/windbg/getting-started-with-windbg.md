---
title: "Getting started with WinDbg"
slug: "getting-started-with-windbg"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Microsoft [describes 3 ways](https://msdn.microsoft.com/en-us/library/windows/hardware/ff551063(v=vs.85).aspx) of installing WinDbg:
* as part of the WDK (Windows Driver Kit)
* as part of the SDK (Software Development Kit)
* with the installer of the SDK and deselecting everything else but "Debugging Tools for Windows"

To get the installer, visit [Download the WDK, WinDbg, and associated tools](https://developer.microsoft.com/en-us/windows/hardware/windows-driver-kit) and scroll down to a section called "Get debugging tools".

A well-known and convenient but inofficial source is [Codemachine](http://codemachine.com/downloads.html) where you can also download older versions of the Debugging Tools directly.

The setup itself is straight-forward. Click through the installer until it finishes.

## Debuggers
WinDbg is often used as an abbreviation of "Debugging tools for Windows". It contains different debuggers:

| Debugger | Description |
|----------|-------------|
| WinDbg | the debugger with a graphical user interface |
| CDB | **c**onsole **d**e**b**ugger, user mode debugger which runs in the currently open console |
| NTSD | **n**ew **t**erminal **s**ymbolic **d**ebugger, user mode debugger which opens a new terminal (console) as the name suggests |
| KD | the **k**ernel **d**ebugger, which runs in the currrently open console |
| NTKD | **n**ew **t**erminal **k**ernel **d**ebugger, opens a new terminal |

The commands are identical, except that there may be GUI related commands which don't work in the console versions.

