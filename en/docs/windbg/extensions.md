---
title: "Extensions"
slug: "extensions"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## SOS
SOS (son of strike) is the official WinDbg extension from Microsoft for .NET. It gets installed as part of the .NET framework and thus is available by default.

Like any extension, it can be loaded using `.load x:\full\path\to\sos.dll`, but there are easier ways. Depending on the version of .NET, the extension is located side by side to `mscorwks.dll` (.NET CLR 2), `clr.dll` (.NET CLR 4) or `coreclr.dll` (Silverlight and Universal apps), so one of the following commands should work:

    .loadby sos clr
    .loadby sos coreclr
    .loadby sos mscorwks

For a list of available commands, consult `!help`.

## SOSex
SOSex is an extension to SOS, written by [Steve Johnson](http://stackoverflow.com/users/1269097/steve-johnson), a Microsoft employee. He provides [SOSex for download](http://www.stevestechspot.com/) for free, but it's not open source.

Typically, the extension is not available side by side to any other DLL, so it is usually loaded with `.load x:\full\path\to\sosex.dll`.

Besides simplifying debugging of .NET, the command `!dlk` can also be used in native environments for checking deadlocks of critical sections.

For a list of available commands, consult `!help` of SOSex.

## PyKD
[PyKD](https://pykd.codeplex.com/) is a WinDbg extension that enables you writing Python scripts. It's open source.

Typically, the extension is not available side by side to any other DLL, so it is usually loaded with `.load x:\full\path\to\pykd.pyd`, where PYD is the extension for a python DLL, but you can rename it to DLL if you like.

Getting started with PyKd
-

PyKD does not offer `!help`, so look up the documentation at Codeplex. Many developers seem to be from Russia and the most up-to-date and complete documentation is probably in Russian. The Google translater does a decent job.

Like other extensions, use the correct bitness of the extension that corresponds to that of WinDbg. In addition to that you must have Python installed with the same bitness as well.

`!py` runs an REPL interpreter and `!py x:\path\to\script.py` runs a python script. Scripts should use

    from pykd import *

as the first line in order to make use of PyKD's functionality, while this line is not needed in the REPL interpreter. The interpreter can be exited using `exit()`.


## NetExt
[NetExt](https://netext.codeplex.com/) is an extension for .NET which provides 

* LINQ-like queries for objects on the heap (`!wselect`, `!wfrom`)
* display capabilities for special objects like dictionaries and hash tables (`!wdict`, `!whash`)
* ASP.NET / HTTP related commands (`!wcookie`, `!wruntime`, `!whttp`)
* several other network related commands

Typically, the extension is not available side by side to any other DLL, so it is usually loaded with .load x:\full\path\to\netext.dll

## Extensions overview
An incomplete list of WinDbg extensions that are not installed with WinDbg itself:

| Extension | Purpose |
|-----------|---------|
| SOS       | .NET (official Microsoft extension) |
| SOSex     | .NET (extension for SOS) |
| CoSOS     | .NET (extension for SOS) |
| NetExt    | .NET (with focus on networking) |
| PyKD      | Python scripting |
| PDE       | Windows native and store applications (stowed exceptions) |
| PSSCOR    | .NET |
| SDbgExt   | .NET |
| [MEX][1]  | .NET |


  [1]: https://www.microsoft.com/en-us/download/details.aspx?id=53304

## CoSOS
[CoSOS](https://github.com/krk/cosos) (cousin of SOS) is an open source extension for WinDbg focusing on .NET memory fragmentation (`!gcview`) and threading issues (`!wfo`, `!tn`).

Typically, the extension is not available side by side to any other DLL, so it is usually loaded with `.load x:\full\path\to\cosos.dll`. It requires that SOS is loaded  and currently works with 32 bit applications only.

