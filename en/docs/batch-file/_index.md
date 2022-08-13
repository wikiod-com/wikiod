---
title : batch-file Tutorial
slug : batch-file-tutorial
weight : 9883
draft : false
images : []
type : docs
---

From Microsoft Technet:
> With batch files, which are also called batch programs or scripts, you can simplify routine or repetitive tasks. A batch file is an unformatted text file that contains one or more commands and has a .bat or .cmd file name extension. When you type the filename at the command prompt, Cmd.exe runs the commands sequentially as they appear in the file.

**Batch File Names and Extensions**

| Extension | Remarks |
| ------ | ------ |
| .bat   |This extension runs with MS-DOS and all versions of Windows   |
| .cmd   |Used for batch files in Windows NT family |
| .btm   |The extension used by 4DOS and 4NT  |

To understand the difference between `.cmd` and `.bat` please see [here][2].

Avoid names which are already the name of built-in commands. like `tracert`. There is a utility called `tracert.exe`. So, avoid naming a batch file `tracert.bat`

**Running Batch File**

The easiest way to run a batch file is simply double-clicking its icon. Or paste the file full path into a command prompt, or just its name if command Prompt was started from the batch file directory, then enter.

Example:
    
    C:\Foo\Bar>test.bat
    C:\Foo\Bar>C:\Foo\Bar\Baz\test.bat


  [1]: http://www.robvanderwoude.com/scripteditors.php
  [2]: http://waynes-world-it.blogspot.in/2008/08/difference-between-bat-and-cmd.html

