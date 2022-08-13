---
title: "Getting started with batch-file"
slug: "getting-started-with-batch-file"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Editing and Viewing Batch Files
Any ASCII editor can edit batch files. A list of editors that can syntax highlight batch syntax can be found [here][1]. You can also use the default notepad shipped with windows to edit and view a batch file, although it does not offer syntax highlighting. 

To open notepad:

 - Press <kbd>Win 𐌎</kbd>+<kbd>R</kbd>, type `notepad` and then press <kbd>Enter</kbd>.

Alternatively, the most "primitive" way to [create a batch file][2] is to redirect output from the command line to a file, eg.

    echo echo hello world > first.bat

which writes `echo hello world` to the file `first.bat`. 

You can edit a batch file by right clicking the file and selecting "Edit" from the context menu. 

To view the contents of a batch file from within a command prompt, run the following command:

    type first.bat

You can also start editing your batch file with notepad from the command prompt by typing

    notepad first.bat


  [1]: http://www.robvanderwoude.com/scripteditors.php
  [2]: https://www.wikiod.com/batch-file/creating-files-using-batch

## Getting Help
To get help on a batch file command you can use the built-in help.

Open a command prompt (whose executable is `cmd.exe`) and enter `help` to see all available commands. 

To get help for any of these commands, type `help` followed by the name of the command. 

For example:

    help help

Will display:

    Provides help information for Windows commands.

    HELP [command]

        command - displays help information on that command.

Some commands will also display help if followed by `/?`.

Try: 

    help /?

Note:

`Help` will only display the help for **internal** commands.



## Opening a Command Prompt
The command prompt comes pre-installed on all Windows NT, Windows CE, OS/2 and eComStation operating systems, and exists as `cmd.exe`, typically located in `C:\Windows\system32\cmd.exe`

On Windows 7 the fastest ways to open the command prompt are:

 - Press <kbd>Win 𐌎</kbd>, type "cmd" and then press <kbd>Enter</kbd>.

 - Press <kbd>Win 𐌎</kbd>+<kbd>R</kbd>, type "cmd" then then press <kbd>Enter</kbd>.

 - If you have an explorer window open, type "cmd" in the address bar to open a prompt in the currently selected directory.

 - Right-click a folder in Explorer while holding <kbd>Shift</kbd> and select "Open command window here".

It can also be opened by navigating to the executable and double-clicking on it.

In some cases you might need to run `cmd` with elevated permissions, in this case right click and select "Run as administrator". This can also be achieved by pressing <kbd>Control</kbd>+<kbd>Shift</kbd>+<kbd>Enter</kbd> instead of <kbd>Enter</kbd> when using way 1 of the points above.

