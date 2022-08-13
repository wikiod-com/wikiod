---
title: "OS-utilities"
slug: "os-utilities"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

There are several built in functions and statements for accessing the operating system.

## OS-COMMAND
Executes a OS-command. 

OS-COMMAND without any options will start a new shell and not exit it - thus you will on graphical OS:es leave a window "hanging". 

    DEFINE VARIABLE cmd AS CHARACTER   NO-UNDO.
    
    cmd = "dir".
    
    OS-COMMAND VALUE(cmd).

There are three options: `SILENT`, `NO-WAIT` and `NO-CONSOLE`.

**SILENT**
>After processing an operating system command, the AVM shell pauses. To exit the window in Windows GUI platforms, you must type exit. To exit the window in Windows character platforms, you must type exit and press RETURN or SPACEBAR. You can use the SILENT option to eliminate this pause. Use this option only if you are sure that the program, command, or batch file does not generate any output to the screen. Cannot be used with NO-WAIT. 

    OS-COMMAND SILENT VALUE("runprogram.exe").

**NO-WAIT**
> In a multi-tasking environment, causes the AVM to immediately pass control back to next statement after the OS-COMMAND without waiting for the operating system command to terminate. Cannot be used with SILENT. This option is supported in Windows only. 

    OS-COMMAND NO-WAIT VALUE("DIR > dirfile.txt").

On Linux/Unix you will have to achieve this by preceding the command with a `&`-sign instead:

    OS-COMMAND VALUE("ls >> file.txt &").

**NO-CONSOLE**
> While processing an operating system command, the AVM creates a console window. The console window may not be cleaned up after the command is executed. You can use the NO-CONSOLE option to prevent this window from being created in the first place. 

    OS-COMMAND NO-CONSOLE VALUE("startbach.bat").


No errors are ever returned from `OS-COMMAND` to Progress ABL so you have to check for errors another way, possibly writing them to a file in a shell-script or similar.


## OPSYS
The `OPSYS`-function returns what OS the program is running on:

    MESSAGE OPSYS VIEW-AS ALERT-BOX.

Result:

[![enter image description here][1]][1]


It can be used to select what OS-utility to call:

    IF OPSYS = "LINUX" THEN 
        OS-COMMAND VALUE("ls -l").
    ELSE 
        OS-COMMAND VALUE("dir").



  [1]: https://i.stack.imgur.com/6sfN6.png


## OS-ERROR
Returns an error from a previous `OS-*` call represented by an integer. The calls that can return an OS-ERROR are:
- OS-APPEND
- OS-COPY
- OS-CREATE-DIR
- OS-DELETE
- OS-RENAME 
- SAVE CACHE 

Note that `OS-COMMAND` is missing. You need to handle errors in OS-COMMAND yourself.

| Error number| Description                 |
| ----------- | --------------------------- |
|  0          | No error                    |
|  1          | Not owner                   |
|  2          | No such file or directory   | 
|  3          |Interrupted system call      | 
|  4          |I/O error                    | 
|  5          |Bad file number              |
|  6          |No more processes            |
|  7          |Not enough core memory       | 
|  8          |Permission denied            |
|  9          |Bad address                  | 
| 10          | File exists                 |
| 11          |No such device               |
| 12          |Not a directory              |
| 13          |Is a directory               | 
| 14          |File table overflow          |
| 15          |Too many open files          |
| 16          |File too large               |
| 17          |No space left on device      |
| 18          |Directory not empty          |
|999          | Unmapped error (ABL default)| 


## OS-GETENV function
Returns the value of any OS environment variable.

    MESSAGE OS-GETENV ("OS") VIEW-AS ALERT-BOX.

On a Windows machine:

[![enter image description here][1]][1]


    MESSAGE OS-GETENV ("SHELL") VIEW-AS ALERT-BOX.
Result on a Linux machine with Bash as current shell:

                            ┌────── Message ───────┐
                            │      /bin/bash       │
                            │ ──────────────────── │
                            │         <OK>         │
                            └──────────────────────┘






  [1]: https://i.stack.imgur.com/ugpDH.png

## OS-COPY
Copy a file

> COPY source-file target-file

Copy `c:\temp\source-file.txt` to `c:\temp\target-file.txt`. You need to check `OS-ERROR`
 for success or lack thereof.

    OS-COPY VALUE("c:\temp\source-file.txt")  VALUE("c:\temp\target-file.txt").
    IF OS-ERROR <> 0 THEN DO:
        MESSAGE "An error occured" VIEW-AS ALERT-BOX ERROR.
    END.



## OS-DELETE
Deletes a file, or a file-tree.

As with many other OS-* utilities, you have to check status in `OS-ERROR`.

> OS-DELETE  file-or-dir-to-delete [ RECURSIVE ]

Delete the entire `/tmp/dir` tree:

    OS-DELETE VALUE("/tmp/dir") RECURSIVE.

Delete the file called `c:\dir\file.txt`

    OS-DELETE VALUE("c:\dir\file.txt").





## OS-CREATE-DIR
Creates a directory, status is in `OS-ERROR`

> OS-CREATE-DIR directory

Create a directory called `/usr/local/appData`

    OS-CREATE-DIR VALUE("/usr/local/appData").

## OS-APPEND
Append one file to another. Status is checked in `OS-ERROR`

> OS-APPEND source target

Appends `targetfile.txt` with `sourcefile.txt`:

    OS-APPEND VALUE("sourcefile.txt") VALUE("targetfile.txt").


## OS-RENAME
Rename a file or directory. Status is in `OS-ERROR`. Can also be used to move files (or move and rename).

> OS-RENAME oldname newname

Rename `/tmp/old-name` to `/tmp/new-name`:

    OS-RENAME VALUE("/tmp/old-name") VALUE("/tmp/new-name").

Move file `c:\temp\old.txt` to `c:\new-dir\old.txt`:

    OS-RENAME VALUE("c:\temp\old.txt") VALUE("c:\new-dir\old.txt").

## OS-DRIVES (Windows only)
Returns a list of all drives on a system.

    MESSAGE OS-DRIVES VIEW-AS ALERT-BOX.

Result with four drives, C through F:

[![enter image description here][1]][1]

On Linux the list will simply be empty as there by definitions are no "drives" connected. Listing directories is done in another way (`INPUT FROM OS-DIR`)

  [1]: https://i.stack.imgur.com/d8VlI.png

