---
title: "Collecting adb commands log"
slug: "collecting-adb-commands-log"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Make sure that your automation does not use `adb kill-server` command.

## in Windows
Open a *Command Prompt* window and run the following commands:

    adb kill-server
    set ADB_TRACE=sockets
    adb nodaemon server 2>&1 | for /f "usebackq tokens=7*" %a in (`findstr /c:"): '"`) do @echo %a %b >> %USERPROFILE%\Desktop\adb_host_log.txt

Now you can run your Android automation. When done run `adb kill-server` in another *Command Prompt* window. Now the *adb_host_log.txt* file on your *Desktop* contains the log of all commands all *adb clients* have sent to the *adb host*.

