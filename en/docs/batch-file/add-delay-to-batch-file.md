---
title: "Add delay to Batch file"
slug: "add-delay-to-batch-file"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

This topic will teach you one of the many useful things to know in the scripting language, batch file; Adding a delay/pause/timeout to your batch file. 

## Timeout
-----
**Timeout**
-------
The simplest way to make a delay or pause for a certain amount of time, is with the standard command `TIMEOUT`. To make a timeout that lasts exactly one minute we type:

    timeout /t 60
Now what is going on here?

First off we use the command `TIMEOUT` with the parameter `/T` (which simply means timeout) then we specify the amount of seconds to wait. In this case... `60` seconds.

**Timeout with the parameter /NOBREAK**

If we take the example from before and run that in a BATCH file: `timeout /t 60` then while waiting those 60 seconds, you are actually able to break the timeout by pressing any key on your keyboard. To prevent this we simply add the parameter `/NOBREAK` to the end of it.

    timeout /t 60 /nobreak
By doing this it will timeout for 60 seconds, and if you want to break the timeout you will have to press (CTRL-C) on your keyboard.

**Silent timeout**

When it's doing a timeout it will display:

    Waiting for X seconds, press a key to continue ...
    or 
    Waiting for X seconds, press CTRL+C to quit ... [This is with the /NOBREAK parameter]

To hide the message use the `NUL` argument (For explanation of `NUL`: **[Click Here][1]**)

    timeout /t 60 > nul
    or
    timeout /t 60 /nobreak > nul


  [1]: http://www.robvanderwoude.com/battech_redirection.php

## Pause
To make your script pause simply use the `PAUSE` command.

    PAUSE

This will display the text `Press any key to continue . . .`, then add a newline on user input.

Let's say we want to create a "Hello World" program and after we click something on our keyboard, we want it to exit the program with the `EXIT` command.

    echo Hello World
    pause
    exit

Here it uses the [`ECHO` command][1] to say "Hello World". Then we use the `PAUSE` command which displays `Press any key to continue . . .` and then we use the `EXIT` command to terminate the current BATCH script.

When it's pausing it will display:

    Press any key to continue . . .

 **Hide the "Press any key to continue... prompt**

To hide the message we redirect the output to a special device called `nul`. This isn't actually a real device, but whatever we send to it is thrown away.

    pause > nul


  [1]: //stackoverflow.com/documentation/batch-file/3981/echo#t=201702171526534444021

## Ping
---
**Ping**
---

One of the most used command to delay for a certain amount of time is `ping`.

**Basic usage**
    
    PING -n 1 -w 1000 1.1.1.1
    
    REM the -n 1 flag means to send 1 ping request.
    REM the -w 1000 means when the IP(1.1.1.1) does not respond, go to the next command
    REM 1.1.1.1 is an non-existing IP so the -w flag can ping a delay and go to next command

   This would output the following on your batch file/console:
   
    C:\Foo\Bar\Baz>ping -n -w 1000 1.1.1.1
    
    Pinging 1.1.1.1 (Using 32 bytes of data)
    Request timed out
    
    Ping statistics for 1.1.1.1
        Packets: Sent = 2,Received = 0, Lost = 1(100% loss)
 
   **Hide the text echoed out**
   
  Just add `>nul` at the back of the command to redirect it to null.

    ping -n w 1000 1.1.1.1 >nul

  This would output nothing.



## Sleep
   ---
   **Sleep**
   ---
   On older Windows system, `timeout` is not available. However, we can use the `sleep` command.

  **Usage** 

    sleep 1
   
 Very self-explanatory; sleep for 1 second.
 However, `sleep` is a deperacted command and should be replaced by [timeout][1].

  **Availability**

  This command is available on old Windows system. Also `SLEEP.exe` is included in 2003 Resource Kit.

  To use `sleep.exe`, put the executable file to `%Windir%\System32` folder. Then you can use it as normal command.
   


  [1]: https://www.wikiod.com/batch-file/add-delay-to-batch-file#Timeout

