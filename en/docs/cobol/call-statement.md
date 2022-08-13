---
title: "CALL statement"
slug: "call-statement"
draft: false
images: []
weight: 9917
type: docs
toc: true
---

The COBOL CALL statement provides access to compiled library routines.

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/USCTk.png

## CALL statement
COBOL can use static linkage for the following statement. GnuCOBOL uses dynamic linkage by default for all external symbols known at compile time, even when the symbol is a literal:

    CALL "subprogram" USING a b c *> run a (possibly static linked) sub program
                                  *> passing three fields

    CALL some-prog USING a b c    *> some-prog is a PIC X item and can be changed
                                  *> at run-time to do a dynamic lookup




This statement forces compile time link edit resolution. *(Non standard, syntax extension)*:

    CALL STATIC "subprogram" USING a b c

Fields in COBOL can be passed `BY REFERENCE` (the default, until overridden - overrides are `sticky` in a left to right order), `BY CONTENT` (a copy is passed BY REFERENCE), or in some cases directly `BY VALUE`:

    CALL "calculation" USING BY REFERENCE a BY VALUE b BY CONTENT c RETURNING d
        ON EXCEPTION DISPLAY 'No linkage to "calculation"' UPON SYSERR
    END-CALL 


COBOL is designed to be a `BY REFERENCE` language, so using `BY VALUE` can present issues. For instance, literal numerics have no explicit type and the COBOL spec has no explicit type promotion rules. Therefore developers have to worry about call frame setup with `BY VALUE` of literals.

See http://open-cobol.sourceforge.net/faq/index.html#call for more details.

## SLEEPY TIME
CALL is also a way to extend COBOL functionality, and also to allow the reusability of code. It can also give access to "system" functionality.

This example illustrates ways to provide "sleep" functionality to IBM Mainframe COBOLs. Bear in mind that the requirement to do so is rare to the extent that usually when someone thinks they need to "sleep" for some reason, it is the wrong thing to do.

ILBOWAT0 is from the old COBOL-specific runtime era on  Mainframes. BXP1SLP and BXP4SLP are Unix System Services (USS) routines which can be used by any language. Effectively they are Unix "sleep" requests.

The current IBM Mainframe Runtime (Language Environment (LE)) provides for inter-language communication, and the CEE3DLY LE services is shown in another example, https://www.wikiod.com/cobol/call-statement#Using z/OS Language Environment thread delay service

ILBOWAT0 has been around for a very long time (perhaps more than 40 years), and you may still come across it. It's use should be replaced by CEE3DLY or BXP1SLP, whichever is the more appropriate for the particular requirement.

Sometimes you need to cause a program to sleep, or cause a Job to sleep for a while (after an FTP or NDM step), which are usually run as separate jobs, and you would need to sleep/loop looking for the resulting datasets.
 
Here is a cute little COBOL program to do said task, calling the COBOL sleep programs available in OS/VS and perhaps other legacy and current mainframe operating environments.

           IDENTIFICATION DIVISION.
           PROGRAM-ID.  SLEEPYTM.
           ENVIRONMENT DIVISION.
           DATA DIVISION.
           WORKING-STORAGE SECTION.
           01  WAIT-PARM.
               05  WAIT-TIME            PIC S9(8) COMP VALUE 90.
               05  WAIT-RESPONSE        PIC S9(8) COMP VALUE 0.
               05  WAIT-PROGRAM-24BIT   PIC  X(8)      VALUE 'ILBOWAT0'.
               05  WAIT-PROGRAM-31BIT   PIC  X(8)      VALUE 'BPX1SLP '.
               05  WAIT-PROGRAM-64BIT   PIC  X(8)      VALUE 'BPX4SLP '.

           PROCEDURE DIVISION.
           GENESIS.
               DISPLAY 'START CALLING WAIT PROGRAM'
               CALL WAIT-PROGRAM-24BIT USING WAIT-TIME WAIT-RESPONSE
               DISPLAY 'END   CALLING WAIT PROGRAM'
               GOBACK
    PERIOD     .

## microfocus way
For Microfocus, it uses the "SleepEx" API. As an example;

    environment division.            
    special-names.                   
        call-convention 74 is winAPI.
             :
             :
    01  wSleep-time              pic 9(8) comp-5.
    01  wSleep-ok                pic 9(8) comp-5.
             :
             :
    move 10000 to wSleep-time  *>10seconds
    call winAPI "SleepEx" using by value wSleep-time
                            by value 0 size 4   
                  returning wSleep-ok 
    end-call.

## Using z/OS Language Environment thread delay service
You can call the CEE3DLY service in 24- 31- or 64- bit mode to delay a task to the nearest second.  It is CICS save and will only delay the thread.

An example:

   

        IDENTIFICATION DIVISION.
        PROGRAM-ID.  SLEEPYTM.
        ENVIRONMENT DIVISION.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  WAIT-PARM.
          05  WAIT-SECS            PIC S9(8) COMP VALUE 90.
          05  WAIT-FC              PIC X(12).
    
        PROCEDURE DIVISION.

          CALL CEE3DLY USING WAIT-SECS WAIT-FC
          
          GOBACK.

You can see more detail here:
 
[IBM Language Environment Callable Services - Sleep][1]


  [1]: http://www.ibm.com/support/knowledgecenter/SSLTBW_1.13.0/com.ibm.zos.r13.ceea300/clc3dly.htm

