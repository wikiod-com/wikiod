---
title: "Getting started with visual-foxpro"
slug: "getting-started-with-visual-foxpro"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
In all languages traditionally the first example is printing "Hello World". Probably doing that is easiest in VFP:

    ? "Hello World"

## Add Global Error Handler
A simple way to catch unhandled errors (exceptions) in a VFP application is to use the ON ERROR command near the beginning of your main program.

The following ON ERROR command calls a method in the current program called "errorHandler". The values returned by ERROR (the VFP Error Number), MESSAGE (the VFP Error Message), PROGRAM (name of the currently executing program) and LINENO (the line number of the error) are passed to the errorHandler method.

    ON ERROR DO errorHandler WITH ERROR(), MESSAGE(), PROGRAM(), LINENO()

A simple errorHandler method may look like the following.

    PROCEDURE errorHandler
        LPARAMETERS tnVFPErrorNumber, tcVFPErrorMessage, tcProcWithError, tnLineNumber

        STORE 'Error message: ' + tcVFPErrorMessage + CHR(13) + ;
            'Error number: ' + TRANSFORM(tnVFPErrorNumber) + CHR(13) + ;
            'Procedure with error: ' + tcProcWithError + CHR(13) + ;
            'Line number of error: ' + TRANSFORM(tnLineNumber) TO lcDetails

        MESSAGEBOX(lcDetails, 16, "Unhandled Exception")

        ON ERROR *
        ON SHUTDOWN
        CLEAR EVENTS

        QUIT
    ENDPROC

You can also change and restore the error handler in between. For example, at one point you want to open all tables in a folder exclusively, and if you can't you don't want to continue:

    procedure DoSomethingWithExclusiveLock(tcFolder)
    local lcOldError, llInUse, ix && by default these variables have a value of .F.
    lcError = on('error') && save current handler
    on error llInUse = .T.  && new handler
    local array laTables[1]  
    for ix=1 to adir(laTables, addbs(m.tcFolder) + '*.dbf'))
       use (addbs(m.tcFolder)+laTables[m.ix,1]) in 0 exclusive
    endfor
    on error &lcError && restore old handler
    if m.llInUse && couldn't get exclusive lock on all tables
       close databases all
       return
    endif
    * do whatever
    endproc

> Tip: Sometimes, especially during debugging, you would want to restore
> default error handler which allows you to break and step into the code where the error has
> occurred, then anywhere before where you got the error, temporarily
> adding:
> 
>     on error
> 
> would do this.

## Installation or Setup
Detailed instructions on getting visual-foxpro set up or installed.

