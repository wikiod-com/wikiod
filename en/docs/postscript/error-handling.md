---
title: "Error Handling"
slug: "error-handling"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Syntax
* { *-code-* } stopped { *-error-* }{ *-no-error-* } ifelse  % error catching frame

* $error /errorname get  % stackunderflow typecheck rangecheck etc   
$error /newerror get  % bool. put false to deactivate error  
$error /ostack get  % copy of operand stack at point of error

* errordict /stackoverflow { *-additional-code-*  /stackoverflow signalerror} put  
% execute additional code on types of errors (here, the /stackoverflow error).

There are two levels to error handling in postscript. This dichotomy applies both to the way the interpreter handles errors as well as the means available to the user (programmer) to control the handling.  

The lower level is an unusual control structure `stop ... stopped`. `stopped` behaves much like a looping construct in that it establishes a mark on the execution stack that can be jumped-to if the `exit` operator (for a loop) or `stop` operator (for a `stopped`-context) is called. Unlike a looping construct, `stopped` yields a Boolean on the stack indicating whether `stop` was called (otherwise the procedure passed to `stopped` is known to have executed to completion.  

When a PostScript error occurs, like `stackunderflow` maybe, the interpreter looks up the error's name in `errordict` which lives in `systemdict`. If the user has not replaced the procedure in `errordict`, then the default error procedure will take snapshots of all the stack and place them in `$error`, another dictionary in `systemdict`. Finally, the default procedure will call `stop` which pops the user program from the exec stack and executes the interpreter's error printing procedure called `handleerror` in `errordict`.  

So using all of this knowledge, you can *catch* errors by wrapping a section of code in `{ ... } stopped`. You can *rethrow* an error by calling `stop`. You can determine what type of error occurred with `$error /errorname get`.  

You can also change the default behavior for a specific type of error by replacing the procedure with that name in `errordict`. Or change the format of printing the error report by replacing `/handleerror` in `errordict`. 



## Sequence of events when an error is signaled
The sequence for an error is usually:

1. error is triggered by looking up the error name in `errordict` and executing this procedure. 
2. the `errordict` procedure calls `signalerror`, passing it the error name. 
3. `signalerror` takes snapshots of the stacks, saving the snapshots in `$error`, and then calls `stop`.
4. `stop` pops the exec stack until the nearest enclosing stopped context established by the stopped operator.
5. if the program has not established its own stopped context to catch the error, it will be caught by an outer-level `stopped { errordict /handleerror get exec } if` which was called by the startup code to bracket the whole user program.
6. `handleerror` uses the information in `$error` to print an error report.


## Signalling (throwing) an error
Most of the tools are standardized with the exception of the name of the operator to throw an error. In Adobe interpreters, it is called `.error`. In ghostscript, it is called `signalerror`. So with this line you can use `signalerror` in postscript code for Adobe interpreters or ghostscript or xpost.

    /.error where {pop /signalerror /.error load def} if

*commandname* *errorname* &nbsp; &nbsp; **signalerror**  &nbsp; &nbsp; &mdash;  
*Take snapshots of the stack in `$error`, then `stop`.*

Eg.

    % my proc only accepts integer
    /proc {
        dup type /integertype ne {
            /proc cvx /typecheck signalerror
        } if
        % ... rest of proc ...
    } def



## Is there a currentpoint?
Yield `true` on the stack if `currentpoint` executes successfully, or `false` if it signals a `/nocurrentpoint` error.

    {currentpoint pop pop} stopped not  % bool

## Catching an error
Since the final action of the default error handler is to call `stop`, you can catch errors from operators by enclosing code in a `{ ... } stopped` construct.

    {
        0 array
        1 get
    } stopped {
        $error /errorname get =
    } if

will print "`rangecheck`", the error signaled by `get` when the index is outside the acceptable range for that array.

## Re-throwing errors
This snippet implements a procedure which behaves like a postscript looping operator. If the user `proc` calls `exit`, it catches the `invalidexit` error to fix the dictstack for the `end` at the end. Any other error except `invalidexit` is re-thrown by calling `stop`.

    % array n proc  .  -
    % Like `forall` but delivers length=n subsequences produced by getinterval
    /fortuple { 4 dict begin
        0 {offset proc n arr} {exch def} forall
        /arr load length n idiv
        {
            {
                /arr load offset n getinterval
                [ /proc load currentdict end /begin cvx ] cvx exec
                /offset offset n add def
            } stopped {
                $error /errorname get /invalidexit eq
                { 1 dict begin exit }{ stop } ifelse
            } if
        } repeat
    end
    } def

    %[ 0 1 10 {} for ] 3 {} fortuple pstack clear ()=


