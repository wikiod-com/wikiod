---
title: "Creating easily removable runtime error checks"
slug: "creating-easily-removable-runtime-error-checks"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

This shows how a runtime error check routine of your own making can be easily incorporated so that it doesn't generate any code overhead when it is turned off.


## Trivial example
    {$DEFINE MyRuntimeCheck} // Comment out this directive when the check is no-longer required!
                             // You can also put MyRuntimeCheck in the project defines instead.
    
       function MyRuntimeCheck: Boolean;  {$IFNDEF MyRuntimeCheck} inline;  {$ENDIF}
       begin
          result := TRUE;
          {$IFDEF MyRuntimeCheck}
            // .. the code for your check goes here
          {$ENDIF}
       end;

The concept is basically this:

The defined symbol is used to turn on the use of the code.  It also stops the code being explicitly in-lined, which means it is easier to put a breakpoint into the check routine.

However, the real beauty of this construction is when you *don't* want the check anymore.  By commenting out the ***$DEFINE*** (put '//' in-front of it) you will not only remove the check code, but you will also *switch on* the ***inline*** for the routine and thus remove any overheads from all the places where you invoked the routine!  The compiler will remove all traces of your check entirely (assuming that inlining itself is set to "On" or "Auto", of course).

The example above is essentially similar to the concept of "assertions", and your first line could set the result to TRUE or FALSE as appropriate to the usage.  

But you are now also free to use this manner of construction for code that does trace-logging, metrics, whatever.  For example:

    
       procedure MyTrace(const what: string);  {$IFNDEF MyTrace} inline;  {$ENDIF}
       begin
          {$IFDEF MyTrace}
            // .. the code for your trace-logging goes here
          {$ENDIF}
       end;
    ...
    MyTrace('I was here');   // This code overhead will vanish if 'MyTrace' is not defined.
    MyTrace( SomeString );   // So will this.



