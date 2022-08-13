---
title: "Loops"
slug: "loops"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Delphi language provide 3 types of loop

`for` - iterator for fixed sequence over integer, string, array or enumeration

`repeat-until` - quit condition is checking after each turn, loop executing at minimum once tmeeven

`while do` - do condition is checking before each turn, loop could be never executed

## Syntax
 - for OrdinalVariable := LowerOrdinalValue to UpperOrdinalValue do begin {loop-body} end;
 - for OrdinalVariable := UpperOrdinalValue downto LowerOrdinalValue do begin {loop-body} end;
 - for EnumerableVariable in Collection do begin {loop-body} end;
 - repeat {loop-body} until {break-condition};
 - while {condition} do begin {loop-body} end;

## Break and Continue in Loops

    program ForLoopWithContinueAndBreaks;
    
    {$APPTYPE CONSOLE}
    
    var
      var i : integer;
    begin
      for i := 1 to 10 do
        begin
          if i = 2 then continue; (* Skip this turn *)
          if i = 8 then break;    (* Break the loop *)
          WriteLn( i );
        end;
      WriteLn('Finish.');
    end.

**Output:**
> 1<br/>
> 3<br/>
> 4<br/>
> 5<br/>
> 6<br/>
> 7<br/>
> Finish.

<!-- end version if -->

## Repeat-Until
    program repeat_test;

    {$APPTYPE CONSOLE}

    var s : string;
    begin
      WriteLn( 'Type a words to echo. Enter an empty string to exit.' );
      repeat
        ReadLn( s );
        WriteLn( s );
      until s = '';
    end.

This short example print on console `Type a words to echo. Enter an empty string to exit.`, wait for user type, echo it and waiting input again in infinite loop - until user entering the empty string.

## While do
    program WhileEOF;
    {$APPTYPE CONSOLE}
    uses SysUtils;
    
    const cFileName = 'WhileEOF.dpr';
    var F : TextFile;
    s : string;
    begin
      if FileExists( cFileName )
        then
          begin
            AssignFile( F, cFileName );
            Reset( F );

            while not Eof(F) do
              begin
                ReadLn(F, s);
                WriteLn(s);
              end;

            CloseFile( F );
          end
        else
          WriteLn( 'File ' + cFileName +  ' not found!' );
    end.

This example print to console the text content of `WhileEOF.dpr` file using `While not(EOF)` condition. If file is empty then `ReadLn-WriteLn` loop is not executed.

