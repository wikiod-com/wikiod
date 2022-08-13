---
title: "Time intervals measurement"
slug: "time-intervals-measurement"
draft: false
images: []
weight: 9929
type: docs
toc: true
---

## Using Windows API GetTickCount
The Windows API `GetTickCount` function returns the number of milliseconds since the system (computer) was started. The simplest example follows:

    var
      Start, Stop, ElapsedMilliseconds: cardinal;
    begin
      Start := GetTickCount;
      // do something that requires measurement
      Stop := GetTickCount;
      ElapsedMillseconds := Stop - Start;
    end;

Note that `GetTickCount` returns 32-bit `DWORD` so it wraps every 49.7 days. To avoid wrapping, you may either use `GetTickCount64` (available since Windows Vista) or special routines to calculate tick difference:

    function TickDiff(StartTick, EndTick: DWORD): DWORD;
    begin
      if EndTick >= StartTick
        then Result := EndTick - StartTick
        else Result := High(NativeUInt) - StartTick + EndTick;
    end;
    
    function TicksSince(Tick: DWORD): DWORD;
    begin
      Result := TickDiff(Tick, GetTickCount);
    end;

Anyway these routines will return incorrect results if the interval of two subsequent calls of `GetTickCount` exceeds the 49.7 day boundary.

To convert milliseconds to seconds example:

    var
      Start, Stop, ElapsedMilliseconds: cardinal;
    begin
      Start := GetTickCount;
      sleep(4000); // sleep for 4 seconds
      Stop := GetTickCount;
      ElapsedMillseconds := Stop - Start;
      ShowMessage('Total Seconds: '
          +IntToStr(round(ElapsedMilliseconds/SysUtils.MSecsPerSec))); // 4 seconds
    end;




## Using TStopwatch record
Recent versions of Delphi ships with the [TStopwatch][1] record which is for time interval measurement. Example usage:

    uses
      System.Diagnostics;
    
    var
      StopWatch: TStopwatch;
      ElapsedMillseconds: Int64;
    begin
      StopWatch := TStopwatch.StartNew;
      // do something that requires measurement
      ElapsedMillseconds := StopWatch.ElapsedMilliseconds;
    end;


  [1]: http://docwiki.embarcadero.com/Libraries/en/System.Diagnostics.TStopwatch

