---
title: "Running other programs"
slug: "running-other-programs"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## CreateProcess
Following function encapsulates code for using `CreateProcess` Windows API for launching other programs.

It is configurable and can wait until calling process finishes or return immediately. 

Parameters:

 - `FileName` - full path to executable
 - `Params` - command line parameters or use empty string
 - `Folder` - working folder for called program - if empty path will be extracted from `FileName`
 - `WaitUntilTerminated` - if true function will wait for process to finish execution
 - `WaitUntilIdle` - if true function will call [WaitForInputIdle][1] function and wait until the specified process has finished processing its initial input and until there is no user input pending
 - `RunMinimized` - if true process will be run minimized
 - `ErrorCode` - if function fails this will contain encountered Windows Error Code


    function ExecuteProcess(const FileName, Params: string; Folder: string; WaitUntilTerminated, WaitUntilIdle, RunMinimized: boolean;
      var ErrorCode: integer): boolean;
    var
      CmdLine: string;
      WorkingDirP: PChar;
      StartupInfo: TStartupInfo;
      ProcessInfo: TProcessInformation;
    begin
      Result := true;
      CmdLine := '"' + FileName + '" ' + Params;
      if Folder = '' then Folder := ExcludeTrailingPathDelimiter(ExtractFilePath(FileName));
      ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
      StartupInfo.cb := SizeOf(StartupInfo);
      if RunMinimized then
        begin
          StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
          StartupInfo.wShowWindow := SW_SHOWMINIMIZED;
        end;
      if Folder <> '' then WorkingDirP := PChar(Folder)
      else WorkingDirP := nil;
      if not CreateProcess(nil, PChar(CmdLine), nil, nil, false, 0, nil, WorkingDirP, StartupInfo, ProcessInfo) then
        begin
          Result := false;
          ErrorCode := GetLastError;
          exit;
        end;
      with ProcessInfo do
        begin
          CloseHandle(hThread);
          if WaitUntilIdle then WaitForInputIdle(hProcess, INFINITE);
          if WaitUntilTerminated then
            repeat
              Application.ProcessMessages;
            until MsgWaitForMultipleObjects(1, hProcess, false, INFINITE, QS_ALLINPUT) <> WAIT_OBJECT_0 + 1;
          CloseHandle(hProcess);
        end;
    end;
    

Usage of above function

    var
      FileName, Parameters, WorkingFolder: string;
      Error: integer;
      OK: boolean;
    begin
      FileName := 'C:\FullPath\myapp.exe';
      WorkingFolder := ''; // if empty function will extract path from FileName
      Parameters := '-p'; // can be empty 
      OK := ExecuteProcess(FileName, Parameters, WorkingFolder, false, false, false, Error);
      if not OK then ShowMessage('Error: ' + IntToStr(Error));
    end;

[CreateProcess documentation][2]


  [1]: https://msdn.microsoft.com/en-us/library/windows/desktop/ms687022(v=vs.85).aspx
  [2]: http://msdn.microsoft.com/en-us/library/windows/desktop/ms682425%28v=vs.85%29.aspx

