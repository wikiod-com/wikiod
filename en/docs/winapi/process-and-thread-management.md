---
title: "Process and Thread Management"
slug: "process-and-thread-management"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Create a process and check its exit code
This example starts Notepad, waits for it to be closed, then gets its exit code.

    #include <Windows.h>
    
    int main()
    {
       STARTUPINFOW si = { 0 };
       si.cb = sizeof(si);
       PROCESS_INFORMATION pi = { 0 };
    
       // Create the child process
       BOOL success = CreateProcessW(
          L"C:\\Windows\\system32\\notepad.exe",  // Path to executable
          NULL,                                   // Command line arguments
          NULL,                                   // Process attributes
          NULL,                                   // Thread attributes
          FALSE,                                  // Inherit handles
          0,                                      // Creation flags
          NULL,                                   // Environment
          NULL,                                   // Working directory
          &si,                                    // Startup info
          &pi);                                   // Process information
    
       if (success)
       {
          // Wait for the process to exit
          WaitForSingleObject(pi.hProcess, INFINITE);
    
          // Process has exited - check its exit code
          DWORD exitCode;
          GetExitCodeProcess(pi.hProcess, &exitCode);
    
          // At this point exitCode is set to the process' exit code
    
          // Handles must be closed when they are no longer needed
          CloseHandle(pi.hThread);
          CloseHandle(pi.hProcess);
       }
    }

References (MSDN):

* [`CreateProcess`](https://msdn.microsoft.com/en-us/library/windows/desktop/ms682425.aspx)
* [`WaitForSingleObject`](https://msdn.microsoft.com/en-us/library/windows/desktop/ms687032.aspx)
* [`GetExitCodeProcess`](https://msdn.microsoft.com/en-us/library/windows/desktop/ms683189.aspx)
* [`CloseHandle`](https://msdn.microsoft.com/en-us/library/windows/desktop/ms724211.aspx)

## Create a new thread
    #include <Windows.h>
    
    DWORD WINAPI DoStuff(LPVOID lpParameter)
    {
        // The new thread will start here
        return 0;
    }
    
    int main()
    {
        // Create a new thread which will start at the DoStuff function
        HANDLE hThread = CreateThread(
            NULL,    // Thread attributes
            0,       // Stack size (0 = use default)
            DoStuff, // Thread start address
            NULL,    // Parameter to pass to the thread
            0,       // Creation flags
            NULL);   // Thread id
        if (hThread == NULL)
        {
            // Thread creation failed.
            // More details can be retrieved by calling GetLastError()
            return 1;
        }
    
        // Wait for thread to finish execution
        WaitForSingleObject(hThread, INFINITE);
    
        // Thread handle must be closed when no longer needed
        CloseHandle(hThread);

        return 0;
    }

Note that the CRT also provides the [`_beginthread` and `_beginthreadex`][4] APIs for creating threads, which are not shown in this example. The following link discusses [the differences between these APIs and the `CreateThread` API][5].

References (MSDN):

* [`CreateThread`][1]
* [`WaitForSingleObject`][2]
* [`CloseHandle`][3]
* [`_beginthread`, `_beginthreadex`][4]

   [1]: https://msdn.microsoft.com/en-us/library/windows/desktop/ms682453(v=vs.85).aspx
   [2]: https://msdn.microsoft.com/en-us/library/windows/desktop/ms687032.aspx
   [3]: https://msdn.microsoft.com/en-us/library/windows/desktop/ms724211.aspx
   [4]: https://msdn.microsoft.com/en-us/library/kdzttdcb.aspx
   [5]: http://stackoverflow.com/q/331536/389966

