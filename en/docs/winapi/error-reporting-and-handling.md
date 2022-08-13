---
title: "Error reporting and handling"
slug: "error-reporting-and-handling"
draft: false
images: []
weight: 9943
type: docs
toc: true
---

Each thread will have its own last error code.  The Windows API will set the last error code on the calling thread.

You should always call the `GetLastError` function immediately after checking a Windows API function's return value.

The majority of Windows API functions set the last error code when they fail.  Some will also set the last error code when they succeed.  There are a number of functions that do not set the last error code.  Always refer to the Windows API function's documentation.

It is unsafe to use `FORMAT_MESSAGE_FROM_SYSTEM` without `FORMAT_MESSAGE_IGNORE_INSERTS` when using the `FormatMessage` function to get a description of an error code.

## Converting an error code into a message string
[`GetLastError`](https://www.wikiod.com/winapi/error-reporting-and-handling#Error reported with additional information on failure) returns a numerical error code. To obtain a descriptive error message (*e.g.*, to display to a user), you can call `FormatMessage`:

    // This functions fills a caller-defined character buffer (pBuffer)
    // of max length (cchBufferLength) with the human-readable error message
    // for a Win32 error code (dwErrorCode).
    // 
    // Returns TRUE if successful, or FALSE otherwise.
    // If successful, pBuffer is guaranteed to be NUL-terminated.
    // On failure, the contents of pBuffer are undefined.
    BOOL GetErrorMessage(DWORD dwErrorCode, LPTSTR pBuffer, DWORD cchBufferLength)
    {
        if (cchBufferLength == 0)
        {
            return FALSE;
        }

        DWORD cchMsg = FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                                     NULL,  /* (not used with FORMAT_MESSAGE_FROM_SYSTEM) */
                                     dwErrorCode,
                                     MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                                     pBuffer,
                                     cchBufferLength,
                                     NULL);
        return (cchMsg > 0);
    }

<br />

In **C++**, you can simplify the interface considerably by using the `std::string` class:

    #include <Windows.h>
    #include <exception>
    #include <stdexcept>
    #include <memory>
    #include <string>
    typedef std::basic_string<TCHAR> String;

    String GetErrorMessage(DWORD dwErrorCode)
    {
        LPTSTR psz = NULL;
        const DWORD cchMsg = FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM
                                             | FORMAT_MESSAGE_IGNORE_INSERTS
                                             | FORMAT_MESSAGE_ALLOCATE_BUFFER,
                                           NULL, // (not used with FORMAT_MESSAGE_FROM_SYSTEM)
                                           dwErrorCode,
                                           MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                                           reinterpret_cast<LPTSTR>(&psz),
                                           0,
                                           NULL);
        if (cchMsg > 0)
        {
            // Assign buffer to smart pointer with custom deleter so that memory gets released
            // in case String's c'tor throws an exception.
            auto deleter = [](void* p) { ::HeapFree(::GetProcessHeap(), 0, p); };
            std::unique_ptr<TCHAR, decltype(deleter)> ptrBuffer(psz, deleter);
            return String(ptrBuffer.get(), cchMsg);
        }
        else
        {
            throw std::runtime_error("Failed to retrieve error message string.");
        }
    }

<br/>

**NOTE:** These functions also work for [`HRESULT` values](https://www.wikiod.com/winapi/error-reporting-and-handling#Error reported as HRESULT value). Just change the first parameter from `DWORD dwErrorCode` to `HRESULT hResult`. The rest of the code can remain unchanged.

## Introduction
The Windows API is provided by means of a C-callable interface. Success or failure of an API call is reported strictly through return values. Exceptions aren't part of the documented contract (although some API **implementations** can raise [SEH](https://msdn.microsoft.com/en-us/library/windows/desktop/ms680657.aspx) exceptions, e.g. when passing a read-only *lpCommandLine* argument to [CreateProcess](https://msdn.microsoft.com/en-us/library/windows/desktop/ms682425.aspx)).

Error reporting roughly falls into one of four categories:
* [Return value only](https://www.wikiod.com/winapi/error-reporting-and-handling#Error reported by return value only)
* [Return value with additional information on failure](https://www.wikiod.com/winapi/error-reporting-and-handling#Error reported with additional information on failure)
* [Return value with additional information on failure and success](https://www.wikiod.com/winapi/error-reporting-and-handling#Error reported with additional information on failure and success)
* [`HRESULT` return value](https://www.wikiod.com/winapi/error-reporting-and-handling#Error reported as HRESULT value)

The documentation for each API call explicitly calls out, how errors are reported. Always consult the documentation.

## Error reported by return value only
Some API calls return a single failure/success flag, without any additional information (e.g. [GetObject](https://msdn.microsoft.com/en-us/library/dd144904.aspx)):

    if ( GetObjectW( obj, 0, NULL ) == 0 ) {
        // Failure: no additional information available.
    }

## Error reported with additional information on failure
In addition to a failure/success return value, some API calls also set the last error on failure (e.g. [CreateWindow](https://msdn.microsoft.com/en-us/library/windows/desktop/ms632679.aspx)). The documentation usually contains the following standard wording for this case:
> If the function succeeds, the return value is *&lt;API-specific success value&gt;*.  
> If the function fails, the return value is *&lt;API-specific error value&gt;*. To get extended error information, call [**GetLastError**](https://msdn.microsoft.com/en-us/library/windows/desktop/ms679360.aspx).

    if ( CreateWindowW( ... ) == NULL ) {
        // Failure: get additional information.
        DWORD dwError = GetLastError();
    } else {
        // Success: must not call GetLastError.
    }

**It is vital that you call `GetLastError()` IMMEDIATELY.** The last error code can be overwritten by any other function, so if there's an extra function call between the function that failed and the call to `GetLastError()`, the return from `GetLastError()` will no longer be reliable. Take extra caution when dealing with C++ constructors.

Once you get an error code, you will need to interpret it. You can get a comprehensive list of error codes on MSDN, at the [System Error Codes (Windows) page](https://msdn.microsoft.com/en-us/library/windows/desktop/ms681381(v=vs.85).aspx). Alternatively, you can look in your system header files; the file with all the error code constants is `winerror.h`. (If you have Microsoft's official SDK for Windows 8 or newer, this is in the `shared` subfolder of the include folder.)

## Notes on calling `GetLastError()` in other programming languages

### .net languages (C#, VB, etc.)
With .net, you **should not** P/Invoke to `GetLastError()` directly. This is because the .net runtime will make other Windows API calls on the same thread behind your back. For instance, the garbage collector might call `VirtualFree()` if it finds enough memory that it is no longer using, *and this can happen between your intended function call and your call to `GetLastError()`*.

Instead, .net provides the `Marshal.GetLastWin32Error()` function, which will retrieve the last error from the last P/Invoke call that you yourself made. Use this instead of calling `GetLastError()` directly.

(.net does not seem to stop you from importing `GetLastError()` anyway; I'm not sure why.)

### Go
The various facilities provided by Go for calling DLL functions (which reside in both package `syscall` and package `golang.org/x/sys/windows`) return three values: `r1`, `r2`, and `err`. `r2` is never used; you can use the blank identifier there. `r1` is the function's return value. `err` is the result of calling `GetLastError()` but converted into a type that implements `error`, so you can pass it up to calling functions to handle.

Because Go does not know when to call `GetLastError()` and when not to, it will **always** return a non-`nil` error. Therefore, the typical Go error-handling idiom

    r1, _, err := syscall.Syscall12(CreateWindowW.Addr(), ...)
    if err != nil {
        // handle err
    }
    // use r1

will not work. Instead, you must check `r1`, exactly as you would in C, and only use `err` if *that* indicates the function returned an error:

    r1, _, err := syscall.Syscall12(CreateWindowW.Addr(), ...)
    if r1 == 0 {
        // handle err
    }
    // use r1

## Error reported with additional information on failure and success
Some API calls can succeed or fail in more than one way. The APIs commonly return additional information for both successful invocations as well as errors (e.g. [CreateMutex](https://msdn.microsoft.com/en-us/library/windows/desktop/ms682411.aspx)).

    if ( CreateMutexW( NULL, TRUE, L"Global\\MyNamedMutex" ) == NULL ) {
        // Failure: get additional information.
        DWORD dwError = GetLastError();
    } else {
        // Success: Determine which mutex was returned.
        if ( GetLastError() == ERROR_ALREADY_EXISTS ) {
            // Existing mutex object returned.
        } else {
            // Newly created mutex object returned.
        }
    }

## Error reported as HRESULT value
[HRESULT](https://msdn.microsoft.com/en-us/library/cc231198.aspx)s are numeric 32-bit values, where bits or bit ranges encode well-defined information. The MSB is a failure/success flag, with the remaining bits storing additional information. Failure or success can be determined using the [FAILED](https://msdn.microsoft.com/en-us/library/windows/desktop/ms693474.aspx) or [SUCCEEDED](https://msdn.microsoft.com/en-us/library/windows/desktop/ms687197.aspx) macros. `HRESULT`s are commonly used with COM, but appear in non-COM implementations as well (e.g. [StringCchPrintf](https://msdn.microsoft.com/en-us/library/windows/desktop/ms647541.aspx)).

    const size_t cchBuf = 5;
    wchar_t buffer[cchBuf] = { 0 };
    HRESULT hr = StringCchPrintfW( buffer, cchBuf, L"%s", L"Hello, world!" );
    if ( FAILED( hr ) ) {
        // Failure: Determine specific reason.
        switch ( hr ) {
        case STRSAFE_E_INSUFFICIENT_BUFFER:
            // Buffer too small; increase buffer and retry.
            ...
        case STRSAFE_E_INVALID_PARAMETER:
            // Invalid parameter; implement custom error handling (e.g. logging).
            ...
        default:
            // Some other error code; implement custom error handling (e.g. logging).
            ...
        }
    }

