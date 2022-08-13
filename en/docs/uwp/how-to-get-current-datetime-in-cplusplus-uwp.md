---
title: "How to get current DateTime in C++ UWP"
slug: "how-to-get-current-datetime-in-c++-uwp"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

The documentation for the `DateTime::UniversalTime` states:

*"A 64-bit signed integer that represents a point in time as the number of 100-nanosecond intervals prior to or after midnight on January 1, 1601 (according to the Gregorian Calendar)."*

This is the same as the Win32 `FILETIME`struct which you need to convert to a 100-nanosecond long long value and set it in the `DateTime::UniversalTime` field.


## GetCurrentDateTime()
    #include <windows.h>

    static Windows::Foundation::DateTime GetCurrentDateTime() {
        // Get the current system time
        SYSTEMTIME st;
        GetSystemTime(&st);
    
        // Convert it to something DateTime will understand
        FILETIME ft;
        SystemTimeToFileTime(&st, &ft);
    
        // Conversion to DateTime's long long is done vie ULARGE_INTEGER
        ULARGE_INTEGER ui;
        ui.LowPart = ft.dwLowDateTime;
        ui.HighPart = ft.dwHighDateTime;
    
        DateTime currentDateTime;
        currentDateTime.UniversalTime = ui.QuadPart;
        return currentDateTime;
    }


