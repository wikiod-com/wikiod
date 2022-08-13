---
title: "File Management"
slug: "file-management"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Create a file and write to it
This example creates a new file named "NewFile.txt", then writes "Hello World!" to its body. If the file already exists, `CreateFile` will fail and no data will be written.
See the `dwCreationDisposition` parameter in the [CreateFile documentation][1] if you don't want the function to fail if the file already exists.

    #include <Windows.h>
    #include <string>
    
    int main()
    {
       // Open a handle to the file
       HANDLE hFile = CreateFile(
          L"C:\\NewFile.txt",     // Filename
          GENERIC_WRITE,          // Desired access
          FILE_SHARE_READ,        // Share mode
          NULL,                   // Security attributes
          CREATE_NEW,             // Creates a new file, only if it doesn't already exist
          FILE_ATTRIBUTE_NORMAL,  // Flags and attributes
          NULL);                  // Template file handle

       if (hFile == INVALID_HANDLE_VALUE)
       {
          // Failed to open/create file
          return 2;
       }
    
       // Write data to the file
       std::string strText = "Hello World!"; // For C use LPSTR (char*) or LPWSTR (wchar_t*)
       DWORD bytesWritten;
       WriteFile(
          hFile,            // Handle to the file
          strText.c_str(),  // Buffer to write
          strText.size(),   // Buffer size
          &bytesWritten,    // Bytes written
          nullptr);         // Overlapped
    
       // Close the handle once we don't need it.
       CloseHandle(hFile);
    }

----------

API Reference:
--------------
- [MSDN `CreateFile`][1]
- [MSDN `WriteFile`][2]

[1]: https://msdn.microsoft.com/en-us/library/windows/desktop/aa363858.aspx
[2]: https://msdn.microsoft.com/en-us/library/windows/desktop/aa365747.aspx

