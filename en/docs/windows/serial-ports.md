---
title: "Serial Ports"
slug: "serial-ports"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

Using the serial ports on Windows can be a bit complex.  This documentation section will, in time, explain all about the use of DCBs, CreateFile(), port events, and asynchronous serial communication.



## Listing all serial ports.
Getting all serial ports information from Windows is often necessary, you may want to give the user a choice of ports to open, or check if your device is connected. 

In addition, some ports just cannot be opened using a "COMx" string and need to be opened using a device name.  Some older versions of Windows cannot open ports named 'COMxx' when the port number is 10 or higher.  So, using the device number is a sensible way to identify your com port with Windows.

Serial port information is held in the registry under the key `HKEY_LOCAL_MACHINE\HARDWARE\DEVICEMAP\SERIALCOMM`

This example in C shows how to list all the serial ports and how one can get to the device name to use for the call to CreateFile.

The method is quite simple:

 - Open the registry at `HKEY_LOCAL_MACHINE\HARDWARE\DEVICEMAP\SERIALCOMM`
 - Enumerate all keys we find there.  The device name is the key name, and the 'display name' is the key value.  

As is often the case with Windows programming, most of the effort is spent on error checking. It may seem a bit silly, but there's no safe way to avoid it.

    #define WIN32_LEAN_AND_MEAN  // excludes stuff frokm windows.h that we won't need here.
    #include <Windows.h>
    #include <string.h>
    #include <tchar.h>
    #include <malloc.h>
    
    void ShowErrorFromLStatus(LSTATUS lResult)
    {
        LPTSTR psz;
        FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
            NULL,
            lResult,
            0,
            (LPTSTR)&psz,
            1024,
            NULL);
    
        _tprintf(_T("Windows reports error: (0x%08X): %s\n"), lResult, (psz) ? psz : _T("(null)"));
        if (psz)
        {
            LocalFree(psz);
        }
    }
    
    int main()
    {
        DWORD nValues, nMaxValueNameLen, nMaxValueLen;
        HKEY hKey = NULL;
        LPTSTR szDeviceName = NULL;
        LPTSTR szFriendlyName = NULL;
        DWORD dwType = 0;
        DWORD nValueNameLen = 0;
        DWORD nValueLen = 0;
        DWORD dwIndex = 0;
    
        LSTATUS lResult = RegOpenKeyEx(HKEY_LOCAL_MACHINE, L"HARDWARE\\DEVICEMAP\\SERIALCOMM", 0, KEY_READ, &hKey);
        if (ERROR_SUCCESS != lResult)
        {
            printf("Failed to open key \'HARDWARE\\DEVICEMAP\\SERIALCOMM\' \n");
            ShowErrorFromLStatus(lResult);
            return 1;
        }
    
        lResult = RegQueryInfoKey(hKey, NULL, NULL, NULL, NULL, NULL, NULL,
            &nValues, &nMaxValueNameLen, &nMaxValueLen, NULL, NULL);
    
        if (ERROR_SUCCESS != lResult)
        {
            _tprintf(_T("Failed to RegQueryInfoKey()\n"));
            ShowErrorFromLStatus(lResult);
            RegCloseKey(hKey);
            return 2;
        }
    
        szDeviceName = (LPTSTR)malloc(nMaxValueNameLen + sizeof(TCHAR));
        if (!szDeviceName)
        {
            _tprintf(_T("malloc() fail\n"));
            RegCloseKey(hKey);
            return 3;
        }
    
        szFriendlyName = (LPTSTR)malloc(nMaxValueLen + sizeof(TCHAR));
        if (!szFriendlyName)
        {
            free(szDeviceName);
            _tprintf(_T("malloc() fail\n"));
            RegCloseKey(hKey);
            return 3;
        }
    
        _tprintf(_T("Found %d serial device(s) registered with PnP and active or available at the moment.\n"), nValues);
    
        for (DWORD dwIndex = 0; dwIndex < nValues; ++dwIndex)
        {
            dwType = 0;
            nValueNameLen = nMaxValueNameLen + sizeof(TCHAR);
            nValueLen = nMaxValueLen + sizeof(TCHAR);
    
            lResult = RegEnumValueW(hKey, dwIndex, 
                szDeviceName, &nValueNameLen,
                NULL, &dwType, 
                (LPBYTE)szFriendlyName, &nValueLen);
    
            if (ERROR_SUCCESS != lResult || REG_SZ != dwType)
            {
                _tprintf(_T("SerialPortEnumerator::Init() : can't process registry value, index: %d\n"), dwIndex);
                ShowErrorFromLStatus(lResult);
                continue;
            }
            _tprintf(_T("Found port \'%s\': Device name for CreateFile(): \'\\.%s\'\n"), szFriendlyName, szDeviceName);
        }
    
        free(szDeviceName);
        free(szFriendlyName);
        RegCloseKey(hKey);
        return 0;
    }

Program output on my laptop:

    Found 1 serial device(s) registered with PnP and active or available at the moment.
    Found port 'COM23': Device name for CreateFile(): '\.\Device\BthModem0'


