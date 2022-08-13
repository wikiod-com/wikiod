---
title: "Windows Services"
slug: "windows-services"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Check if a service is installed
This example show how you can check if a service already exists (*i.e.*, is installed on the machine) or not. This code requires only the lowest privileges necessary, so each process can perform the check, no matter what level of security it is running at.

    #define UNICODE
    #define _UNICODE
    #include <Windows.h>
    #include <string>
    #include <iostream>

    enum Result
    {
       unknown,
       serviceManager_AccessDenied,
       serviceManager_DatabaseDoesNotExist,
       service_AccessDenied,
       service_InvalidServiceManagerHandle,
       service_InvalidServiceName,
       service_DoesNotExist,
       service_Exist
    };

    Result ServiceExists(const std::wstring &serviceName)
    {
       Result r = unknown;

       // Get a handle to the SCM database
       SC_HANDLE manager = OpenSCManager(NULL, SERVICES_ACTIVE_DATABASE, GENERIC_READ);
       
       if (manager == NULL)
       {
          DWORD lastError = GetLastError();
          
          // At this point, we can return directly because no handles need to be closed.
          if (lastError == ERROR_ACCESS_DENIED)
             return serviceManager_AccessDenied;
          else if (lastError == ERROR_DATABASE_DOES_NOT_EXIST)
             return serviceManager_DatabaseDoesNotExist;
          else
             return unknown;
       }

       SC_HANDLE service = OpenService(manager, serviceName.c_str(), GENERIC_READ);
       
       if (service == NULL)
       {
          DWORD error = GetLastError();
          
          if (error == ERROR_ACCESS_DENIED)
             r = service_AccessDenied;
          else if (error == ERROR_INVALID_HANDLE)
             r = service_InvalidServiceManagerHandle;
          else if (error == ERROR_INVALID_NAME)
             r = service_InvalidServiceName;
          else if (error == ERROR_SERVICE_DOES_NOT_EXIST)
             r = service_DoesNotExist;
          else
             r = unknown;
       }
       else
          r = service_Exist;

       if (service != NULL)
          CloseServiceHandle(service);

       if (manager != NULL)
          CloseServiceHandle(manager);
       
       return r;
    }

    int main()
    {
       std::wstring serviceName = L"MSSQL$SQLEXPRESS";  // name of the service to check
       Result result = ServiceExists(serviceName);
       if (result == service_Exist)
          std::wcout << L"The service '" << serviceName << "' exists." << std::endl;
       else if (result == service_DoesNotExist)
          std::wcout << L"The service '" << serviceName << "' does not exist." << std::endl;
       else
          std::wcout << L"An error has occurred, and it could not be determined whether the service '" << serviceName << "' exists or not." << std::endl;
    }

----------

API Reference:
----------
- [MSDN OpenSCManager][1]
- [MSDN OpenService][2]
- [MSDN CloseServiceHandle][3]

[1]: https://msdn.microsoft.com/de-de/library/windows/desktop/ms684323.aspx
[2]: https://msdn.microsoft.com/de-de/library/windows/desktop/ms684330.aspx
[3]: https://msdn.microsoft.com/de-de/library/windows/desktop/ms682028.aspx

