---
title: "Migrating ISAPI MFC extension (C++) VS2005 DLL project to VS2015."
slug: "migrating-isapi-mfc-extension-c++-vs2005-dll-project-to-vs2015"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

You may have seen several websites that will show how to create an ISAPI extension project but none of them will demonstrate how to migrate the existing legacy ISAPI extension (VS2005) project to VS2015. I had faced similar issue while I was working on one of such requirement. This article demonstrates the experimental waythat I had took to solve my issue.

I was working on a migration task where I came across a project. The legacy ISAPI project was an MFC extension DLL project built in VS2005 and after googling few things I came to know that the MFC ISAPI classes (CHttpServerContext, CHttpServer etc.) are not being shipped after VS2005. Microsoft recommends using ISAPI Entry-Point functions from the Microsoft Internet Information Services (IIS) software development kit (SDK) instead of MFC ISAPI classes. I thought it might require a complete rewriting of the project. However fortunately I took an experimental way which worked in my case. I thought to write this article which might help someone to solve the similar issue.

Purpose of this topic is to demonstrate the changes required for removing dependency of MFC from MFC ISAPI Extension DLL without rewriting of complete code. Example shown above doesn't contain whole implementation, it is just for demonstration purpose.

## Example:
Purpose of the project: Download particular file based on message id sent as a request from client to server. File will be created on server and transmitted to client.

Here, I haven't share the whole code but shown what is necessary as the purpose is to demonstrate the changes required for migration.
  
To make the changes. I took the reference of existing afxisapi.h, afxisapi.inl and HttpExt.h which are being shipped with VS2005.

-    Use EXTENSION_CONTROL_BLOCK instead of CHttpServerContext. EXTENSION_CONTROL_BLOCK is a main structure which is used by IIS and the ISAPI extension to exchange the information. It contains all the information about new request.

-    Remove the dependency of CHttpServer class from your MFC extension class. Remove http command parse message mapping if it is used in your project. Macros like ON_PARSE_COMMAND, BEGIN_PARSE_MAP, END_PARSE_MAP and DEFAULT_PARSE_COMMAND were being removed after VS2005. 

-    Implement your own GetExtensionVersion, TerminateExtension and HttpExtensionProc function in your MFC extension class which was previously derived from CHttpSever class. These function

-    GetExtensionVersion()
The GetExtensionVersion function is the first entry-point function in IIS. This function allows your ISAPI extension to register its version information with IIS.

    BOOL  CISAPIExtension::GetExtensionVersion( HSE_VERSION_INFO* pVer )
    {
        ISAPIVERIFY(
           ::LoadString( AfxGetResourceHandle(),
                              IDS_SERVER,
                              sz,
                              HSE_MAX_EXT_DLL_NAME_LEN) );

           _tcscpy_s( pVer->lpszExtensionDesc, ( strlen( sz ) + 1U ), sz )
    }

-    HttpExtensionProc() 
: The HttpExtensionProc function is the main entry point for an ISAPI extension called by IIS. It exposes methods that IIS uses to access the functionality exposed by the extension. Extract the request information from extension control block and do your processing.
    
    DWORD CISAPIExtension::HttpExtensionProc(LPEXTENSION_CONTROL_BLOCK pECB)
    {
    try
    {
        if (strcmp(pECB->lpszMethod, "POST") == 0)
        {
            if (0 < pECB->cbTotalBytes)
            {
                unsigned long ulSize = pECB->cbTotalBytes;
                char* lpszContent = new char[ulSize + 1];
                if (NULL != lpszContent)
                {
                    memcpy(lpszContent, pECB->lpbData, ulSize);
                    lpszContent[ulSize] = '\0';
                    
                    std::string message_id;
                    // Extract Message id from lpszContent using string operation.

                    if(strcmp(pECB->lpszQueryString, "DownloadFile") == 0)
                    {            
                        DownloadFileFunction(pECB, message_id));            
                    }
                    
                    delete[] lpszContent;
                    lpszContent = NULL;
                }
            }
        }
    }    
    catch (...)
    {

    }
    return HSE_STATUS_SUCCESS;
    }
-    Use ServerSupportFunction of EXTENSION_CONTROL_BLOCK instead of TransmitFile function of CHttepServerContext. Previously TransmitFile() were used for File Transfer.
-    Use HSE_REQ_TRANSMIT_FILE in ServerSupportFunction for file transfer.
-    Pull the transmit file information in HSE_TF_INFO structure.
-    Create a callback function (AsyncIOCompletionFunction) that will be called on asynchronous I/O completion.

    void WINAPI  AsyncIOCompletionFunction(
    EXTENSION_CONTROL_BLOCK * pECB,
    PVOID    pContext,
    DWORD    cbIO,
    DWORD    dwError)
    {
        HSE_CUSTOM_ERROR_INFO* pErrorInfo = (HSE_CUSTOM_ERROR_INFO*)pContext;
        DWORD dwIOStatus = dwError == NO_ERROR ?
            HSE_STATUS_SUCCESS :
            HSE_STATUS_ERROR;
    
        if (NULL != pErrorInfo)
        {
            //
            // log HTTP status code
            //
            pECB->dwHttpStatusCode = atoi(pErrorInfo->pszStatus);
    
            delete pErrorInfo;
            pErrorInfo = NULL;
        }
    
        pECB->ServerSupportFunction(
            pECB->ConnID,
            HSE_REQ_DONE_WITH_SESSION,
            &dwIOStatus,
            (LPDWORD)NULL,
            (LPDWORD)NULL);
    }

Code snippet for DownloadFileFunction()

    HSE_TF_INFO pHSEInfo;

    pHSEInfo.hFile            = hFile;
    pHSEInfo.dwFlags          = HSE_IO_ASYNC;
    pHSEInfo.pHead            = (PVOID) pvHeader;
    pHSEInfo.HeadLength       = dwHeaderLen;
    pHSEInfo.pContext         = 0;
    pHSEInfo.pTail            = 0;
    pHSEInfo.TailLength       = 0;
    pHSEInfo.BytesToWrite     = 0;
    pHSEInfo.Offset           = 0;
    pHSEInfo.pfnHseIO         = AsyncIOCompletionFunction; // callback function that will     be called on asynchronous I/O completion
    pHSEInfo.pszStatusCode    = LPCSTR("200 OK");

    bResult = m_pCtxt->ServerSupportFunction(m_pCtxt->ConnID, HSE_REQ_TRANSMIT_FILE, &pHSEInfo, 0, 0);


