---
title: "Getting started with mfc"
slug: "getting-started-with-mfc"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## A basic MFC program
    // Include the MFC header:
    // (you do not need to and should not include the standard Windows headers, e.g. 
    // Windows.h)
    #include <AfxWin.h>               // MFC core and standard components
    // The following header defines resource constants, such as dialog and control IDs:
    #include "resource.h"

    // The basic element of an MFC application is a class that inherits from CWinApp.
    class CMyApp : public CWinApp
    {
        // This gets called as the application gets initialized.
        virtual BOOL InitInstance()
        {
            // Initialize a CDialog object to show in a moment.
            CDialog dlg(IDD_DIALOG1);
            // Display the dialog box as a modal dialog box.
            dlg.DoModal();

            // Return FALSE from this method to exit the application.
            return FALSE;
        }
    };

    // The one and only application object.
    CMyWinApp theApp;
    
**Summary:**

IDD_DIALOG1 should be the ID of a dialog box defined in a project resource file created by a resource editor, such as the one built into Visual Studio. (A resource file generally has the .rc extension.) To customize the behavior of a dialog, you can derive a new class from CDialog.

A modal dialog box runs its own message loop. The call "dlg.DoModal();" does not return until the dialog has been closed by the user.

If we had returned TRUE from InitInstance(), it would have started the application's message loop. This is used when you have a more complex, non-dialog-based app.

