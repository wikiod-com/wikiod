---
title: "Window messages"
slug: "window-messages"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Syntax
- #include <windows.h>
- BOOL WINAPI DestroyWindow(HWND hwnd);
- VOID WINAPI PostQuitMessage(int exitcode);
- BOOL WINAPI MoveWindow(HWND hwnd, int x, int y, int cx, int cy, BOOL bRepaint);




## WM_DESTROY
This message is sent to your window procedure when a window is being destroyed. It is sent after the window is removed from the screen. Most applications free any resources, like memory or handles, obtained in WM_CREATE. If you handle this message, return 0.

    LRESULT CALLBACK winproc(HWND hwnd, UINT wm, WPARAM wp, LPARAM lp)
    {
        static char *text;
        switch (wm) {
            case WM_CREATE:
                text = malloc(256);
                /* use the allocated memory */
                return 0;
            case WM_CLOSE:
                 switch (MessageBox(hwnd, "Save changes?", "", MB_YESNOCANCEL)) {
                     case IDYES:
                          savedoc();
                                             /* fall through */
                     case IDNO:
                          DestroyWindow(hwnd);
                          break;
                 }
                 return 0;
            case WM_DESTROY:
                /* free the memory */
                free(text);
                PostQuitMessage(0);
                return 0;
        }
        return DefWindowProc(hwnd, wm, wp, lp);
    }

## WM_CLOSE
Sent when an application's close button is clicked. Do not confuse this with `WM_DESTROY` which is sent when a window will be destroyed. The main difference lies in the fact that closing may be canceled in WM_CLOSE (think of Microsoft Word asking to save your changes), versus that destroying is when the window has already been closed (think of Microsoft Word freeing memory).

    LRESULT CALLBACK winproc(HWND hwnd, UINT wm, WPARAM wp, LPARAM lp)
    {
        static char *text;
        switch (wm) {
            case WM_CREATE:
                text = malloc(256);
                /* use the allocated memory */
                return 0;
            case WM_CLOSE:
                 switch (MessageBox(hwnd, "Save changes?", "", MB_YESNOCANCEL)) {
                     case IDYES:
                          savedoc();
                                             /* fall through */
                     case IDNO:
                          DestroyWindow(hwnd);
                          break;
                 }
                 return 0;
            case WM_DESTROY:
                /* free the memory */
                free(text);
                PostQuitMessage(0);
                return 0;
        }
        return DefWindowProc(hwnd, wm, wp, lp);
    }


## WM_COMMAND
Sent to a window procedure when:
* the user selects an item from a menu
* a control sends a notification to its parent window
* an accelerator keystroke is translated

| Message Source | HIWORD(wp) | LOWORD(wp)            | lp |
| ------         | ------     | ------                | ------ |
| Menu           | 0          | Menu ID (IDM_*)       | 0 |
| Accelerator    | 1          | Accel ID (IDM_*)      | 0 |
| Control        | notification code | Control id | HWND of control window |

For example, in Notepad, when a user clicks "File->Open" a dialog box is displayed to allow the user to open a file. Menu items are processed in the window procedure's WM_CREATE message like this:

    LRESULT CALLBACK winproc(HWND hwnd, UINT wm, WPARAM wp, LPARAM lp)
    {
        switch (wm) {
            case WM_COMMAND:
                switch (LOWORD(wp) {
                    case ID_FILE_OPEN:
                        /* show file open dialog */
                        break;
                    case ID_FILE_NEW:
                        /* create new instance */
                        break;
                }
                return 0;
       }
       return DefWindowProc(hwnd, wm, wp, lp);
    }

## WM_CREATE
A WM_CREATE message is sent to your window procedure during the window's `CreateWindowEx` call. The `lp` argument contains a pointer to a `CREATESTRUCT` which contains the arguments  passed to `CreateWindowEx`. If an application returns 0 from WM_CREATE, the window is created. If an application returns -1, creation is canceled.

    LRESULT CALLBACK winproc(HWND hwnd, UINT wm, WPARAM wp, LPARAM lp)
    {
        switch (wm) {
            case WM_CREATE:
                CREATESTRUCT *cs = (CREATESTRUCT *) lp;
                if (MessageBox(hwnd, 
                    "Do you want to continue creating the window?", "", MB_YESNO)  
                                             == IDYES) {
                    /* create window controls */
                    return 0;
                }
                /* cancel creation */
                return -1;
        }
        return DefWindowProc(hwnd, wm, wp, lp);
    }

## WM_SIZE
This message is sent to the window's window procedure after it's size has changed. The most common reason for handling this message is to adjust the position of any child windows. For example, in Notepad, when the window is resized the child window (edit control) is also resized. Return 0 if you handle this message.

| Argument | Value |
| ------ | ------ |
| wp   | One of the [window sizing constants][1].|
|lp | LOWORD(lp) is the new width of the client area<br>HIWORD(lp) is the new height of the client area.|

    LRESULT CALLBACK winproc(HWND hwnd, UINT wm, WPARAM wp, LPARAM lp)
    {
        switch (wm) {
            case WM_SIZE:
                /* hwndEdit is the handle of the edit control window */
                MoveWindow(hwndEdit, 0, 0, LOWORD(lp), HIWORD(lp), TRUE);
                return 0;
       }
       return DefWindowProc(hwnd, wm, wp, lp);
    }


  [1]: https://msdn.microsoft.com/en-us/library/windows/desktop/ms632646(v=vs.85).aspx

