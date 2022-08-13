---
title: "Dealing with windows"
slug: "dealing-with-windows"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Creating a window
    #define UNICODE
    #define _UNICODE
    #include <windows.h>
    #include <tchar.h>
    const TCHAR CLSNAME[] = TEXT("helloworldWClass");
    LRESULT CALLBACK winproc(HWND hwnd, UINT wm, WPARAM wp, LPARAM lp);
    
    int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrevInst, PTSTR cmdline,
                       int cmdshow)
    {
        WNDCLASSEX wc = { };
        MSG msg;
        HWND hwnd;
    
        wc.cbSize        = sizeof (wc);
        wc.style         = 0;
        wc.lpfnWndProc   = winproc;
        wc.cbClsExtra    = 0;
        wc.cbWndExtra    = 0;
        wc.hInstance     = hInst;
        wc.hIcon         = LoadIcon (NULL, IDI_APPLICATION);
        wc.hCursor       = LoadCursor (NULL, IDC_ARROW);
        wc.hbrBackground = (HBRUSH) GetStockObject (WHITE_BRUSH);
        wc.lpszMenuName  = NULL;
        wc.lpszClassName = CLSNAME;
        wc.hIconSm       = LoadIcon (NULL, IDI_APPLICATION);
    
        if (!RegisterClassEx(&wc)) {
            MessageBox(NULL, TEXT("Could not register window class"), 
                      NULL, MB_ICONERROR);
            return 0;
        }
    
        hwnd = CreateWindowEx(WS_EX_LEFT,
                              CLSNAME,
                              NULL,
                              WS_OVERLAPPEDWINDOW,
                              CW_USEDEFAULT,
                              CW_USEDEFAULT,
                              CW_USEDEFAULT,
                              CW_USEDEFAULT,
                              NULL,
                              NULL,
                              hInst,
                              NULL);
        if (!hwnd) {
            MessageBox(NULL, TEXT("Could not create window"), NULL, MB_ICONERROR);
            return 0;
        }
    
        ShowWindow(hwnd, cmdshow);
        UpdateWindow(hwnd);
        while (GetMessage(&msg, NULL, 0, 0)) {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
        return msg.wParam;
    }
    LRESULT CALLBACK winproc(HWND hwnd, UINT wm, WPARAM wp, LPARAM lp)
    {
        return DefWindowProc(hwnd, wm, wp, lp);
    }

The first thing one sees are the two macro definitions, `UNICODE` and `_UNICODE`. These macros cause our program to understand wide character strings (`wchar_t[n]`), not plain narrow strings(`char[n]`). As a result, all string literals must be wrapped in a `TEXT(` macro. The generic character type for Win32 strings is `TCHAR`, whose definition depends on whether or not `UNICODE` is defined. A new header is included: `<tchar.h>` contains the declaration of `TCHAR`.

A window consists of what is known as a *window class*. This describes information about a window that is to be shared between instances of it, like the icon, the cursor, and others. A window class is identified by a window class name, which is given in the `CLSNAME` global variable in this example. The first act of `WinMain` is to fill in the window class structure, `WNDCLASSEX wc`. The members are:

* cbSize: The size, in bytes, of the structure
* style: The window class styles. This is 0 for now.
* lpfnWndProc: This is one of the more important fields. It stores the address of the *window procedure*. The window procedure is a function that handles events for all windows that are instances of this window class.
* cbClsExtra: The number of extra bytes to allocate for the window class. For most situations, this member is 0.
* cbWndExtra: The number of extra bytes to allocate for each individual window. Do not confuse this with `cbClsExtra`, which is common to all instances. This is often 0.
* hInstance: The instance handle. Just assign the `hInst` argument in `WinMain` to this field.
* hIcon: The icon handle for the window class. `LoadIcon(NULL, IDI_APPLICATION)` loads the default application icon.
* hCursor: The cursor handle for the window class. `LoadCursor(NULL, IDC_ARROW)` loads the default cursor.
* hbrBackground: A handle to the background brush. `GetStockObject (WHITE_BRUSH)` gives a handle to a white brush. The return value must be cast because `GetStockObject` returns a generic object.
* lpszMenuName: The resource name of the menu bar to use. If no menu bar is needed, this field can be NULL.
* lpszClassName: The class name that identifies this window class structure. In this example, the `CLSNAME` global variable stores the window class name.
* hIconSm: A handle to the small class icon.

After this structure is initialized, the `RegisterClassEx` function is called. This causes the window class to be registered with Windows, making it known to the application. It returns 0 on failure.

Now that the window class has been registered, we can display the window using `CreateWindowEx`. The arguments are:

* stylesex: The extended window styles. The default value is WS_EX_LEFT.
* clsname: The class name
* cap: The window title, or caption. In this case, it is the caption that is displayed in a window's title bar.
* styles: The window styles. If you want to create a top-level (parent) window like this one, the flag to pass in is WS_OVERLAPPEDWINDOW.
* x: The x-coordinate of the upper-left corner of the window.
* y: The y-coordinate of the upper-left corner of the window
* cx: The width of the window
* cy: The height of the window
* hwndParent: The handle to the parent window. Since this window is in itself a parent window, this argument is NULL.
* hMenuOrID: If the window being created is a parent window, then this argument is a handle to the window menu. Do not confuse this with the class menu, which is `WNDCLASSEX::lpszClassName`. The class menu is common to all instances of windows with the same class name. This argument, however, is specific for just this instance. If the window being created is a child window, then this is the ID of the child window. In this case, we are creating a parent window with no menu, so NULL is passed.
* hInst: The handle to the instance of the application.
* etc: The extra information that is passed to the window's window procedure. If no extra information is to be transmitted, pass NULL.

If `x` or `y` or `cx` or `cy` is `CW_USEDEFAULT`, then that argument's value will be determined by Windows. That is what is done in this example.

`CreateWindowEx` returns the handle to the newly created window. If window creation failed, it returned `NULL`.

We then show the window by calling `ShowWindow`. The first argument for this function is the handle to the window. The second argument is the show style, which indicates how the window is to be displayed. Most applications just pass the `cmdshow` argument passed in `WinMain`. After the window is shown, it must be updated by a call to `UpdateWindow`. It causes an update message to be sent to the window. We will learn what this means in another tutorial.

Now comes the heart of the application: The message pump. It pumps messages sent to this application by the operating system, and dispatches the messages to the window procedure. The `GetMessage` call returns non-zero until the application receieves a messages that causes it to quit, in which case it returns 0. The only argument that concerns us is the pointer to an `MSG` structure that will be filled in with information about the message. The other arguments are all 0.

Inside the message loop, `TranslateMessage` translates virtual-key messages into character messages. The meaning of this, again, is unimportant to us. It takes a pointer to an `MSG` structure. The call directly following it, `DispatchMessage`, dispatches the message pointed to by its argument to the window's window procedure. The last thing `WinMain` must do is return a status code. The `wParam` member of the `MSG` structure contains this return value, so it is returned.

But that's just for the `WinMain` function. The other function is `winproc`, the window procedure. It will handle messages for the window that are sent to it by Windows. The signature for `winproc` is:

* hwnd: A handle to the window whose messages are being processed.
* wm: The window message identifier
* wp: One of the message information arguments. This depends on the `wm` argument
* lp: One of the message information arguments. This depends on the `wm` argument. This argument is usually used to transmit pointers or handles

In this simple program, we do not handle any messages ourselves. But that doesn't mean Windows doesn't either. This is why one must call `DefWindowProc`, which contains default window handling code. This function must be called at the end of every window procedure.

<h3>What is a handle?</h3>
A *handle* is a data type that represents a unique object. They are pointers, but to secret data structures maintained by the operating system. The details of these structures need not concern us. All a user needs to do is simply create/retreive a handle using an API call, and pass it around to other API calls taking that type of handle. The only type of handle we used was the `HWND` returned by `CreateWindowEx`. 

<h3>Constants</h3>
In this example, we encounter a handful of constants, which are in all-caps and begin with a 2 or 3 letter prefix. (The Windows types are also in all-caps)

* IDI_APPLICATION: The resource name containing the default application icon. This is used with either `LoadIcon` or `LoadImage` (LoadIcon in this example).
* IDC_ARROW: The resource name countaining the default application cursor. This is used with either `LoadIcon` or `LoadImage` (LoadIcon in this example).
* WHITE_BRUSH: The name of a stock object. This stock object is the white brush.
* MB_ICONERROR: A flag used with `MessageBox` to display an error icon.
* WS_EX_LEFT: The default extended window style. This causes the window to have left-aligned properties.
* WS_OVERLAPPEDWINDOW: A window style indicating that the window should be a parent window with a title bar, size box, and others elements typical of top-level windows.
* CW_USEDEFAULT: Used with `CreateWindowEx`'s `x`, `y`, `cx`, or `cy` arguments. Causes Windows to choose a valid value for the argument for which `CW_USEDEFAULT` was passed.

<h3>Windows Types</h3>
When programming for Windows, you will have to get used to the Win32 types, which are aliases for builtin types. These types are in all caps. The alias types used in this program are:

* TCHAR: The generic character type. If `UNICODE` is defined, this is a `wchar_t`. Otheriwse, it is a `char`.
* UINT: An unsigned integer. Used to represent the message identifier in window procedures, and other purposes.
* WPARAM: In Win16, this was a WORD argument (hence the `W` prefix). With the introduction of Win32, however, this is now a `UINT_PTR`. This illustrates the point of these Windows aliases; they are there to protect programs from change.
* LPARAM: This is a `LONG` argument (`LONG_PTR` in Win64). 
* PTSTR: The `P` means pointer. The `T` means generic character, and the `STR` means string. Thus, this is a pointer to a `TCHAR` string. Other string types include:
  - LPTSTR: Same as `PTSTR`
  - LPCTSTR: Means `const TCHAR *`
  - PCTSTR: Same as `LPCTSTR`
  - LPWSTR: Wide string (`wchar_t *`)
  - LPCWSTR: Means `const wchar_t *`
  - PWSTR: Same as `LPWSTR`
  - and much more
As you can see, the Win32 types can be a hassle to understand, especially with so many synonymous types, which is an artifact of Win16. 
* LRESULT: This type is used to represent the return value of window procedures. It is usually a LONG (hence the `L`).

