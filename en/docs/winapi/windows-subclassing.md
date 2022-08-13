---
title: "Windows Subclassing"
slug: "windows-subclassing"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

Window subclassing is a way to hook up into standard window procedure and to modify or extend its default behavior. An application subclasses a window by replacing the the window's original window procedure with a new window procedure. This new window procedure receives any messages sent or posted to the window.


## Syntax
- BOOL SetWindowSubclass(HWND hWnd, SUBCLASSPROC SubclassProc, UINT_PTR SubclassId, DWORD_PTR RefData);
- BOOL RemoveWindowSubclass(HWND hWnd, SUBCLASSPROC SubclassProc, UINT_PTR SubclassId);
- BOOL GetWindowSubclass(HWND hWnd, SUBCLASSPROC SubclassProc, UINT_PTR SubclassId, DORD_PTR* RefData);
- LRESULT DefSubclassProc(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam);


## Parameters
|Parameter|Detail|
|---------|---------|
|hWnd|The handle of the window to subclass.|
|SubclassProc|The subclass callback procedure.|
|SubclassId|User specified ID to identify the subclass, together with the subclass procedure uniquely identifies a subclass. It can simply be an arbitrary consecutive number.|
|RefData|User specified data. The meaning is determined by the application. It is passed to the subclass callback in unmodified way. It could be an object pointer to a class instance for example.|

**MSDN Documentation**

* [About Windows Procedures](https://www.google.com/search?q=+Window+Procedure+Overviews+site:msdn.microsoft.com&btnI)
* [Subclassing Controls](https://www.google.com/search?q=+Subclassing+Controls+site:msdn.microsoft.com&btnI)


## Subclassing windows button control within C++ class
This example shows how to manipulate button ideal size by specifying a fixed size.

    class ButtonSubclass {
    public:

        ButtonSubclass(HWND hWndButton) {
            SetWindowSubclass(hWndButton, MyButtonSubclassProc, 1, (DWORD_PTR) this);
        }
        ~ButtonSuclass() {
            RemoveWindowSubclass(hWndButton, MyButtonSubclassProc, 1, (DWORD_PTR) this);
        }

    protected:

        static LRESULT CALLBACK MyButtonSubclassProc(
               HWND hWnd, UINT Msg, WPARAM w, LPARAM l, DWORD_PTR RefData) {

            ButtonSubclass* o = reinterpret_cast<ButtonSubclass*>(RefData);

            if (Msg == BCM_GETIDEALSIZE) {
                reinterpret_cast<SIZE*>(lParam)->cx = 100;
                reinterpret_cast<SIZE*>(lParam)->cy = 100;
                return TRUE;
            }
            return DefSubclassProc(hWnd, Msg, w, l);
        }
    }

**Installing and removing subclass procedure**

The following methods installs or removes the subclass callback. The combination of `SubclassId` and `SubclassProc` uniquely identifies a subclass. There is no reference counting, calling `SetWindowSubclass` multiple times with different `RefData` only updates that value but will not causes the subclass callback to be called multiple times.

    BOOL SetWindowSubclass(HWND hWnd, SUBCLASSPROC SubclassProc, UINT_PTR SubclassId, DWORD_PTR RefData);
    BOOL RemoveWindowSubclass(HWND hWnd, SUBCLASSPROC SubclassProc, UINT_PTR SubclassId);

To retrieve the reference data that was passed in the last `SetWindowSubclass`call, one can use the `GetWindowSubclass` method.

    BOOL GetWindowSubclass(HWND hWnd, SUBCLASSPROC SubclassProc, UINT_PTR SubclassId, DORD_PTR* RefData);

|Parameter|Detail|
|---------|---------|
|hWnd|The handle of the window to subclass.|
|SubclassProc|The subclass callback procedure.|
|SubclassId|User specified ID to identify the subclass, together with the subclass procedure uniquely identifies a subclass. It can simply be an arbitrary consecutive number.|
|RefData|User specified data. The meaning is determined by the application. It is passed to the subclass callback in unmodified way. It could be an object pointer to a class instance for example.|

The subclass callback is responsible to call the next handler in window's subclass chain. `DefSubclassProc` calls the next handler in window's subclass chain. The last handler calls the original window procedure. It should be called in any subclassing callback procedure unless the message is completely handled by the application.

    LRESULT DefSubclassProc(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam);

|Parameter|Detail|
|---------|------|
|hWnd|Window handle where the message originates from|
|Msg|Window message|
|wParam|WPARAM argument, this value depends on specific window message|
|lParam|LPARAM argument, this value depends on specific window message|

**SUBCLASSPROC**

It is similar to `WINDOWPROC` callback but contains an additional argument `RefData`.

    typedef LRESULT (CALLBACK *SUBCLASSPROC)(
        HWND hWnd,
        UINT Msg,
        WPARAM wParam,
        LPARAM lParam,
        UINT_PTR SubclassId,
        DWORD_PTR RefData
    );
    

## Handling common controls notification messages within C++ class
    class MyToolbarControl {
    public:
        MyToolbarControl(HWND hWndToolbar, HWND hWndNotifyParent = nullptr) : _Handle(hWndToolbar) {
            if (hWndNotifyParent == nullptr) {
                hWndNotifyParent = GetAncestor(hWndToolbar, GA_ROOTOWNER);
            }
            SetWindowSubclass(
                hWndNotifyParent , SubclassWindowProc, reinterpret_cast<UINT_PTR>(this), reinterpret_cast<DWORD_PTR>(this)
            );
        }
        ~MyToolbarControl() {
            RemoveWindowSubclass(
                hWndNotifyParent , SubclassWindowProc, reinterpret_cast<UINT_PTR>(this), reinterpret_cast<DWORD_PTR>(this)
            );
        }

    protected:
        HWND _Handle;

        static LRESULT CALLBACK SubclassWindowProc(
            HWND hWnd, UINT Msg, WPARAM w, LPARAM l, UINT_PTR SubclassId, DWORD_PTR RefData) {
            MyToolbarControl * w = reinterpret_cast<MyToolbarControl *>(RefData);
            if (Msg == WM_NOTIFY) {
                NMHDR* h = reinterpret_cast<NMHDR*>(l);
                if (h->hwndFrom == w->_Handle) {
                    // Handle notification message here...
                }
            }
            return DefSubclassProc(hWnd, Msg, w, l);
        }
    };
    

