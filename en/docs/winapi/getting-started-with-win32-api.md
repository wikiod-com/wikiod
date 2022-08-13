---
title: "Getting started with Win32 API"
slug: "getting-started-with-win32-api"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
Microsoft Windows applications are usually written as either a console application or a windowed application (there are other types such as services and plug-ins). The difference for the programmer is the difference in the interface for the main entry point for the application source provided by the programmer.

When a C or C++ application starts, the executable entry point used by the [executable loader][1] is the Runtime that is provided by the compiler. The executable loader reads in the executable, performs any fixup to the image needed, and then invokes the executable entry point which for a C or C++ program is the Runtime provided by the compiler.

The executable entry point invoked by the loader is not the main entry point provided by the application programmer but is instead the Runtime provided by the compiler and [the linker][2] which creates the executable. The Runtime sets up the environment for the application and then calls the main entry point provided by the programmer.

A Windows console application may have several slightly different interfaces for the main entry point provided by the programmer. The difference between these is whether the main entry point is the traditional `int main (int argc, char *argv[])` or if it is the Windows specific version of `int _tmain(int argc, _TCHAR* argv[])` which provides for wide characters in the application parameters. If you generate a Windows Win32 console application project using Visual Studio, the source generated will be the Windows specific version.

A Windows window (GUI) application has a different interface for the main entry point provided by the programmer. This main entry point provided by the programmer has a more complex interface because the Runtime sets up a GUI environment and provides additional information along with the application parameters.

This example explains the Windows window (GUI) main entry point interface. To explore this topics you should have:
* an IDE with compiler (preferably Visual Studio)
* C knowledge

Create an empty Win32 windows (GUI, not console) project using the IDE. The project settings must be set for a window application (not a console application) in order for the linker to link with the correct Runtime. Create a `main.c` file adding it to the project and then type the following code:
    
    #include <windows.h>

    int APIENTRY WinMain(HINSTANCE hInst, HINSTANCE hInstPrev, PSTR cmdline, int cmdshow)
    {
        return MessageBox(NULL, "hello, world", "caption", 0);
    }

This is our Win32 "Hello, world" program. The first step is to include the windows header files. The main header for all of Windows is `windows.h`, but there are others.

The `WinMain` is different from a standard `int main()` used with a console application. There are more parameters used in the interface and more importantly the main entry point for a window application uses a calling convention different from standard C/C++.

The qualifier `APIENTRY` indicates the calling convention, which is the order in which arguments are pushed on the stack<sup>&dagger;</sup>. By default, the calling convention is the standard C convention indicated by `__cdecl`. However Microsoft uses a different type of calling convention, the PASCAL convention, for the Windows API functions which is indicated by the `__stdcall` qualifier. `APIENTRY` is a defined name for `__stdcall` in one of the header files included by `windows.h` (see also [What is __stdcall?](http://stackoverflow.com/questions/297654/what-is-stdcall)).

The next arguments to `WinMain` are as follows:
* hInst: The instance handle
* hInstPrev: The previous instance handle. No longer used.
* cmdline: Command line arguments (see [Pass WinMain (or wWinMain) arguments to normal main](http://stackoverflow.com/questions/27363851/pass-winmain-or-wwinmain-arguments-to-normal-main))
* cmdshow: indicates if a window should be displayed.

We don't use any of these arguments yet.

Inside of `WinMain()`, is a call to `MessageBox()`, which displays a simple dialog with a message, a message box. The first argument is the handle to the owner window. Since we don't have our own window yet, pass `NULL`. The second argument is the body text. The third argument is the caption, and the fourth argument contains the flags. When 0 is passed, a default message box is shown. The diagram below dissects the message box dialog.

[![enter image description here][3]][3]

Good links:

* [Tutorial at winprog.org](http://winprog.org/tutorial/)
* [`MessageBox` function documentation at MSDN](https://msdn.microsoft.com/en-us/library/windows/desktop/ms645505.aspx)

_<sup>&dagger;</sup>On 32 bit systems only. Other architectures have different calling conventions._


  [1]: https://en.wikipedia.org/wiki/Loader_(computing)
  [2]: https://en.wikipedia.org/wiki/Linker_(computing)
  [3]: http://i.stack.imgur.com/U0n67.png

