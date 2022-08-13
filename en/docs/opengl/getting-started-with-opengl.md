---
title: "Getting started with opengl"
slug: "getting-started-with-opengl"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Manual OpenGL setup on Windows
*Full example code included at the end*

<h1>Windows components for OpenGL</h1>

<h2>WGL</h2>

WGL (can be pronounced *wiggle*) stands for "Windows-GL", as in "an interface between Windows and OpenGL" - a set of functions from the Windows API to communicate with OpenGL. WGL functions have a *wgl* prefix and its tokens have a *WGL_* prefix.

Default OpenGL version supported on Microsoft systems is 1.1. That is a very old version (most recent one is 4.5). The way to get the most recent versions is to update your graphics drivers, but your graphics card must support those new versions.

Full list of WGL functions can be found [here][2].

<h2>Graphics device interface (GDI)</h2>

GDI (today updated to GDI+) is a 2D drawing interface that allows you to draw onto a window in Windows. You need GDI to initialize OpenGL and allow it to interact with it (but will not actually use GDI itself).

In GDI, each window has a *device context (DC)* that is used to identify the drawing target when calling functions (you pass it as a parameter). However, OpenGL uses its own *rendering context (RC)*. So, DC will be used to create RC.

<hr />

<h1>Basic setup</h1>

<h2>Creating a window</h2>

So for doing things in OpenGL, we need RC, and to get RC, we need DC, and to get DC we need a window. Creating a window using the Windows API requires several steps. *This is a basic routine, so for a more detailed explanation, you should consult other documentation, because this is not about using the Windows API.*

This is a Windows setup, so <code>Windows.h</code> must be included, and the entry point of the program must be <code>WinMain</code> procedure with its parameters. The program also needs to be linked to <code>opengl32.dll</code> and to <code>gdi32.dll</code> (regardless of whether you are on 64 or 32 bit system).

First we need to describe our window using the <code>WNDCLASS</code> structure. It contains information about the window we want to create:

    /* REGISTER WINDOW */
    WNDCLASS window_class;

    // Clear all structure fields to zero first
    ZeroMemory(&window_class, sizeof(window_class));

    // Define fields we need (others will be zero)
    window_class.style = CS_OWNDC;
    window_class.lpfnWndProc = window_procedure; // To be introduced later
    window_class.hInstance = instance_handle;
    window_class.lpszClassName = TEXT("OPENGL_WINDOW");

    // Give our class to Windows
    RegisterClass(&window_class);
    /* *************** */

For a precise explanation of the meaning of each field (and for a full list of fields), consult MSDN documenation.

Then, we can create a window using <code>CreateWindowEx</code>. After the window is created, we can acquire its DC:

    /* CREATE WINDOW */
    HWND window_handle = CreateWindowEx(WS_EX_OVERLAPPEDWINDOW,
                                        TEXT("OPENGL_WINDOW"),
                                        TEXT("OpenGL window"),
                                        WS_OVERLAPPEDWINDOW,
                                        0, 0,
                                        800, 600,
                                        NULL,
                                        NULL,
                                        instance_handle,
                                        NULL);
    
    HDC dc = GetDC(window_handle);
    
    ShowWindow(window_handle, SW_SHOW);
    /* ************* */

Finally, we need to create a message loop that receives window events from the OS:

    /* EVENT PUMP */
    MSG msg;
    
    while (true) {
        if (PeekMessage(&msg, window_handle, 0, 0, PM_REMOVE)) {
            if (msg.message == WM_QUIT)
                break;
            
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
        
        // draw(); <- there goes your drawing

        SwapBuffers(dc); // To be mentioned later
    }
    /* ********** */

<h2>Pixel format</h2>

OpenGL needs to know some information about our window, such as color bitness, buffering method, and so on. For this, we use a *pixel format*. However, we can only suggest to the OS what kind of a pixel format we need, and the OS will supply *the most similar supported one*, we don't have direct control over it. That is why it is only called a *descriptor*.

    /* PIXEL FORMAT */
    PIXELFORMATDESCRIPTOR descriptor;
    
    // Clear all structure fields to zero first
    ZeroMemory(&descriptor, sizeof(descriptor));
    
    // Describe our pixel format
    descriptor.nSize = sizeof(descriptor);
    descriptor.nVersion = 1;
    descriptor.dwFlags = PFD_DRAW_TO_WINDOW | PFD_DRAW_TO_BITMAP | PFD_SUPPORT_OPENGL | PFD_GENERIC_ACCELERATED | PFD_DOUBLEBUFFER | PFD_SWAP_LAYER_BUFFERS;
    descriptor.iPixelType = PFD_TYPE_RGBA;
    descriptor.cColorBits = 32;
    descriptor.cRedBits = 8;
    descriptor.cGreenBits = 8;
    descriptor.cBlueBits = 8;
    descriptor.cAlphaBits = 8;
    descriptor.cDepthBits = 32;
    descriptor.cStencilBits = 8;
    
    // Ask for a similar supported format and set it
    int pixel_format = ChoosePixelFormat(dc, &descriptor);
    SetPixelFormat(dc, pixel_format, &descriptor);
    /* *********************** */

We've enabled double buffering in the <code>dwFlags</code> field, so we must call <code>SwapBuffers</code> in order to see things after drawing.

<h2>Rendering context</h2>

After that, we can simply create our rendering context:

    /* RENDERING CONTEXT */
    HGLRC rc = wglCreateContext(dc);
    wglMakeCurrent(dc, rc);
    /* ***************** */

Note that only one thread can use the RC at a time. If you wish to use it from another thread later, you must call <code>wglMakeCurrent</code> there to activate it again (this will deactivate it on the thread it's currently active, and so on).

<h2>Getting OpenGL functions</h2>

OpenGL functions are obtained by using function pointers. The general procedure is:

 1. Somehow obtain function pointer types (essentially the function prototypes)
 2. Declare each function we would like to use (with its function pointer type)
 3. Obtain the actual function

For example, consider glBegin:

    // We need to somehow find something that contains something like this,
    // as we can't know all the OpenGL function prototypes
    typedef void (APIENTRY *PFNGLBEGINPROC)(GLenum);
    
    // After that, we need to declare the function in order to use it
    PFNGLBEGINPROC glBegin;
    
    // And finally, we need to somehow make it an actual function
    
*("PFN" means "pointer to function", then follows the name of an OpenGL function, and "PROC" at the end - that is the usual OpenGL function pointer type name.)*

Here's how it's done on Windows. As mentioned previously, Microsoft only ships OpenGL 1.1. First, function pointer types for that version can be found by including <code>GL/gl.h</code>. After that, we declare all the functions we intend to use as shown above (doing that in a header file and declaring them "extern" would allow us to use them all after loading them once, just by including it). Finally, loading the OpenGL 1.1 functions is done by opening the DLL:

    HMODULE gl_module = LoadLibrary(TEXT("opengl32.dll"));
    
    /* Load all the functions here */
    glBegin = (PFNGLBEGINPROC)GetProcAddress("glBegin");
    // ...
    /* *************************** */
    
    FreeLibrary(gl_module);
    
However, we probably want a little bit more than OpenGL 1.1. But Windows doesn't give us the function prototypes or exported functions for anything above that. The prototypes need to be acquired from the [OpenGL registry][1]. There are three files of interest to us: <code>GL/glext.h</code>, <code>GL/glcorearb.h</code>, and <code>GL/wglext.h</code>.

In order to complete <code>GL/gl.h</code> provided by Windows, we need <code>GL/glext.h</code>. It contains (as described by the registry) "OpenGL 1.2 and above compatibility profile and extension interfaces" (more about profiles and extensions later, where we'll see that it's **actually not a good idea to use those two files**).

The actual functions need to be obtained by <code>wglGetProcAddress</code> (no need for opening the DLL for this guy, they aren't in there, just use the function). With it, we can fetch all the functions from OpenGL 1.2 and above (but not 1.1). Note that, in order for it to function properly, **the OpenGL rendering context must be created and made current**. So, for example, <code>glClear</code>:

    // Include the header from the OpenGL registry for function pointer types
    
    // Declare the functions, just like before
    PFNGLCLEARPROC glClear;
    // ...
    
    // Get the function
    glClear = (PFNGLCLEARPROC)wglGetProcAddress("glClear");
    
We can actually build a wrapper <code>get_proc</code> procedure that uses both <code>wglGetProcAddress</code> and <code>GetProcAddress</code>:

    // Get function pointer
    void* get_proc(const char *proc_name)
    {
        void *proc = (void*)wglGetProcAddress(proc_name);
        if (!proc) proc = (void*)GetProcAddress(gl_module, proc_name); // gl_module must be somewhere in reach

        return proc;
    }

So to wrap up, we would create a header file full of function pointer declarations like this:

    extern PFNGLCLEARCOLORPROC glClearColor;
    extern PFNGLCLEARDEPTHPROC glClearDepth;
    extern PFNGLCLEARPROC glClear;
    extern PFNGLCLEARBUFFERIVPROC glClearBufferiv;
    extern PFNGLCLEARBUFFERFVPROC glClearBufferfv;
    // And so on...


We can then create a procedure like <code>load_gl_functions</code> that we call only once, and works like so:

    glClearColor = (PFNGLCLEARCOLORPROC)get_proc("glClearColor");
    glClearDepth = (PFNGLCLEARDEPTHPROC)get_proc("glClearDepth");
    glClear = (PFNGLCLEARPROC)get_proc("glClear");
    glClearBufferiv = (PFNGLCLEARBUFFERIVPROC)get_proc("glClearBufferiv");
    glClearBufferfv = (PFNGLCLEARBUFFERFVPROC)get_proc("glClearBufferfv");

And you're all set! Just include the header with the function pointers and GL away.

<hr />

<h1>Better setup</h1>

<h2>OpenGL profiles</h2>

OpenGL has been in development for over 20 years, and the developers were always strict about *backwards compatibility (BC)*. Adding a new feature is very hard because of that. Thus, in 2008, it was separated into two "profiles". *Core* and *compatibility*. Core profile breaks BC in favor of performance improvements and some of the new features. It even completely removes some legacy features. Compatibility profile maintains BC with all versions down to 1.0, and some new features are not available on it. It is only to be used for old, legacy systems, all new applications should use the core profile.

*Because of that, there is a problem with our basic setup - it only provides the context that is backwards compatible with OpenGL 1.0. The pixel format is limited too. There is a better approach, using extensions.*

<h2>OpenGL extensions</h2>

Any addition to the original functionality of OpenGL are called extensions. Generally, they can either make some things legal that weren't before, extend parameter value range, extend GLSL, and even add completely new functionality.

There are three major groups of extensions: vendor, EXT, and ARB. Vendor extensions come from a specific vendor, and they have a vendor specific mark, like AMD or NV. EXT extensions are made by several vendors working together. After some time, they may become ARB extensions, which are all the officially supported ones and ones approved by ARB.

To acquire function pointer types and function prototypes of all the extensions *and as mentioned before, all the function pointer types from OpenGL 1.2 and greater*, one must download the header files from the [OpenGL registry][1]. As discussed, for new applications it's better to use core profile, so it would be preferrable to include <code>GL/glcorearb.h</code> instead of <code>GL/gl.h</code> and <code>GL/glext.h</code> (if you are using <code>GL/glcorearb.h</code> then don't include <code>GL/gl.h</code>).

There are also extensions for the WGL, in <code>GL/wglext.h</code>. For example, the function for getting the list of all supported extensions is actually an extension itself, the <code>wglGetExtensionsStringARB</code> (it returns a big string with a space-separated list of all the supported extensions).

Getting extensions is handled via <code>wglGetProcAddress</code> too, so we can just use our wrapper like before.

<h2>Advanced pixel format and context creation</h2>

The <code>WGL_ARB_pixel_format</code> extension allows us the advanced pixel format creation. Unlike before, we don't use a struct. Instead, we pass the list of wanted attributes.

    int pixel_format_arb;
    UINT pixel_formats_found;
    
    int pixel_attributes[] = {
        WGL_SUPPORT_OPENGL_ARB, 1,
        WGL_DRAW_TO_WINDOW_ARB, 1,
        WGL_DRAW_TO_BITMAP_ARB, 1,
        WGL_DOUBLE_BUFFER_ARB, 1,
        WGL_SWAP_LAYER_BUFFERS_ARB, 1,
        WGL_COLOR_BITS_ARB, 32,
        WGL_RED_BITS_ARB, 8,
        WGL_GREEN_BITS_ARB, 8,
        WGL_BLUE_BITS_ARB, 8,
        WGL_ALPHA_BITS_ARB, 8,
        WGL_DEPTH_BITS_ARB, 32,
        WGL_STENCIL_BITS_ARB, 8,
        WGL_ACCELERATION_ARB, WGL_FULL_ACCELERATION_ARB,
        WGL_PIXEL_TYPE_ARB, WGL_TYPE_RGBA_ARB,
        0
    };

    BOOL result = wglChoosePixelFormatARB(dc, pixel_attributes, NULL, 1, &pixel_format_arb, &pixel_formats_found);
    
Similarly, the <code>WGL_ARB_create_context</code> extension allows us the advanced context creation:

    GLint context_attributes[] = {
        WGL_CONTEXT_MAJOR_VERSION_ARB, 3,
        WGL_CONTEXT_MINOR_VERSION_ARB, 3,
        WGL_CONTEXT_PROFILE_MASK_ARB, WGL_CONTEXT_CORE_PROFILE_BIT_ARB,
        0
    };

    HGLRC new_rc = wglCreateContextAttribsARB(dc, 0, context_attributes);
    
For a precise explanation of the parameters and functions, consult the OpenGL specification.

Why didn't we just start off with them? Well, that's because the *extensions* allow us to do this, and to get extensions we need <code>wglGetProcAddress</code>, but that only works with an active valid context. So in essence, before we are able to create the context we want, we need to have some context active already, and it's usually referred to as a *dummy context*.

However, Windows doesn't allow setting the pixel format of a window more than once. Because of that, the window needs to be destroyed and recreated in order to apply new things:

    wglMakeCurrent(dc, NULL);
    wglDeleteContext(rc);
    ReleaseDC(window_handle, dc);
    DestroyWindow(window_handle);
    
    // Recreate the window...

<hr />

Full example code:

    /* We want the core profile, so we include GL/glcorearb.h. When including that, then
       GL/gl.h should not be included.

       If using compatibility profile, the GL/gl.h and GL/glext.h need to be included.

       GL/wglext.h gives WGL extensions.

       Note that Windows.h needs to be included before them. */

    #include <cstdio>
    #include <Windows.h>
    #include <GL/glcorearb.h>
    #include <GL/wglext.h>

    LRESULT CALLBACK window_procedure(HWND, UINT, WPARAM, LPARAM);
    void* get_proc(const char*);

    /* gl_module is for opening the DLL, and the quit flag is here to prevent
       quitting when recreating the window (see the window_procedure function) */

    HMODULE gl_module;
    bool quit = false;

    /* OpenGL function declarations. In practice, we would put these in a
       separate header file and add "extern" in front, so that we can use them
       anywhere after loading them only once. */

    PFNWGLGETEXTENSIONSSTRINGARBPROC wglGetExtensionsStringARB;
    PFNWGLCHOOSEPIXELFORMATARBPROC wglChoosePixelFormatARB;
    PFNWGLCREATECONTEXTATTRIBSARBPROC wglCreateContextAttribsARB;
    PFNGLGETSTRINGPROC glGetString;

    int WINAPI WinMain(HINSTANCE instance_handle, HINSTANCE prev_instance_handle, PSTR cmd_line, int cmd_show) {
        /* REGISTER WINDOW */
        WNDCLASS window_class;

        // Clear all structure fields to zero first
        ZeroMemory(&window_class, sizeof(window_class));

        // Define fields we need (others will be zero)
        window_class.style = CS_HREDRAW | CS_VREDRAW | CS_OWNDC;
        window_class.lpfnWndProc = window_procedure;
        window_class.hInstance = instance_handle;
        window_class.lpszClassName = TEXT("OPENGL_WINDOW");

        // Give our class to Windows
        RegisterClass(&window_class);
        /* *************** */
            
        /* CREATE WINDOW */
        HWND window_handle = CreateWindowEx(WS_EX_OVERLAPPEDWINDOW,
                                            TEXT("OPENGL_WINDOW"),
                                            TEXT("OpenGL window"),
                                            WS_OVERLAPPEDWINDOW,
                                            0, 0,
                                            800, 600,
                                            NULL,
                                            NULL,
                                            instance_handle,
                                            NULL);
            
        HDC dc = GetDC(window_handle);
            
        ShowWindow(window_handle, SW_SHOW);
        /* ************* */
            
        /* PIXEL FORMAT */
        PIXELFORMATDESCRIPTOR descriptor;
            
        // Clear all structure fields to zero first
        ZeroMemory(&descriptor, sizeof(descriptor));
            
        // Describe our pixel format
        descriptor.nSize = sizeof(descriptor);
        descriptor.nVersion = 1;
        descriptor.dwFlags = PFD_DRAW_TO_WINDOW | PFD_DRAW_TO_BITMAP | PFD_SUPPORT_OPENGL | PFD_GENERIC_ACCELERATED | PFD_DOUBLEBUFFER | PFD_SWAP_LAYER_BUFFERS;
        descriptor.iPixelType = PFD_TYPE_RGBA;
        descriptor.cColorBits = 32;
        descriptor.cRedBits = 8;
        descriptor.cGreenBits = 8;
        descriptor.cBlueBits = 8;
        descriptor.cAlphaBits = 8;
        descriptor.cDepthBits = 32;
        descriptor.cStencilBits = 8;
            
        // Ask for a similar supported format and set it
        int pixel_format = ChoosePixelFormat(dc, &descriptor);
        SetPixelFormat(dc, pixel_format, &descriptor);
        /* *********************** */
            
        /* RENDERING CONTEXT */
        HGLRC rc = wglCreateContext(dc);
        wglMakeCurrent(dc, rc);
        /* ***************** */

        /* LOAD FUNCTIONS (should probably be put in a separate procedure) */
        gl_module = LoadLibrary(TEXT("opengl32.dll"));

        wglGetExtensionsStringARB = (PFNWGLGETEXTENSIONSSTRINGARBPROC)get_proc("wglGetExtensionsStringARB");
        wglChoosePixelFormatARB = (PFNWGLCHOOSEPIXELFORMATARBPROC)get_proc("wglChoosePixelFormatARB");
        wglCreateContextAttribsARB = (PFNWGLCREATECONTEXTATTRIBSARBPROC)get_proc("wglCreateContextAttribsARB");
        glGetString = (PFNGLGETSTRINGPROC)get_proc("glGetString");
        
        FreeLibrary(gl_module);
        /* ************** */

        /* PRINT VERSION */
        const GLubyte *version = glGetString(GL_VERSION);
        printf("%s\n", version);
        fflush(stdout);
        /* ******* */

        /* NEW PIXEL FORMAT*/
        int pixel_format_arb;
        UINT pixel_formats_found;
        
        int pixel_attributes[] = {
            WGL_SUPPORT_OPENGL_ARB, 1,
            WGL_DRAW_TO_WINDOW_ARB, 1,
            WGL_DRAW_TO_BITMAP_ARB, 1,
            WGL_DOUBLE_BUFFER_ARB, 1,
            WGL_SWAP_LAYER_BUFFERS_ARB, 1,
            WGL_COLOR_BITS_ARB, 32,
            WGL_RED_BITS_ARB, 8,
            WGL_GREEN_BITS_ARB, 8,
            WGL_BLUE_BITS_ARB, 8,
            WGL_ALPHA_BITS_ARB, 8,
            WGL_DEPTH_BITS_ARB, 32,
            WGL_STENCIL_BITS_ARB, 8,
            WGL_ACCELERATION_ARB, WGL_FULL_ACCELERATION_ARB,
            WGL_PIXEL_TYPE_ARB, WGL_TYPE_RGBA_ARB,
            0
        };

        BOOL result = wglChoosePixelFormatARB(dc, pixel_attributes, NULL, 1, &pixel_format_arb, &pixel_formats_found);

        if (!result) {
            printf("Could not find pixel format\n");
            fflush(stdout);
            return 0;
        }
        /* **************** */

        /* RECREATE WINDOW */
        wglMakeCurrent(dc, NULL);
        wglDeleteContext(rc);
        ReleaseDC(window_handle, dc);
        DestroyWindow(window_handle);
        
        window_handle = CreateWindowEx(WS_EX_OVERLAPPEDWINDOW,
                                            TEXT("OPENGL_WINDOW"),
                                            TEXT("OpenGL window"),
                                            WS_OVERLAPPEDWINDOW,
                                            0, 0,
                                            800, 600,
                                            NULL,
                                            NULL,
                                            instance_handle,
                                            NULL);
            
        dc = GetDC(window_handle);
            
        ShowWindow(window_handle, SW_SHOW);
        /* *************** */

        /* NEW CONTEXT */
        GLint context_attributes[] = {
            WGL_CONTEXT_MAJOR_VERSION_ARB, 3,
            WGL_CONTEXT_MINOR_VERSION_ARB, 3,
            WGL_CONTEXT_PROFILE_MASK_ARB, WGL_CONTEXT_CORE_PROFILE_BIT_ARB,
            0
        };

        rc = wglCreateContextAttribsARB(dc, 0, context_attributes);
        wglMakeCurrent(dc, rc);
        /* *********** */
            
        /* EVENT PUMP */
        MSG msg;
            
        while (true) {
            if (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) {
                if (msg.message == WM_QUIT) 
                    break;
                    
                TranslateMessage(&msg);
                DispatchMessage(&msg);
            }
                
            // draw(); <- there goes your drawing
                
            SwapBuffers(dc);
        }
        /* ********** */
            
        return 0;
    }

    // Procedure that processes window events
    LRESULT CALLBACK window_procedure(HWND window_handle, UINT message, WPARAM param_w, LPARAM param_l)
    {
        /* When destroying the dummy window, WM_DESTROY message is going to be sent,
           but we don't want to quit the application then, and that is controlled by
           the quit flag. */

        switch(message) {
        case WM_DESTROY:
            if (!quit) quit = true;
            else PostQuitMessage(0);
            return 0;
        }

        return DefWindowProc(window_handle, message, param_w, param_l);
    }

    /* A procedure for getting OpenGL functions and OpenGL or WGL extensions.
       When looking for OpenGL 1.2 and above, or extensions, it uses wglGetProcAddress,
       otherwise it falls back to GetProcAddress. */
    void* get_proc(const char *proc_name)
    {
        void *proc = (void*)wglGetProcAddress(proc_name);
        if (!proc) proc = (void*)GetProcAddress(gl_module, proc_name);

        return proc;
    }

Compiled with <code>g++ GLExample.cpp -lopengl32 -lgdi32</code> with MinGW/Cygwin or <code>cl GLExample.cpp opengl32.lib gdi32.lib user32.lib</code> with MSVC compiler. Make sure however, that the headers from the OpenGL registry are in the include path. If not, use <code>-I</code> flag for <code>g++</code> or <code>/I</code> for <code>cl</code> in order to tell the compiler where they are.

  [1]: https://www.opengl.org/registry/
  [2]: https://msdn.microsoft.com/en-us/library/dd374394(v=vs.85).aspx

## Setup Modern OpenGL 4.1 on macOS (Xcode, GLFW and GLEW)
**1. Install GLFW**

First step is to create an OpenGL window. GLFW is an Open Source, multi-platform library for creating windows with OpenGL, to install GLFW first download its files from [www.glfw.org][1]

[![GLFW Webpage][2]][2]

Extract the GLFW folder and its contents will look like this

[![GLFW Folder Contents][3]][3]

Download and install CMake to build GLFW. Goto [www.cmake.org/download/][4], download CMake and install for MAC OS X

[![CMake Downloads Webpage][5]][5]

If Xcode is not installed. Download and install Xcode from Mac App Store.

[![Xcode from Mac App Store][6]][6]

Create a new folder **Build** inside the GLFW folder

[![GLFW folder after creating "Build" folder][7]][7]

Open CMake, click on **Browse Source** button to select the GLFW folder (make sure that CMakeLists.txt) is located inside that folder. After that, click on **Browse Build** button and select the newly created **Build** folder in previous step.

[![CMake Paths][8]][8]

Now Click on **Configure** button and select **Xcode** as generator with **Use default native compilers option**, and click **Done**.

[![Makefile for Xcode][9]][9]

Tick on **BUILD_SHARED_LIBS** option and then click on **Configure** button again and finally click **Generate** button.

[![Select BUILD_SHARED_LIBS][10]][10]

After generation CMake should look like this

[![Final CMake][11]][11]

Now Open **Finder** and goto **/usr**, create a folder name **local** if not already there. Open the **local** folder and create two folders **include** and **lib** if not already there.

Now open the GLFW folder and goto **Build** (where CMake had built the files). Open **GLFW.xcodeproj** file in Xcode.

[![Xcode Project File][12]][12]

Select **install > My Mac** and then click on **run** (Play shaped button).

[![Install GLFW][13]][13]

It is now successfully installed (ignore the warnings).

To make sure Open Finder and goto **/usr/local/lib** folder and three GLFW library files will be already present there (If not then open **Build** folder inside GLFW folder and go to **src/Debug** copy all files to **/usr/local/lib**)

[![GLFW Lib Files][14]][14]

Open Finder and goto **/usr/local/include** and a GLFW folder will be already present there with two header files inside it by name of **glfw3.h** and **glfw3native.h**

[![GLFW Header Files][15]][15]

**2. Install GLEW**

GLEW is a cross-platform library that helps in querying and loading OpenGL extensions. It provides run-time mechanisms for determining which OpenGL extensions are supported on the target platform. It is only for modern OpenGL (OpenGL version 3.2 and greater which requires functions to be determined at runtime). To install first download its files from [glew.sourceforge.net][16]

[![GLEW Webpage][17]][17]

Extract the GLFW folder and its contents will look like this.

[![GLEW Folder Contents][18]][18]

Now open Terminal, navigate to GLEW Folder and type the following commands

    make
    sudo make install 
    make clean

Now GLEW is successfully installed. To make sure its installed, Open Finder, go to **/usr/local/include** and a GL folder will be already present there with three header files inside it by name of **glew.h**, **glxew.h** and **wglew.h**

[![GLEW Header Files][19]][19]

Open Finder and go to **/usr/local/lib** and GLEW library files will be already present there

[![GLEW Library Files][20]][20]

**3. Test and Run**

Now we have successfully installed GLFW and GLEW. Its time to code. Open Xcode and create a new Xcode project. Select **Command Line Tool** then proceed next and select **C++** as language.

[![Xcode Project][21]][21]

Xcode will create a new command line project.

Click on project name, and under **Build Settings** tab switch from **Basic to All**, under **Search Paths** section, add **/usr/local/include** in Header Search Paths and add **/usr/local/lib** in Library Search Paths

[![Search Paths][22]][22]

Click on project name, and under **Build Phases** tab and under **Link With Binary Libraries** add **OpenGL.framework** and also add recently created **GLFW** and **GLEW** libraries from **/usr/local/lib**

[![Link Binaries][23]][23]


Now we are ready to code in Modern Open GL 4.1 on macOS using C++ and Xcode. The following code will create an OpenGL Window using GLFW with Blank Screen Output.

    #include <GL/glew.h> 
    #include <GLFW/glfw3.h>

    // Define main function
    int main() 
    {
        // Initialize GLFW
        glfwInit();
    
        // Define version and compatibility settings
        glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
        glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2); 
        glfwWindowHint(GLFW_OPENGL_PROFILE,GLFW_OPENGL_CORE_PROFILE);
        glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE); 
        glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);
    
        // Create OpenGL window and context
        GLFWwindow* window = glfwCreateWindow(800, 600, "OpenGL", NULL, NULL);
        glfwMakeContextCurrent(window);
    
        // Check for window creation failure
        if (!window) 
        {
            // Terminate GLFW
            glfwTerminate();
            return 0; 
        }
    
        // Initialize GLEW
        glewExperimental = GL_TRUE; glewInit();
    
        // Event loop
        while(!glfwWindowShouldClose(window)) 
        {
            // Clear the screen to black
            glClearColor(0.0f, 0.0f, 0.0f, 1.0f); glClear(GL_COLOR_BUFFER_BIT);
            glfwSwapBuffers(window);
            glfwPollEvents(); 
        }
    
        // Terminate GLFW
        glfwTerminate(); return 0;
    }

  [![Blank OpenGL Window][24]][24]


  [1]: http://www.glfw.org
  [2]: http://i.stack.imgur.com/YXXOU.png
  [3]: http://i.stack.imgur.com/6QE70.png
  [4]: http://www.cmake.org/download/
  [5]: http://i.stack.imgur.com/TkDNc.png
  [6]: http://i.stack.imgur.com/BtoEq.png
  [7]: http://i.stack.imgur.com/EzceG.png
  [8]: http://i.stack.imgur.com/N0RTp.png
  [9]: http://i.stack.imgur.com/8Xnzf.png
  [10]: http://i.stack.imgur.com/4ffVA.png
  [11]: http://i.stack.imgur.com/jf3fQ.png
  [12]: http://i.stack.imgur.com/odeTV.png
  [13]: http://i.stack.imgur.com/YpZmD.png
  [14]: http://i.stack.imgur.com/MySGM.png
  [15]: http://i.stack.imgur.com/M3BSz.png
  [16]: http://glew.sourceforge.net
  [17]: http://i.stack.imgur.com/8W8sl.png
  [18]: http://i.stack.imgur.com/LTq72.png
  [19]: http://i.stack.imgur.com/2Atrm.png
  [20]: http://i.stack.imgur.com/4AJdj.png
  [21]: http://i.stack.imgur.com/hhNhd.png
  [22]: http://i.stack.imgur.com/T7lMy.png
  [23]: http://i.stack.imgur.com/xMzCH.png
  [24]: http://i.stack.imgur.com/AIE8O.png

## Cross Platform OpenGL context creation (using SDL2)
Creating a Window with OpenGL context (extension loading through [GLEW](http://glew.sourceforge.net/)):

```C
#define GLEW_STATIC

#include <GL/glew.h>
#include <SDL2/SDL.h>

int main(int argc, char* argv[])
{
    SDL_Init(SDL_INIT_VIDEO); /* Initialises Video Subsystem in SDL */

    /* Setting up OpenGL version and profile details for context creation */
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 2);
    
    /* A 800x600 window. Pretty! */
    SDL_Window* window = SDL_CreateWindow
        (
        "SDL Context",
        SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
        800, 600,
        SDL_WINDOW_OPENGL
        );
    
    /* Creating OpenGL Context */
    SDL_GLContext gl_context = SDL_GL_CreateContext(window);

    /* Loading Extensions */
    glewExperimental = GL_TRUE;
    glewInit();

    /* The following code is for error checking. 
    *  If OpenGL has initialised properly, this should print 1.
    *  Remove it in production code.
    */
    GLuint vertex_buffer;
    glGenBuffers(1, &vertex_buffer);
    printf("%u\n", vertex_buffer);
    /* Error checking ends here */

    /* Main Loop */
    SDL_Event window_event;
    while(1) {
        if (SDL_PollEvent(&window_event)) {
            if (window_event.type == SDL_QUIT) {
                /* If user is exiting the application */
                break;
            }
        }
        /* Swap the front and back buffer for flicker-free rendering */
        SDL_GL_SwapWindow(window);
    }
    
    /* Freeing Memory */
    glDeleteBuffers(1, &vertex_buffer);
    SDL_GL_DeleteContext(gl_context);
    SDL_Quit();

    return 0;
}
```

## Creating OpenGL 4.1 with C++ and Cocoa


## Obtaining OpenGL
One of the most common misconceptions about OpenGL is, that it were a library that could be installed from 3rd party sources. This misconception leads to many questions in the form "how to I install OpenGL" or "where to download the OpenGL SDK".

This is not how OpenGL finds the way into computer system. OpenGL by itself is merely a set of specifications on what commands an implementation must follow. So it's the implementation that matters. And for the time being, OpenGL implementations are part of the GPU drivers. This *might* change in the future, when new GPU programming interface allow to truly implement OpenGL as a library, but for now it's a programming API towards the graphics drivers.

When OpenGL got first released the API somehow found its way into the ABI (Application Binary Interface) contract of Windows, Solaris and Linux (LSB-4 Desktop) in addition to it's origin Sun Irix. Apple followed and in fact integrated OpenGL so deep into MacOS X, that the OpenGL version available is tightly coupled to the version of MacOS X installed. This has the notable effect, that system programming environments for these operating systems (i.e. the compiler and linker toolchain that natively targets these systems) **must** deliver also OpenGL API definitions. Such it is not necessary to actually install an SDK for OpenGL. It is technically possible to program OpenGL on these operating systems without the requirement to install a dedicated SDK, assuming that a build environment following the targeted ABI is installed.

A side effect of these strict ABI rules is, that the OpenGL version exposed through the binding interface is a lowest common denominator that programs running on the target platform may expect to be available. Hence modern OpenGL features are to be accessed through the extension mechanism, which is described in depth separately.

## Linux
In Linux it is quite common to compartmentize the development packages for different aspects of the system, so that these can be updated individually. In most Linux distributions the development files for OpenGL are contained in a dedicated package, that is usually a dependency for a desktop application development meta-package. So installing the OpenGL development files for Linux is usually taken care of with the installation of the desktop development meta package/s.*

## Microsoft Windows
The API binding library `opengl32.dll` (named so for both 32 bit and 64 bit versions of Windows) is shipped by default with every Windows version since Windows NT-4 and Windows 95B (both ca. 1997). However this DLL does not provide an actual OpenGL implementation (apart from a software fallback which sole purpose is to act as a safety net for programs if no other OpenGL implementation is installed). This DLL belongs to Windows and **must not** be altered or moved! Modern OpenGL versions are shipped as part of the so called *Installable Client Driver* (ICD) and accessed through the default `opengl32.dll` that comes pre-installed with every version of Windows. It was decided internally by Microsoft, however, that graphics drivers installed through *Windows Update* would not install/update a OpenGL ICD. As such fresh installations of Windows with drivers installed automatically are lacking support for modern OpenGL features. To obtain an OpenGL ICD with modern features, graphics drivers must be downloaded directly from the GPU vendor's website and installed manually.

Regarding development no extra steps must be taken per-se. All C/C++ compilers following the Windows ABI specifications ship with headers and the linker stub (opengl32.lib) required to build and link executables that make use of OpenGL. 

## Create Opengl Context with Java and LWJGL 3.0
In this example code we will create A blank Opengl Window using LWJGL 3.0+, this doesn't contain steps to create the project in your IDE

[![enter image description here][1]][1]

 1. Create a class name WindowManager that will contain all the boiler
    plate code for creating a opengl context window on screen

> WindowManager.java

    import org.lwjgl.glfw.*;
    import static org.lwjgl.glfw.Callbacks.*;
    import static org.lwjgl.glfw.GLFW.*;
    import static org.lwjgl.opengl.GL11.*;
    import static org.lwjgl.system.MemoryUtil.*;

    /**
     * Class Containing code related to inflating Opengl Window
     */
    public class Displaymanager {

        private static long window;
    
        public static void createDisplay(){
            // Setup an error callback. The default implementation
            // will print the error message in System.err.
            GLFWErrorCallback.createPrint(System.err).set();

            // Initialize GLFW. Most GLFW functions will not work before doing this.
            if ( !glfwInit() )
                throw new IllegalStateException("Unable to initialize GLFW");

            // Configure our window
            glfwDefaultWindowHints(); // optional, the current window hints are already the default
            glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE); // the window will stay hidden after creation
            glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE); // the window will be resizable

            int WIDTH = 300;
            int HEIGHT = 300;

            // Create the window
            window = glfwCreateWindow(WIDTH, HEIGHT, "Hello World!", NULL, NULL);
            if ( window == NULL )
                throw new RuntimeException("Failed to create the GLFW window");

            // Setup a key callback. It will be called every time a key is pressed, repeated or released.
            glfwSetKeyCallback(window, (window, key, scancode, action, mods) -> {
                if ( key == GLFW_KEY_ESCAPE && action == GLFW_RELEASE )
                    glfwSetWindowShouldClose(window, true); // We will detect this in our rendering loop
            });

            // Get the resolution of the primary monitor
            GLFWVidMode vidmode = glfwGetVideoMode(glfwGetPrimaryMonitor());
            // Center our window
            glfwSetWindowPos(
                    window,
                    (vidmode.width() - WIDTH) / 2,
                    (vidmode.height() - HEIGHT) / 2
            );

            // Make the OpenGL context current
            glfwMakeContextCurrent(window);
            // Enable v-sync
            glfwSwapInterval(1);

            // Make the window visible
            glfwShowWindow(window);
        }

        public static boolean isCloseRequested(){
            return glfwWindowShouldClose(window);
        }

        public static void updateDisplay(){
            glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); // clear the framebuffer

            glfwSwapBuffers(window); // swap the color buffers

            // Poll for window events. The key callback above will only be
            // invoked during this call.
            glfwPollEvents();
        }

        public static void destroyDisplay(){
            // Terminate GLFW and free the error callback
            cleanUp();
            glfwTerminate();
            glfwSetErrorCallback(null).free();
        }

        private static void cleanUp() {
            // Free the window callbacks and destroy the window
            glfwFreeCallbacks(window);
            glfwDestroyWindow(window);
        }
    }

 2. Next create a class that contain main rendering loop ,which will call all the above function created

> OpenGlMain.java

    import org.lwjgl.opengl.GL;
    import renderEngine.Displaymanager;
    import static org.lwjgl.opengl.GL11.glClearColor;


    /**
     * Class to test the opengl Window
     */
    public class OpenGlMain {

        public static void main(String[] args) {

            Displaymanager.createDisplay();

            // This line is critical for LWJGL's interoperation with GLFW's
            // OpenGL context, or any context that is managed externally.
            // LWJGL detects the context that is current in the current thread,
            // creates the GLCapabilities instance and makes the OpenGL
            // bindings available for use.
            GL.createCapabilities();

            while (!Displaymanager.isCloseRequested()){

                // Set the clear color
                glClearColor(1.0f, 0.0f, 0.0f, 0.0f);

                Displaymanager.updateDisplay();
            }

            Displaymanager.destroyDisplay();
        }
    }

For further detail checkout official [LWJGL Guide][2]


  [1]: http://i.stack.imgur.com/cWrS3.png
  [2]: https://www.lwjgl.org/guide

