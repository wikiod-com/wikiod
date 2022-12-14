---
title : mfc Tutorial
slug : mfc-tutorial
weight : 9963
draft : false
images : []
type : docs
---

The **Microsoft Foundation Classes**, or **MFC**, is a library that provides an object-oriented wrapper around the Win32 API. By encapsulating the "raw" Win32 API in C++ classes, MFC makes it significantly easier to create GUI applications and manage resources.

MFC has been around a very long time. It was first introduced in 1992 with version 7 of Microsoft's C/C++ compiler. At this time, C++ development was just beginning to take off. Subsequent versions of Visual Studio have shipped with significantly improved versions of MFC. It is still included with the latest version of Visual Studio 2015. But its legacy roots are unfortunately quite visible. Since most of it was developed prior to the standardization of the C++ language, the MFC classes do not make full use of modern C++ features like templates, provide their own custom implementations of other standard C++ features like RTTI, and use many non-standard idioms. These facts make it nearly impossible to compile an MFC application with any compiler other than Microsoft's. On the plus side, though, MFC is well-integrated into Visual Studio, making development significantly easier.

During early development, the library was known as Application Framework Extensions (abbreviated to AFX). The marketing department later changed its name to MFC, but it was too late to change any of the code, so much of the code still references "Afx" instead of "Mfc". A noticeable example is the standard pre-compiled header file that is automatically generated by Visual Studio: it is named `StdAfx.h`.

On 7 April 2008, Microsoft released an update to the MFC classes as an out-of-band update to Visual Studio 2008 and MFC 9. The update features new user interface constructs, including the ribbons (similar to that of Microsoft Office 2007) and associated UI widgets, fully customizable toolbars, docking panes (like Visual Studio 2005) which can either be freely floated or docked to any side and document tabs. The new functionality is provided in new classes so that old applications still continue to run. This update is building on top of [BCGSoft???s](https://www.bcgsoft.com/index.htm) BCGControlBar Library Professional Edition and was named as **MFC Feature Pack**. 

So now MFC consists from two library with different approaches:
- Classic MFC (wrapper for Win32 API).
- MFC Feature pack (mixed set from Win32 API controls and new self-drawing controls, like Ribbon).


### See also:

 - [MFC Desktop Applications](https://msdn.microsoft.com/en-us/library/d06h2x6e.aspx) (overview)
 - [MFC Reference](https://msdn.microsoft.com/en-us/library/d06h2x6e.aspx) (API reference)
 - [MFC Feature Pack](https://msdn.microsoft.com/en-us/library/bb982354(v=vs.100).aspx) (overview)
 - [MFC Feature Pack Reference](https://msdn.microsoft.com/en-us/library/bb983528(v=vs.100).aspx) (Feature Pack API reference)
 - [MFC Feature Pack Samples](https://msdn.microsoft.com/en-us/library/bb983962(v=vs.100).aspx)

