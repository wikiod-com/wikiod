---
title : winforms Tutorial
slug : winforms-tutorial
weight : 9940
draft : false
images : []
type : docs
---

**Windows Forms** ("WinForms" for short) is a GUI class library included with the .NET Framework. It is a sophisticated object-oriented wrapper around the [Win32 API](https://www.wikiod.com/docs/winapi), allowing the development of Windows desktop and mobile applications that target the [.NET Framework](https://www.wikiod.com/docs/dotnet).

WinForms is primarily [event-driven](https://en.wikipedia.org/wiki/Event-driven_programming). An application consists of multiple *forms* (displayed as windows on the screen), which contain *controls* (labels, buttons, textboxes, lists, etc.) that the user interacts with directly. In response to user interaction, these controls raise events that can be handled by the program to perform tasks.

Like in Windows, everything in WinForms is a control, which is itself a type of window. The base Control class provides basic functionality, including properties for setting text, location, size, and color, as well as a common set of events that can be handled. All controls derive from the Control class, adding additional features. Some controls can host other controls, either for reusability (`Form`, `UserControl`) or layout (`TableLayoutPanel`, `FlowLayoutPanel`).

WinForms has been supported since the original version of the .NET Framework (v1.0), and is still available in modern versions (v4.5). However, it is no longer under active development, and no new features are being added. [According to 9 Microsoft developers at the Build 2014 conference](https://www.infoq.com/news/2014/04/WPF-QA):

> Windows Forms is continuing to be supported, but in maintenance mode. They will fix bugs as they are discovered, but new functionality is off the table.

The cross-platform, open-source [Mono library](http://www.mono-project.com/docs/gui/winforms/) provides a basic implementation of Windows Forms, supporting all of the features that Microsoft's implementation did as of .NET 2.0. However, WinForms is not actively developed on Mono and a complete implementation is considered impossible, given how inextricably linked the framework is with the native Windows API (which is unavailable in other platforms).

### See also:

- [Windows Forms](https://msdn.microsoft.com/en-us/library/dd30h2yb.aspx) documentation on MSDN

