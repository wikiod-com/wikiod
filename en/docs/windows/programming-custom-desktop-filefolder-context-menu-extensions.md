---
title: "Programming Custom Desktop FileFolder context menu extensions"
slug: "programming-custom-desktop-filefolder-context-menu-extensions"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Using Registry entries
This works well for single level right click context menu. All you need to do is create a registry entry under Classes Root HKEY_CLASSES_ROOT for specific extension. If you want to create a entry for all types of files choose * else choose extension like .pdf etc.

       var regmenu = Registry.ClassesRoot.CreateSubKey("*\\shell\\hello");
       if (regmenu != null)
           regmenu.SetValue("", "Hello World");
       var regcmd = Registry.ClassesRoot.CreateSubKey("*\\shell\\hello\\command");
           if (regcmd != null)
                regcmd.SetValue("", "Do something" );

This works well for one level menus

Simple example is at http://www.codeproject.com/KB/cs/appendmenu.aspx?msg=2236729

## Using ShellSharp
When you need multi level menus, with multiple parameters SharpShell comes to rescue. 
https://github.com/dwmkerr/sharpshell has umpteen number of examples and it works perfect even for single level to multi level custom context menus.

Key thing is to create class with attributes [ComVisible(true)] and     [COMServerAssociation(AssociationType.AllFiles)] and inheriting class fromSharpContextMenu which implements CanShowMenu and CreateMenu functions and you need to register the assembly via regasm tool or ServerRegistrationManager that Sharpshell creator recommends

    [ComVisible(true)]
    [COMServerAssociation(AssociationType.AllFiles)]
    public class AdvancedContextMenu : SharpContextMenu
    {
        
        protected override bool CanShowMenu()
        {
            //  We can show the item only for a single selection.
        }
        protected override ContextMenuStrip CreateMenu()
        {
            //  Create the menu strip.
            var menu = new ContextMenuStrip();
            ... add any level of ToolStripMenuItems and add them to menu
            return menu
        }
    }

More details can be obtained at https://github.com/dwmkerr and http://www.codeproject.com/Articles/512956/NET-Shell-Extensions-Shell-Context-Menus

