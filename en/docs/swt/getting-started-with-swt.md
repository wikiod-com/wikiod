---
title: "Getting started with swt"
slug: "getting-started-with-swt"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Creating a new SWT progam
Create a new text file named `HelloWorld.java` and paste this code in it:


<!-- language: lang-java -->

    import org.eclipse.swt.*;
    import org.eclipse.swt.layout.*;
    import org.eclipse.swt.widgets.*;
    
    public class HelloWorld
    {
        public static void main(String[] args)
        {
            final Display display = new Display();
            final Shell shell = new Shell(display);
            shell.setLayout(new FillLayout());
    
            Label label = new Label(shell, SWT.NONE);
            label.setText("Hello World!");
    
            shell.pack();
            shell.open();
    
            while (!shell.isDisposed())
            {
                if (!display.readAndDispatch())
                    display.sleep();
            }
            display.dispose();
        }
    }

When you start the program it will look something like this:

[![enter image description here][1]][1]

---

# A closer look at the Hello World application

The Hello World application consists of a `HelloWorld` class definition and a `main` method.

The main method defines a [`Display`](http://help.eclipse.org/kepler/index.jsp?topic=%2Forg.eclipse.platform.doc.isv%2Freference%2Fapi%2Forg%2Feclipse%2Fswt%2Fwidgets%2FDisplay.html) and a [`Shell`](http://help.eclipse.org/kepler/index.jsp?topic=%2Forg.eclipse.platform.doc.isv%2Freference%2Fapi%2Forg%2Feclipse%2Fswt%2Fwidgets%2FShell.html). The display acts as the interface between SWT and the underlying operating system. It handles the platform event model in the form of the SWT event loop. The shell represents a single window of the desktop or window manager.

Widgets are added to the shell by specifying the shell in the constructor of the widget. In this example we create a [`Label`](http://help.eclipse.org/kepler/index.jsp?topic=%2Forg.eclipse.platform.doc.isv%2Freference%2Fapi%2Forg%2Feclipse%2Fswt%2Fwidgets%2FLabel.html). A label is a widget that can display text or an image. In this case we set the text "Hello World!" to it. The widget is added to the shell by specifying our shell as the first argument in the constructor.

To make the label visible in the shell we either have to set a fixed size to it or we need to tell its parent (the shell) how to layout its children.

The [`FillLayout`](http://help.eclipse.org/kepler/index.jsp?topic=%2Forg.eclipse.platform.doc.isv%2Freference%2Fapi%2Forg%2Feclipse%2Fswt%2Flayout%2FFillLayout.html) is the simplest SWT [`Layout`](http://help.eclipse.org/kepler/index.jsp?topic=%2Forg.eclipse.platform.doc.isv%2Freference%2Fapi%2Forg%2Feclipse%2Fswt%2Fwidgets%2FLayout.html). It organizes all its children in a single row or column and forces them to have the same size.

The following lines tell the shell to apply its layout and become visible:

<!-- language: lang-java -->

    shell.pack();
    shell.open();

Last but most importantly, we need to define the event loop of the SWT program. The event loop is needed to transfer the user input events from the underlying operating system widgets to the SWT event system.

<!-- language: lang-java -->

    while (!shell.isDisposed())
    {
        if (!display.readAndDispatch())
            display.sleep();
    }
    display.dispose();

This loop will run until the shell is disposed. Once this happens, the display is disposed as well and the program will terminate. While the program is looping, it will read the next operating system event and transfer it to SWT. If there is no event, the thread will sleep until the next event arrives.


  [1]: http://i.stack.imgur.com/mgRjN.png

## Installation or Setup
Detailed instructions on getting swt set up or installed.

