---
title: "Creating Windows Forms with IronPython"
slug: "creating-windows-forms-with-ironpython"
draft: false
images: []
weight: 9907
type: docs
toc: true
---

## Hello Word example using Windows Forms
First, references will be added to the CLR assemblies that will be used. 

    import clr
    clr.AddReference('System.Windows.Forms')

Next the names we will use are imported.

    from System.Windows.Forms import Application, Form

A class will be created for the Hello World form using `Form` as its subclass.

    class HelloWorldForm(System.Windows.Forms.Form):
        def __init__(self):
            self.Text = 'Hello World'
            self.Name = 'Hello World'

The text attribute of the form sets the title bar's text.

To run the application, we create an instance of the `HelloWorldForm`.

    form = HelloWorldForm()
    Application.Run(form)

The `Application` class provides static methods and such as starting and stopping an application. The `Run` static method runs the form on the current thread.

