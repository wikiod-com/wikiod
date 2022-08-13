---
title: "kivy - Cross-platform Python Framework for NUI Development"
slug: "kivy---cross-platform-python-framework-for-nui-development"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

NUI : A natural user interface (NUI) is a system for human-computer interaction that the user operates through intuitive actions related to natural, everyday human behavior. 

Kivy is a Python library for development of multi-touch enabled media rich applications which can be installed on different devices. Multi-touch refers to the ability of a touch-sensing surface (usually a touch screen or a trackpad) to detect or sense input from two or more points of contact simultaneously.


## First App

To create an kivy application 

 1. sub class the **app** class
 2. Implement the **build** method, which will return the widget.
 3. Instantiate the class an invoke the **run**.


    from kivy.app import App
    from kivy.uix.label import Label

    class Test(App):
        def build(self):
            return Label(text='Hello world')

    if __name__ == '__main__':
        Test().run()

**Explanation**

    from kivy.app import App
The above statement will import the parent class **app**. This will be present in your installation directory your_installtion_directory/kivy/app.py

    from kivy.uix.label import Label
The above statement will import the ux element **Label**. All the ux element are present in your installation directory your_installation_directory/kivy/uix/.

    class Test(App):
The above statement is for to create your app and class name will be your app name. This class is inherited the parent app class.

    def build(self):
The above statement override the build method of app class. Which will return the widget that needs to be shown when you will start the app.

    return Label(text='Hello world')
The above statement is the body of the build method. It is returning the Label with its text **Hello world**.

    if __name__ == '__main__':
The above statement is the entry point from where python interpreter start executing your app.

    Test().run()
The above statement Initialise your Test class by creating its instance. And invoke the app class function run().

Your app will look like the below picture.

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/CnCNj.png

