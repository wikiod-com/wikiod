---
title: "Creating Maya UI"
slug: "creating-maya-ui"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Parameters
| parameter| details|
|--------|---------|
| e / edit | tells Maya you want to change the value of an existing property |
| q / query| tells Maya you want to get the value of an existing property |

Maya comes with a fairly complete UI toolkit that includes windows, layouts, and a variety of controls. This is implemented using the [QT](https://www.wikiod.com/qt) framework in C++, but exposed to MEL and Python users via the default Maya command set.  

QT
===
Advanced users can extend the Maya UI using either C++ or Python. Maya versions from 2012 to 2016 use Pyside and QT4; Maya 2017 uses Pyside2 and QT5.  More details [here](https://fredrikaverpil.github.io/2016/07/25/dealing-with-maya-2017-and-pyside2/)


**Note:** Older reference on the web refers to the Maya GUI toolkit as "ELF"; that's still the correct name but it's rarely used.

## Callback functions
Many widgets include events which can fire callback functions when the user interacts with the widget. For example when a button is pressed, a checkbox checked, or a dropdown chosen you can fire a function.

The exact flag which is associated with these event depends on the widget, but a typical callback would look like this:


     def callback_fn(_ignore):
         print "button pressed"

     button = cmds.button(label='press me', command = callback_fn)

Pressing the button will print "button pressed" to the listener window. Most widget's fire some arguments when their callbacks activate -- the `button` for example always includes a boolean value -- so you'll need to make sure that the callback handler has the right signature to go with the widget you're using.  That's why  `callback_fn()` takes an argument even though it doesn't need it.

Callback Assignment
----------------------
Maya supports two different ways of attaching callback functions:

       # this works, but is not a great idea
       cmds.button(label = 'string reference', command = 'string_name_of_function')
       # use this form whenever possible
       cmds.button(label = 'direct function reference', command = callback_fn)

In the first example the callback is assigned by a string value. Maya will find the callback in the global Python scope -- which is usually hard to access when writing properly organized code. String-name callbacks are also slower to resolve.  The second example passes the actual Python function to the callback -- this form is preferred because it is faster and, if you've failed to provide a valid function to the callback function, you'll know when the UI is created instead of when the UI widgets are actually used.  

If you want to pass a argument value to a call back function, you can use a [lambda](http://stackoverflow.com/questions/890128/why-are-python-lambdas-useful), a [closure](http://www.shutupandship.com/2012/01/python-closures-explained.html) or a [functools.partial](https://www.pydanny.com/python-partials-are-fun.html) bind arguments to the callback.


## Using `partial`:

     from functools import partial
     ....
     def callback_fn(myValue, _ignore):  # _ignore swallows the original button argument
         print myValue
    
     button = cmds.button(label='press me', command = partial(callback_fn, "fooo"))

## Using `lambda`:

     def callback_fn(myValue):
         print myValue
    
     button = cmds.button(label='press me', command = lambda _ignore: callback_fn("fooo"))
     # here the lambda needs to handle the button argument

## Using Closures

    b = cmds.button(label = 'press me')
    # by defining this function when `b` exists, we can use it later
    # without storing it explicitly
    def get_button_label(*_):
        print "label of button", b, " is ", cmds.button(b, q=True, l=True)
    cmds.button(b, e=True, c=get_button_label)

There' more about string callback names vs. callback function [here](https://theodox.github.io/2014/maya_callbacks_cheat_sheet#.WPqom4Qs5xU.link)  




## Basic UI example [Python]
The Maya GUI toolkit creates a variety of UI elements in a simple, imperative form.  There are basic commands to create and edit GUI widgets; the widgets are identified by a unique string name.   

All gui commands take the same basic form: you supply a command type and the string name of the object you want to work on or create, along with flags that specify the look or behavior of the widget.  So, for example, to create a button you'd use:

     cmds.button('my_button', label = 'my label')

This will create a new gui button. To edit the button you'd use the same command with the `edit` flag (the short version is just `e`).  So you could change the label of the button like this:

    cmds.button('my_button', e=True, label = 'a different label')

and you can query the current value of a property with the `query` or `q` flag:

    cmds.button(`my button`, q=True, label=True)
    # 'a different label'






## Widget naming
When you create a new widget with a UI command you can supply the name you'd like the new widget to get. However, its **not** guaranteed: Maya will give the button the name you asked for -- if you've given it a character it doesn't recognize or if there is already a widget with the same name you may get back a different name. It *always* a good practice to capture the name of a new widget when it's created to avoid surprises:

     my_button = cmds.button('my_button') 
     cmds.button(my_button, e=True, label = "a new label")


## Creating a window


    # create a window with a button that closes the window when clicked
    window = cmds.window(title='example window')       # create the window
    layout = cmds.columnLayout(adjustableColumn=True)  # add a vertical layout
    
    def close_window(*_):
        cmds.deleteUI(window)                          # deletes the window above

    button = cmds.button(label= 'press to close", command = close_window)

    # show the window
    cmds.showWindow(window)



## Lambdas and loops
Lambdas are a useful shortcut for hooking up behaviors to GUI elements. 

    b = cmds.button("make a cube", command = lambda _: cmds.polyCube())

However, due to the way Python captures variables inside of lambdas, you can get unexpected results if you bind commands using lambdas inside a loop.  For example this *looks* like it should produce buttons that create spheres of different sizes:

    # warning: doesn't work like it looks!
    for n in range(5):
        b = cmds.button("sphere size %i" % n, command = lambda _: cmds.polySphere(radius=n))

The buttons will be labelled correctly but will all use the same radius (4) because the lambdas will all capture that value when the loop closes.  *TLDR:* If you're generating callbacks inside of a loop, use `functools.partial` or another method for capturing values - lambdas don't work for this application.  See [here](http://blog.theodox.com/2014/maya_callbacks_cheat_sheet) for more details

