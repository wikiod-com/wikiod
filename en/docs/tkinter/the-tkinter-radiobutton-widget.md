---
title: "The Tkinter Radiobutton widget"
slug: "the-tkinter-radiobutton-widget"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Syntax
 - radiobutton = tk.Radiobutton(parent, **kwargs)


## Parameters
| Parameter | Description |
| ------ | ------ |
| parent   | tkinter widgets exist in a hierarchy. Except for the root window, all widgets have a parent. Some online tutorials call this "master". When the widget is added to the screen with pack, place or grid, it will appear inside this parent widget. 
|
| command  | function called each time the user changes the state of the radiobutton
|
| indicatoron | 1 or True for radio buttons, 0 or False for button boxes
|
| text      | Text to display next to the radiobutton.
|
| value   | When the radiobutton is selected, the associated control variable is set to value.
|
| variable  | Control variable the radiobutton shares with the other radiobutton of the group.

These examples assume that tkinter has been imported with either `import tkinter as tk` (python 3) or `import Tkinter as tk` (python 2).

**Reference:**

[![enter image description here][1]][1]
>To turn the above example into a “button box” rather than a set of radio buttons, set the indicatoron option to 0. In this case, there’s no separate radio button indicator, and the selected button is drawn as SUNKEN instead of RAISED:

[![enter image description here][2]][2]

-[effbot](http://effbot.org/tkinterbook/radiobutton.htm)


  [1]: http://i.stack.imgur.com/mR7SL.png
  [2]: http://i.stack.imgur.com/6Nwdz.png

## Here's an example of how to turn radio buttons to button boxes:
    import tkinter as tk
    root = tk.Tk()
    
    rbvar = StringVar()
    rbvar.set(" ")
    
    rb1 = tk.Radiobutton(root, text="Option 1", variable=rbvar, value='a', indicatoron=0)
    rb1.pack()
    
    rb2 = tk.Radiobutton(root, text="Option 2", variable=rbvar, value='b', indicatoron=0)
    rb2.pack()



## Create a group of radiobuttons
Such a group is made of radiobuttons that share a control variable so that no more than one can be selected.

    # control variable
    var = tk.IntVar(parent, 0)

    # group of radiobuttons
    for i in range(1,4):
        tk.Radiobutton(parent, text='Choice %i' % i, value=i, variable=var).pack()

    tk.Button(parent, text='Print choice', command=lambda: print(var.get())).pack()



