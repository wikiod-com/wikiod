---
title: "The Tkinter Entry Widget"
slug: "the-tkinter-entry-widget"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Syntax
* entry = tk.Entry(*parent*, ***kwargs*)
* entry.get()
* entry.insert(index, "value")
* entry.delete(start_index, end_index)
* entry.bind(event, callback)

## Parameters
| Parameter | Description |
| ------ | ------ |
| parent   | tkinter widgets exist in a hieararchy. Except for the root window, all widgets have a parent. Some online tutorials call this "master". When the widget is added to the screen with `pack`, `place` or `grid`, it will appear inside this parent widget   |
| width | The width specifies the _desired_ width of the widget based on an average character width. For variable width fonts, this is based on the width of the zero character (`0`). The default is 20. Note that the actual width could be larger or smaller depending on how it is added to the screen.|


These examples assume that tkinter has been imported with either `import tkinter as tk` (python 3) or `import Tkinter as tk` (python 2).


## Getting the value of an Entry widget
The value of an entry widget can be obtained with the `get` method of the widget:

    name_entry = tk.Entry(parent)
    ...
    name = name_entry.get()

Optionally, you may associate an instance of a `StringVar`, and retrieve the value from the `StringVar` rather than from the widget:

    name_var = tk.StringVar()
    name_entry = tk.Entry(parent, textvariable=name_var)
    ...
    name = name_var.get()




## Creating an Entry widget and setting a default value
    entry = tk.Entry(parent, width=10)
    entry.insert(0, "Hello, World!")
    

## Adding validation to an Entry widget
To restrict the characters that can be typed into an entry widget, only numbers for instance, a validate command can be added to the entry. A validate command is a function that return `True` if the change is accepted, `False` otherwise. This function will be called each time the content of the entry is modified. Various arguments can be passed to this function, like the type of change (insertion, deletion), the inserted text, ... 

    def only_numbers(char):
        return char.isdigit()

    validation = parent.register(only_numbers)
    entry = Entry(parent, validate="key", validatecommand=(validation, '%S'))


The `validate` option determines the type of event that triggers the validation, here, it's any keystroke in the entry. The `'%S'` in the validatecommand option means that the inserted or deleted character is passed in argument to the `only_numbers` function. The full list of possibilities can be found [here][1].


  [1]: http://%20http://infohost.nmt.edu/tcc/help/pubs/tkinter/web/entry-validation.html

## Getting int From Entry Widget
When using the .get() method whatever is in the entry widget will be converted into a string. For example, regardless of the type of input(It can be a number or sentence), the resulting outcome will be a string. If the user types 4 the output will be "4" as in a string. To get an int from an Entry Widget, first, call the .get() method.

    What_User_Wrote = Entry.get()

Now we convert that string into an int like so:

    Convert_To_Int = int(What_User_Wrote)

Likewise, if you want to save time you can simply do:

    Convert_To_Int = int(Entry.get())
You can use the above method if you don't want to convert str to int.

