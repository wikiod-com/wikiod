---
title: "Ttk widgets"
slug: "ttk-widgets"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Examples of the different ttk widgets. Ttk has a total of 17 widgets, eleven of which already existed in tkinter (tk).

Using ttk module gives your application a more modern and improved look.

## Syntax
- tree=ttk.Treeview(master,**kwargs)

## Parameters
| Parameter | Description |
| ------ | ------ |
| master  |  tkinter widgets exist in a hieararchy. Except for the root window, all widgets have a parent (also called "master"). When the widget is added to the screen with pack, place or grid, it will appear inside this parent widget

These examples assume that tkinter has been imported with either `import tkinter as tk` (python 3) or `import Tkinter as tk` (python 2).

It is also assumed that ttk has been imported with either `from tkinter import ttk` (python 3) or `import ttk` (python 2).

## Treeview: Basic example
This widget is used to display items with hierarchy. For instance, windows explorer can be reproduced in this way. Some nice tables can be also done using `treeview` widget.

# Create the widget
    tree=ttk.Treeview(master)

# Definition of the columns
You can define how many columns, their width and minimum width when the user tries to stretch it. By defining `stretch=tk.NO`, the user cannot modify the width of the column.

    tree["columns"]=("one","two","three")
    tree.column("#0", width=270, minwidth=270, stretch=tk.NO)
    tree.column("one", width=150, minwidth=150, stretch=tk.NO)
    tree.column("two", width=400, minwidth=200)
    tree.column("three", width=80, minwidth=50, stretch=tk.NO)

# Definition of the headings
    tree.heading("#0",text="Name",anchor=tk.W)
    tree.heading("one", text="Date modified",anchor=tk.W)
    tree.heading("two", text="Type",anchor=tk.W)
    tree.heading("three", text="Size",anchor=tk.W)

# Insert some rows
    # Level 1
    folder1=tree.insert("", 1, "", text="Folder 1", values=("23-Jun-17 11:05","File folder",""))
    tree.insert("", 2, "", text="text_file.txt", values=("23-Jun-17 11:25","TXT file","1 KB"))
    # Level 2
    tree.insert(folder1, "end", "", text="photo1.png", values=("23-Jun-17 11:28","PNG file","2.6 KB"))
    tree.insert(folder1, "end", "", text="photo2.png", values=("23-Jun-17 11:29","PNG file","3.2 KB"))
    tree.insert(folder1, "end", "", text="photo3.png", values=("23-Jun-17 11:30","PNG file","3.1 KB"))

# Packing
    tree.pack(side=tk.TOP,fill=tk.X)

On Windows, the following screenshot can be obtained from this example.

[![Screenshot of the treeview widget on Windows][1]][1]


  [1]: https://i.stack.imgur.com/2Mzp2.png

## Progressbar
The widget `ttk.progress` is useful when dealing with long computations so that the user knows that the program is running. Following, an example updating a progressbar each 0.5 seconds is given:
    
# Function updating the progressbar
    def progress(currentValue):
        progressbar["value"]=currentValue

# Set the maximum value
    maxValue=100

# Create the progress bar
    progressbar=ttk.Progressbar(master,orient="horizontal",length=300,mode="determinate")
    progressbar.pack(side=tk.TOP)

"determinate" mode is used when the progressbar is under control of the program.

# Initial and maximum values
    currentValue=0
    progressbar["value"]=currentValue
    progressbar["maximum"]=maxValue

# Emulate progress each 0.5 s
    divisions=10
    for i in range(divisions):
        currentValue=currentValue+10
        progressbar.after(500, progress(currentValue))
        progressbar.update() # Force an update of the GUI

