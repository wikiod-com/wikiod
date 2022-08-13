---
title: "Scrolling widgets"
slug: "scrolling-widgets"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Scrollbars can be added to Listbox, Canvas, and Text widgets. In addition, Entry widgets can be scrolled horizontally. To be able to scroll other type of widgets, you need to put them inside a Canvas or a Text widget.

## Syntax
 - scrollbar = tk.Scrollbar(parent, **kwargs)

## Parameters
| Parameter | Description |
| ------ | ------ |
| parent   | tkinter widgets exist in a hierarchy. Except for the root window, all widgets have a parent. Some online tutorials call this "master". When the widget is added to the screen with pack, place or grid, it will appear inside this parent widget  |
| orient | Orientation of the scrollbar, either `"vertical"` (default value) or `"horizontal"`

These examples assume that tkinter has been imported with either `import tkinter as tk` (python 3) or `import Tkinter as tk` (python 2).

## Connecting a vertical scrollbar to a Text widget
The connection between the widget and the scrollbar goes both ways. The scrollbar needs to be expanded vertically so that it has the same height as the widget.

    text = tk.Text(parent)
    text.pack(side="left")

    scroll_y = tk.Scrollbar(parent, orient="vertical", command=text.yview)
    scroll_y.pack(side="left", expand=True, fill="y")

    text.configure(yscrollcommand=scroll_y.set)




## Scrolling a Canvas widget horizontally and vertically
The principle is essentially the same as for the Text widget, but a `Grid` layout is used to put the scrollbars around the widget.

    canvas = tk.Canvas(parent, width=150, height=150)
    canvas.create_oval(10, 10, 20, 20, fill="red")
    canvas.create_oval(200, 200, 220, 220, fill="blue")
    canvas.grid(row=0, column=0)

    scroll_x = tk.Scrollbar(parent, orient="horizontal", command=canvas.xview)
    scroll_x.grid(row=1, column=0, sticky="ew")

    scroll_y = tk.Scrollbar(parent, orient="vertical", command=canvas.yview)
    scroll_y.grid(row=0, column=1, sticky="ns")

    canvas.configure(yscrollcommand=scroll_y.set, xscrollcommand=scroll_x.set)

 Unlike for the Text widget, the scrollable region of the Canvas is not updated automatically when its content is modified, so we need to define it and update it manually using the `scrollregion` argument:

    canvas.configure(scrollregion=canvas.bbox("all"))

`canvas.bbox("all")` returns the coordinates of the rectangle fitting the whole canvas content.


## Scrolling a group of widgets
When a window contains many widgets, they might not all be visible. However, neither a window (Tk or Toplevel instance) nor a Frame are scrollable. One solution to make the window content scrollable is to put all the widgets in a Frame, and then, embed this Frame in a Canvas using the `create_window` method.

    canvas = tk.Canvas(parent)
    scroll_y = tk.Scrollbar(parent, orient="vertical", command=canvas.yview)

    frame = tk.Frame(canvas)
    # group of widgets
    for i in range(20):
        tk.Label(frame, text='label %i' % i).pack()
    # put the frame in the canvas
    canvas.create_window(0, 0, anchor='nw', window=frame)
    # make sure everything is displayed before configuring the scrollregion
    canvas.update_idletasks()

    canvas.configure(scrollregion=canvas.bbox('all'), 
                     yscrollcommand=scroll_y.set)
                     
    canvas.pack(fill='both', expand=True, side='left')
    scroll_y.pack(fill='y', side='right')


