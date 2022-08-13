---
title: "Tkinter Geometry Managers"
slug: "tkinter-geometry-managers"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

There are three geometry managers to position widgets: `pack()`, `grid()` and `place()`.

## pack()
The `pack()` geometry manager organizes widgets in blocks before placing them in the parent widget. It uses the options `fill`, `expand` and `side`.

**Syntax**

`widget.pack(option)`

**Fill**<br>
Determines if the widget keeps the minimal space needed or takes up any extra space allocated to it. Attributes: NONE (default), X (fill horizontally), Y (fill vertically), or BOTH (fill both horizontally and vertically).

**Expand**<br>
When set to YES, the widget expands to fill any space not used in widget's parent. Attributes: YES, NO.

**Side**<br>
Determines which side of the widget's parent it packs to. Attributes: TOP (default), BOTTOM, LEFT, or RIGHT.

**Example**<br>

    from tkinter import *
    root = Tk()
    btn_fill = Button(root, text="Button")
    btn_fill.pack(fill=X)
    
    btn_expand = Button(root, text="Button")
    btn_expand.pack(expand=YES)
    
    btn_side = Button(root, text="Button")
    btn_side.pack(side=RIGHT)
    
    root.mainloop()
**Result**<br>

[![Pack Examples][1]][1]


  [1]: https://i.stack.imgur.com/AOcN1.jpg

## grid()
The `grid()` geometry manager organises widgets in a table-like structure in the parent widget. The master widget is split into rows and columns, and each part of the table can hold a widget. It uses `column`, `columnspan`, `ipadx`, `ipady`, `padx`, `pady`, `row`, `rowspan` and `sticky`.

**Syntax**<br>

`widget.grid(options)`

**Column**<br>
The column to put widget in. The default column is 0, which is the leftmost column.

**Columnspan**<br>
How many columns widget takes up. The default is 1.

**Ipadx**<br>
How many pixels to pad widget horizontally inside the widget's borders.

**Ipady**<br>
How many pixels to pad widget vertically inside the widget's borders.

**Padx**<br>
How many pixels to pad widget horizontally outside the widget's borders.

**Pady**<br>
How many pixels to pad widget vertically outside the widget's borders.

**Row**<br>
The row to put widget in. The default row is 0, which is the topmost column.

**Rowspan**<br>
How many rows the widget takes up. The default is 1.

**Sticky**<br>
When the widget is smaller than the cell, `sticky` is used to indicate which sides and corners of the cell the widget sticks to. The direction is defined by compass directions: N, E, S, W, NE, NW, SE, and SW and zero. These could be a string concatenation, for example, NESW make the widget take up the full area of the cell.

**Example**

    from tkinter import *
    root = Tk()
    btn_column = Button(root, text="I'm in column 3")
    btn_column.grid(column=3)

    btn_columnspan = Button(root, text="I have a columnspan of 3")
    btn_columnspan.grid(columnspan=3)

    btn_ipadx = Button(root, text="ipadx of 4")
    btn_ipadx.grid(ipadx=4)
    
    btn_ipady = Button(root, text="ipady of 4")
    btn_ipady.grid(ipady=4)

    btn_padx = Button(root, text="padx of 4")
    btn_padx.grid(padx=4)

    btn_pady = Button(root, text="pady of 4")
    btn_pady.grid(pady=4)

    btn_row = Button(root, text="I'm in row 2")
    btn_row.grid(row=2)

    btn_rowspan = Button(root, text="Rowspan of 2")
    btn_rowspan.grid(rowspan=2)

    btn_sticky = Button(root, text="I'm stuck to north-east")
    btn_sticky.grid(sticky=NE)

    root.mainloop()
**Result**

[![Grid example][1]][1]


  [1]: https://i.stack.imgur.com/XTc9h.jpg

## place()
The `place()` manager organises widgets by placing them in a specific position in the parent widget. This geometry manager uses the options `anchor`, `bordermode`, `height`, `width`, `relheight`, `relwidth`,`relx`, `rely`, `x` and `y`.

**Anchor**<br>
Indicates where the widget is anchored to. The options are compass directions: N, E, S, W, NE, NW, SE, or SW, which relate to the sides and corners of the parent widget. The default is NW (the upper left corner of widget)

**Bordermode**<br>
Bordermode has two options: `INSIDE`, which indicates that other options refer to the parent's inside, (Ignoring the parent's borders) and  `OUTSIDE`, which is the opposite.

**Height**<br>
Specify the height of a widget in pixels.

**Width**<br>
Specify the width of a widget in pixels.

**Relheight**<br>
Height as a float between 0.0 and 1.0, as a fraction of the height of the parent widget.

**Relwidth**<br>
Width as a float between 0.0 and 1.0, as a fraction of the width of the parent widget.

**Relx**<br>
Horizontal offset as a float between 0.0 and 1.0, as a fraction of the width of the parent widget.

**Rely**<br>
Vertical offset as a float between 0.0 and 1.0, as a fraction of the height of the parent widget.

**X**<br>
 Horizontal offset in pixels.

**Y**<br>
Vertical offset in pixels.

**Example**

    from tkinter import *
    root = Tk()
    root.geometry("500x500")
    
    btn_height = Button(root, text="50px high")
    btn_height.place(height=50, x=200, y=200)
    
    btn_width = Button(root, text="60px wide")
    btn_width.place(width=60, x=300, y=300)
    
    btn_relheight = Button(root, text="Relheight of 0.6")
    btn_relheight.place(relheight=0.6)
    
    btn_relwidth= Button(root, text="Relwidth of 0.2")
    btn_relwidth.place(relwidth=0.2)
    
    btn_relx=Button(root, text="Relx of 0.3")
    btn_relx.place(relx=0.3)
    
    btn_rely=Button(root, text="Rely of 0.7")
    btn_rely.place(rely=0.7)
    
    btn_x=Button(root, text="X = 400px")
    btn_x.place(x=400)
    
    btn_y=Button(root, text="Y = 321")
    btn_y.place(y=321)

    root.mainloop()

**Result**

[![Place example][1]][1]


  [1]: https://i.stack.imgur.com/HBBkv.jpg

