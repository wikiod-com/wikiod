---
title: "Multiple windows (TopLevel widgets)"
slug: "multiple-windows-toplevel-widgets"
draft: false
images: []
weight: 9940
type: docs
toc: true
---

## Difference between Tk and Toplevel
`Tk` is the absolute root of the application, it is the first widget that needs to be instantiated and the GUI will shut down when it is destroyed.

`Toplevel` is a window in the application, closing the window will destroy all children widgets placed on that window{1} but will not shut down the program.  

    try:
        import tkinter as tk #python3
    except ImportError:
        import Tkinter as tk #python2
    
    #root application, can only have one of these.
    root = tk.Tk() 
    
    #put a label in the root to identify the window.
    label1 = tk.Label(root, text="""this is root
    closing this window will shut down app""")
    label1.pack()
    
    #you can make as many Toplevels as you like
    extra_window = tk.Toplevel(root)
    label2 = tk.Label(extra_window, text="""this is extra_window
    closing this will not affect root""")
    label2.pack()

    root.mainloop()

If your python program only represents a single application (which it almost always will) then you should have only one `Tk` instance, but you may create as many `Toplevel` windows as you like.

    try:
        import tkinter as tk #python3
    except ImportError:
        import Tkinter as tk #python2
    
    def generate_new_window():
        window = tk.Toplevel()
        label = tk.Label(window, text="a generic Toplevel window")
        label.pack()
    
    root = tk.Tk()
    
    spawn_window_button = tk.Button(root,
                                    text="make a new window!",
                                    command=generate_new_window)
    spawn_window_button.pack()
    
    root.mainloop()



                            


----------


{1}: if a Toplevel (`A = Toplevel(root)`) is the parent of another Toplevel (`B = Toplevel(A)`) then closing window A will also close window B.


## arranging the window stack (the .lift method)
The most basic case to lift a particular window above the others, just call the `.lift()` method on that window (either `Toplevel` or `Tk`)

    import tkinter as tk #import Tkinter as tk #change to commented for python2

    root = tk.Tk()

    for i in range(4):
        #make a window with a label
        window = tk.Toplevel(root)
        label = tk.Label(window,text="window {}".format(i))
        label.pack()
        #add a button to root to lift that window
        button = tk.Button(root, text = "lift window {}".format(i), command=window.lift)
        button.grid(row=i)

    root.mainloop()


However if that window is destroyed trying to lift it will raise an error like this:

    Exception in Tkinter callback
    Traceback (most recent call last):
      File "/.../tkinter/__init__.py", line 1549, in __call__
        return self.func(*args)
      File "/.../tkinter/__init__.py", line 785, in tkraise
        self.tk.call('raise', self._w, aboveThis)
    _tkinter.TclError: bad window path name ".4385637096"

Often when we are trying to put a particular window in front of the user but it was closed a good alternative is to recreate that window:

    import tkinter as tk #import Tkinter as tk #change to commented for python2

    dialog_window = None

    def create_dialog():
        """creates the dialog window
      ** do not call if dialog_window is already open, this will
         create a duplicate without handling the other
    if you are unsure if it already exists or not use show_dialog()"""
        global dialog_window
        dialog_window = tk.Toplevel(root)
        label1 = tk.Label(dialog_window,text="this is the dialog window")
        label1.pack()
        #put other widgets
        dialog_window.lift() #ensure it appears above all others, probably will do this anyway

    def show_dialog():
        """lifts the dialog_window if it exists or creates a new one otherwise"""
        #this can be refactored to only have one call to create_dialog()
        #but sometimes extra code will be wanted the first time it is created
        if dialog_window is None:
            create_dialog()
            return
        try:
            dialog_window.lift()
        except tk.TclError:
            #window was closed, create a new one.
            create_dialog()
        
        
    root = tk.Tk()

    dialog_button = tk.Button(root,
                              text="show dialog_window",
                              command=show_dialog)
    dialog_button.pack()
    root.mainloop()

This way the function `show_dialog` will show the dialog window whether it exists or not, also note that you can call `.winfo_exists()` to check if it exists before trying to lift the window instead of wrapping it in a `try:except`.

There is also the `.lower()` method that works the same way as the `.lift()` method, except lowering the window in the stack:

    import tkinter as tk #import Tkinter as tk #change to commented for python2
    
    root = tk.Tk()
    root.title("ROOT")
    extra = tk.Toplevel()
    label = tk.Label(extra, text="extra window")
    label.pack()
    
    lower_button = tk.Button(root,
                             text="lower this window",
                             command=root.lower)
    lower_button.pack()
    
    root.mainloop()

You will notice that it lowers even below other applications, to only lower below a certain window you can pass it to the `.lower()` method, similarly this can also be done with the `.lift()` method to only raise a window above another one.

