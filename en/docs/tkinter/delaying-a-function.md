---
title: "Delaying a function"
slug: "delaying-a-function"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Syntax

- widget.after(delay_ms, callback, *args)


## Parameters

| Parameter | Description |
| ------ | ------ |
| delay_ms  | Time (milliseconds) which is delayed the call to the function `callback`  |
| callback | Function that is called after the given `delay_ms`. If this parameter is not given, `.after` acts similar to `time.sleep` (in milliseconds)

Syntax assumes a `widget` accepted by the method `.after` has been previously created (i.e `widget=tk.Label(parent)`)

## .after()
`.after(delay, callback=None)` is a method defined for all tkinter widgets. This method simply calls the function `callback` after the given `delay` in ms. If no function is given, it acts similar to [`time.sleep`][1] (but in milliseconds instead of seconds)
 
Here is an example of how to create a simple timer using `after`:

    # import tkinter
    try:
        import tkinter as tk
    except ImportError:
        import Tkinter as tk

    class Timer:
        def __init__(self, parent):
            # variable storing time
            self.seconds = 0
            # label displaying time
            self.label = tk.Label(parent, text="0 s", font="Arial 30", width=10)
            self.label.pack()
            # start the timer
            self.label.after(1000, self.refresh_label)

        def refresh_label(self):
            """ refresh the content of the label every second """
            # increment the time
            self.seconds += 1
            # display the new time
            self.label.configure(text="%i s" % self.seconds)
            # request tkinter to call self.refresh after 1s (the delay is given in ms)
            self.label.after(1000, self.refresh_label)

    if __name__ == "__main__":
        root = tk.Tk()
        timer = Timer(root)
        root.mainloop()


  [1]: https://docs.python.org/3/library/time.html#time.sleep

