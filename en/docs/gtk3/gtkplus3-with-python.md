---
title: "GTK+3 with Python"
slug: "gtk+3-with-python"
draft: false
images: []
weight: 9934
type: docs
toc: true
---

## A simple GTK window
Simply presenting a window is easy with GTK and Python. The example below is based off the [Python GTK3 Tutorial](https://python-gtk-3-tutorial.readthedocs.io), which you should read if you are a beginner in GUI programming or GTK. 

<!-- language: python -->
    import gi
    gi.require_version('Gtk', '3.0')
    from gi.repository import Gtk

    # Set up the Gtk window    
    win = Gtk.Window()

    # Tell Gtk what to do when the window is closed (in this case quit the main loop)
    win.connect("delete-event", Gtk.main_quit)
    
    # Create a label saying Hello World!
    label = Gtk.Label(label="Hello World!")

    # Add the label to the window
    win.add(label)

    # Tell Gtk to show all widgets inside the window
    win.show_all()
    
    # Start the Gtk main loop, which returns when Gtk.main_quit is called
    Gtk.main()

Which will (on Windows 10) result in:

[![The result of the example above][1]][1]


  [1]: http://i.stack.imgur.com/51xxE.png

## Simple binding to a widget's key-press-event
The simplest way to get an event handler called on a key press is to connect the handler to the `key-press-event` signal. In this example, we register for the event for the whole window, but you can also register for individual widgets too.

The most important part is the connection of the handler to the event:

    self.connect("key-press-event",self.on_key_press_event)

In the event handler, the widget and the key-press event are passed in as parameters. Key-press modifiers such as the <kbd>Ctrl</kbd> key are available in `event.state` and the pressed key is `event.keyval`.

Values for modifier keys are found in [`Gdk.ModiferType`][1] and include `CONTROL_MASK`, `SHIFT_MASK` and several others.

Key values are found in [`Gdk` with prefixes of `KEY_`][2], for example, the <kbd>h</kbd> key is `Gdk.KEY_h`) These can be converted to a string using [`Gdk.keyval_name()`][3].

    import gi
    gi.require_version('Gtk', '3.0')
    from gi.repository import Gtk
    from gi.repository import Gdk
    
    class MyWindow(Gtk.Window):
    
        key = Gdk.KEY_h
    
        def __init__(self):
            # init the base class (Gtk.Window)
            super().__init__()
    
            # state affected by shortcuts
            self.shortcut_hits = 0
    
            # Tell Gtk what to do when the window is closed (in this case quit the main loop)
            self.connect("delete-event", Gtk.main_quit)
    
            # connect the key-press event - this will call the keypress
            # handler when any key is pressed
            self.connect("key-press-event",self.on_key_press_event)
    
            # Window content goes in a vertical box
            box = Gtk.VBox()
    
            # mapping between Gdk.KEY_h and a string
            keyname = Gdk.keyval_name(self.key)
    
            # a helpful label
            instruct = Gtk.Label(label="Press Ctrl+%s" % keyname)
            box.add(instruct)
    
            # the label that will respond to the event
            self.label = Gtk.Label(label="")
            self.update_label_text()
    
            # Add the label to the window
            box.add(self.label)
    
            self.add(box)
    
        def on_key_press_event(self, widget, event):
    
            print("Key press on widget: ", widget)
            print("          Modifiers: ", event.state)
            print("      Key val, name: ", event.keyval, Gdk.keyval_name(event.keyval))
    
            # check the event modifiers (can also use SHIFTMASK, etc)
            ctrl = (event.state & Gdk.ModifierType.CONTROL_MASK)
    
            # see if we recognise a keypress
            if ctrl and event.keyval == Gdk.KEY_h:
                self.shortcut_hits += 1
                self.update_label_text()
    
        def update_label_text(self):
            # Update the label based on the state of the hit variable
            self.label.set_text("Shortcut pressed %d times" % self.shortcut_hits)
    
    if __name__ == "__main__":
        win = MyWindow()
        win.show_all()
    
        # Start the Gtk main loop
        Gtk.main()

More advanced behaviour for application-wide shortcuts can be achieved with an accelerator group ([`Gtk.AccelGroup`][4]), but often a quick key-press handler is all you need to capture the keyboard events you want for a specific widget.

  [1]: https://lazka.github.io/pgi-docs/#Gdk-3.0/flags.html#Gdk.ModifierType
  [2]: https://lazka.github.io/pgi-docs/#Gdk-3.0/constants.html
  [3]: https://lazka.github.io/pgi-docs/#Gdk-3.0/functions.html#Gdk.keyval_name
  [4]: https://lazka.github.io/pgi-docs/#Gtk-3.0/classes/AccelGroup.html#Gtk.AccelGroup

## Embed a Video in a Gtk window in Python3
Below is an example of Gstreamer pipeline embedded in a simple gtk window. When run, a small window should appear like this:

[![The example when run][1]][1]

    import gi
    gi.require_version('Gtk', '3.0')
    gi.require_version('Gst', '1.0')

    from gi.repository import Gtk, Gst
    Gst.init(None)
    Gst.init_check(None)


    class GstWidget(Gtk.Box):
        def __init__(self, pipeline):
            super().__init__()
            # Only setup the widget after the window is shown.
            self.connect('realize', self._on_realize)

            # Parse a gstreamer pipeline and create it.
            self._bin = Gst.parse_bin_from_description(pipeline, True)

        def _on_realize(self, widget):
            pipeline = Gst.Pipeline()
            factory = pipeline.get_factory()
            gtksink = factory.make('gtksink')
            pipeline.add(self._bin)
            pipeline.add(gtksink)
            # Link the pipeline to the sink that will display the video.
            self._bin.link(gtksink)
            self.pack_start(gtksink.props.widget, True, True, 0)
            gtksink.props.widget.show()
            # Start the video
            pipeline.set_state(Gst.State.PLAYING)


    window = Gtk.ApplicationWindow()

    # Create a gstreamer pipeline with no sink. 
    # A sink will be created inside the GstWidget.
    widget = GstWidget('videotestsrc')
    widget.set_size_request(200, 200)

    window.add(widget)

    window.show_all()


    def on_destroy(win):
        Gtk.main_quit()

    window.connect('destroy', on_destroy)

    Gtk.main()


  [1]: https://i.stack.imgur.com/cnZHV.jpg

