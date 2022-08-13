---
title: "GTK+ 3 with Vala"
slug: "gtk+-3-with-vala"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Hello world
Could be even more basic, but this showcases some of the features the Vala language.

# The code

    using Gtk;

    int main (string[] args) {
        Gtk.init (ref args);

        var window = new Window ();
        window.title = "First GTK+ Program";
        window.border_width = 10;
        window.window_position = WindowPosition.CENTER;
        window.set_default_size (350, 70);
        window.destroy.connect (Gtk.main_quit);

        var button = new Button.with_label ("Click me!");
        button.clicked.connect (() => {
            button.label = "Thank you";
        });

        window.add (button);
        window.show_all ();

        Gtk.main ();
        return 0;
    }

All GTK+ classes are inside the `Gtk` namespace. You must initialize every GTK+ program with `Gtk.init ()`.

# Compilation and Running on Linux

    $ valac --pkg gtk+-3.0 gtk-hello.vala
    $ ./gtk-hello

This needs the `valac` compiler, `gcc`, the `glib` and `gtk3` development packages installed on your system.

Taken from the [GNOME Wiki][1].


  [1]: https://wiki.gnome.org/Projects/Vala/GTKSample

