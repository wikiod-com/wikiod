---
title: "Getting started with gtk3"
slug: "getting-started-with-gtk3"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
**Python**
------

**Windows**

The easiest way to install GTK3 for Python is by using [PyGObject for Windows][1]. It offers an installer that installs most things you need to develop GTK appilcations. 

The number of options the PyGObject installer offers can be daunting, but for most GTK projects the only option you have to select is `GTK+ 3.xx`.

**C++**
-------
The C++ binding for Gtk+ is known as **gtkmm**.

**Windows**

On Microsoft Windows gtkmm can be installed through [MSYS2](https://msys2.github.io/) environment. Once MSYS2 environment is set up by installing the installer and updating the package list, install gtkmm with

<!-- language: sh -->
    pacman -S mingw-w64-x86_64-gtkmm3 #64 bit
    pacman -S mingw-w64-i686-gtkmm3   #32 bit

Install _pkg-config_ for easily obtaining compiler and linker flags and GNU autotools build integration

<!-- language: sh -->
    pacman -S pkg-config

Now gtkmm application can be compiled, linked and run from within MSYS2 environment.

<!-- language: sh -->
    # enable C++ 14 support if needed
    # -mwindows flag is to suppress the background command-prompt window 
    # for GUI applications
    g++ -mwindows -std=c++14 -o app.exe app.cpp `pkg-config --cflags --libs gtkmm-3.0`
    ./app.exe

But the executable won't run outside the MSYS2 shell because of missing standard environment variables for .dll lookup. The following .dlls need to be copied from `<MSYS2 INSTALLATION DIRECTORY>\mingw64\lib\`(for 64-bit installation) into the *application directory* (where the `.exe` is located) manually. The version numbers may change according to the installation.

```
libatk-1.0-0.dll
libatkmm-1.6-1.dll
libbz2-1.dll
libcairo-2.dll
libcairo-gobject-2.dll
libcairomm-1.0-1.dll
libepoxy-0.dll
libexpat-1.dll
libffi-6.dll
libfontconfig-1.dll
libfreetype-6.dll
libgcc_s_seh-1.dll
libgdk_pixbuf-2.0-0.dll
libgdk-3-0.dll
libgdkmm-3.0-1.dll
libgio-2.0-0.dll
libgiomm-2.4-1.dll
libglib-2.0-0.dll
libglibmm-2.4-1.dll
libgmodule-2.0-0.dll
libgobject-2.0-0.dll
libgtk-3-0.dll
libgtkmm-3.0-1.dll
libharfbuzz-0.dll
libiconv-2.dll
libintl-8.dll
libpango-1.0-0.dll
libpangocairo-1.0-0.dll
libpangoft2-1.0-0.dll
libpangomm-1.4-1.dll
libpangowin32-1.0-0.dll
libpixman-1-0.dll
libpng16-16.dll
libsigc-2.0-0.dll
libstdc++-6.dll
libwinpthread-1.dll
zlib1.dll
```
After this step the program should run. But it won't find standard icon sets for Gtk+, i.e. the _Adwaita icon theme_, so icons may not load. The icons and a few other files need to be copied into application directory so that the application can load them.

From `<MSYS2 INSTALL DIRECTORY>`
```
mingw64
 |
 +-- lib
      |
      +-- gdk-pixbuf-2.0
share
 |
 +-- icons
       |
       +-- Adwaita
       |
       +-- hicolor (fallback icon theme for Gtk+)
```
To application directory, with same directory structure.

  [1]: https://sourceforge.net/projects/pygobjectwin32/

## [C++] "Hello World" in gtkmm
<!-- language: c++ -->
    #include <gtkmm/application.h>
    #include <gtkmm/applicationwindow.h>
    #include <gtkmm/button.h>
    
    // main window of the application
    class HelloWorldWindow : public Gtk::ApplicationWindow {
        // a simple push button
        Gtk::Button btn;
    public:
        HelloWorldWindow()
        : btn("Click me!") {// initialize button with a text label
            // when user presses the button "clicked" signal is emitted
            // connect an event handler for the signal with connect()
            // which accepts lambda expression, among other things
            btn.signal_clicked().connect(
            [this]() {
                btn.set_label("Hello World");
            });
            // add the push button to the window
            add(btn);
            // make the window visible
            show_all();
        }
    };
    
    int main(int argc, char *argv[]) {
         // This creates an Gtk+ application with an unique application ID
         auto app = Gtk::Application::create(argc, argv, "org.gtkmm.example.HelloApp");
         HelloWorldWindow hw;
         // this starts the application with our window
         // close the window to terminate the application
         return app->run(hw);
    }

## [C] "Hello World" in Gtk+
<!-- language: c -->
    
    #include <gtk/gtk.h>

    // callback function which is called when button is clicked
    static void on_button_clicked(GtkButton *btn, gpointer data) {
        // change button label when it's clicked
        gtk_button_set_label(btn, "Hello World");
    }

    // callback function which is called when application is first started
    static void on_app_activate(GApplication *app, gpointer data) {
        // create a new application window for the application
        // GtkApplication is sub-class of GApplication
        // downcast GApplication* to GtkApplication* with GTK_APPLICATION() macro
        GtkWidget *window = gtk_application_window_new(GTK_APPLICATION(app));
        // a simple push button
        GtkWidget *btn = gtk_button_new_with_label("Click Me!");
        // connect the event-handler for "clicked" signal of button
        g_signal_connect(btn, "clicked", G_CALLBACK(on_button_clicked), NULL);
        // add the button to the window
        gtk_container_add(GTK_CONTAINER(window), btn);
        // display the window
        gtk_widget_show_all(GTK_WIDGET(window));
    }

    int main(int argc, char *argv[]) {
        // create new GtkApplication with an unique application ID
        GtkApplication *app = gtk_application_new(
            "org.gtkmm.example.HelloApp", 
            G_APPLICATION_FLAGS_NONE
        );
        // connect the event-handler for "activate" signal of GApplication
        // G_CALLBACK() macro is used to cast the callback function pointer
        // to generic void pointer
        g_signal_connect(app, "activate", G_CALLBACK(on_app_activate), NULL);
        // start the application, terminate by closing the window
        // GtkApplication* is upcast to GApplication* with G_APPLICATION() macro
        int status = g_application_run(G_APPLICATION(app), argc, argv);
        // deallocate the application object
        g_object_unref(app);
        return status;
    }

## starter kit
    #include <gtk/gtk.h>
    static void destroy(GtkWidget *widget, gpointer data)
    {
    gtk_main_quit();
    }
    int main(int argc, char *argv[])
    {
    gtk_init(&argc, &argv);
    GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(window), "Window");
    g_signal_connect(window, "destroy", G_CALLBACK(destroy), NULL);
    
    GtkWidget *k;
    k= gtk_fixed_new();
        gtk_container_add(GTK_CONTAINER(window), k);
    
           GtkWidget* la,*r;
        la = gtk_button_new_with_label (",mkl");
         gtk_fixed_put (GTK_FIXED (k), la,50,237);
        gtk_widget_set_size_request(la, 98, 90);
    //    gtk_container_set_border_width(GTK_CONTAINER (la)    , 5);
    
    
        r = gtk_button_new_with_label (",kii");
         gtk_fixed_put (GTK_FIXED (k), r,150,237);
        gtk_widget_set_size_request(r, 98, 90);
    
        gtk_widget_set_size_request(GTK_WIDGET(window),300,349);
    gtk_widget_show_all(GTK_WIDGET(window));
    
    gtk_main();
    return 0;
    }

compile:

    c++ starterkit.c `pkg-config --libs --cflags gtk+-3.0` -o p
and 

    ./p



