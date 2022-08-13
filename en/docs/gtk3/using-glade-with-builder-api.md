---
title: "Using Glade with Builder API"
slug: "using-glade-with-builder-api"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## [C++] using Gtk::Builder in gtkmm
# Overview #

Gtk+ supports a workflow where the task of user interface design and the task of programming are decoupled. Although the user interface elements such as buttons, menus, layout etc. can be directly added from code, this approach not only clutters the code, but also makes changing the UI for anyone but the programmer hard. Besides, some interface elements are used only to hold the layout structure and not need to participate in logic, adding them from code only makes it longer. Instead Glade can be used to generate UI description as XML and Gtk+ Builder API can be used to load the UI and operate on it.

# Workflow #

 1. Design UI elements in [Glade](https://glade.gnome.org/) using drag and drop. Glade generates XML file which contains UI descriptions. This can also be done manually by writing proper XML syntax and saving it with a `.glade` extension.
 2. Load UI from the file directly with 
    <!-- language: c++ -->
        auto ui = Gtk::Builder::create_from_file("ui.glade");
 3. Access individual UI elements
    <!-- language: c++ -->
        // when element doesn't need to be added to another UI element
        auto ui_elem = Glib::RefPtr<Gtk::Button>::cast_dynamic(
            ui->get_object("button_UI_id")
        );
        // when element needs to be added to another widget
        Gtk::Button *btn = nullptr;
        ui->get_widget<Gtk::Button>("button_UI_id", btn);

# Example #

[![Glade UI][1]][1]

[![Example UI: simple.glade][2]][2]

<strong>simple.glade</strong>
<!-- language: xml -->
    <?xml version="1.0" encoding="UTF-8"?>
    <!-- Generated with glade 3.20.0 -->
    <interface>
      <requires lib="gtk+" version="3.20"/>
      <object class="GtkBox" id="cont">
        <property name="width_request">200</property>
        <property name="height_request">200</property>
        <property name="visible">True</property>
        <property name="can_focus">False</property>
        <property name="orientation">vertical</property>
        <child>
          <object class="GtkLabel" id="display_label">
            <property name="visible">True</property>
            <property name="can_focus">False</property>
            <property name="wrap">True</property>
            <attributes>
              <attribute name="weight" value="bold"/>
              <attribute name="scale" value="5"/>
              <attribute name="foreground" value="#a4a400000000"/>
            </attributes>
          </object>
          <packing>
            <property name="expand">True</property>
            <property name="fill">True</property>
            <property name="position">0</property>
          </packing>
        </child>
        <child>
          <object class="GtkButton" id="display_button">
            <property name="label" translatable="yes">Display Message</property>
            <property name="visible">True</property>
            <property name="can_focus">True</property>
            <property name="receives_default">True</property>
          </object>
          <packing>
            <property name="expand">False</property>
            <property name="fill">True</property>
            <property name="position">1</property>
          </packing>
        </child>
      </object>
    </interface>
<strong>simple.cpp</strong>
<!-- language: c++ -->
    #include <gtkmm/application.h>
    #include <gtkmm/applicationwindow.h>
    #include <gtkmm/button.h>
    #include <gtkmm/label.h>
    #include <gtkmm/box.h>
    #include <gtkmm/builder.h>

    class HelloWindow : public Gtk::ApplicationWindow {
        Gtk::Box *cont;
        Glib::RefPtr<Gtk::Label> display_label;
        Glib::RefPtr<Gtk::Button> display_btn;
        Glib::RefPtr<Gtk::Builder> ui;
    public:
        HelloWindow()
        : ui{Gtk::Builder::create_from_file("simple.glade")} {
            if(ui) {
                ui->get_widget<Gtk::Box>("cont", cont);
                display_label = Glib::RefPtr<Gtk::Label>::cast_dynamic(
                    ui->get_object("display_label")
                );
                display_btn = Glib::RefPtr<Gtk::Button>::cast_dynamic(
                    ui->get_object("display_button")
                );
                if(cont && display_label && display_btn) {
                    display_btn->signal_clicked().connect(
                    [this]() {
                        display_label->set_text("Hello World");
                    });
                    add(*cont);
                }
            }
            set_title("Simple Gtk::Builder demo");
            set_default_size(400, 400);
            show_all();
        }
    };

    int main(int argc, char *argv[]) {
        auto app = Gtk::Application::create(
            argc, argv, 
            "org.gtkmm.example.HelloApp"
        );
        HelloWindow hw;
        return app->run(hw);
    }
<strong>Output</strong>

[![output][3]][3]

# Using Gio::Resource #

Loading UIs directly from `.glade` files is quick and easy. But when the application is packaged, UI descriptions, icons and other images can be put together in _resource bundles_. First a resource description needs to be created as XML file.

<strong>resources.xml</strong>
<!-- language: xml -->
    <gresources>
        <gresource prefix="/unique/prefix/">
            <file>icon.png</file>
            <!-- text files such as XML can be compressed to save memory -->
            <file compressed="true">ui.glade</file>
        </gresource>
    </gresources>
Then either create a separate `.gresource` file or a `.c` file containing resources as string data to be linked as a part of the application.
<!-- language: sh -->
    # generates separate resource file
    glib-compile-resources --target=ui.gresource resources.xml
    # generates .c file
    glib-compile-resources --generate-source resources.xml
Then from application code load the resource bundle
<!-- language: c++ -->
    // from separate file
    auto resource_bundle = Gio::Resource::create_from_file("ui.gresource");
    // from stream of bytes in .c file
    auto resource_bundle = Glib:wrap(draw_resource_get_resource());
    resource_bundle.register_global();
From resource bundle load UI elements
<!-- language: c++ -->
    auto ui = Gtk::Builder::create_from_resource("/unique/prefix/ui.glade");


  [1]: http://i.stack.imgur.com/5rKKh.png
  [2]: http://i.stack.imgur.com/9iDwG.png
  [3]: http://i.stack.imgur.com/VZpna.png

