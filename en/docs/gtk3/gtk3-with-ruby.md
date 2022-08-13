---
title: "Gtk3 with Ruby"
slug: "gtk3-with-ruby"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Get up and running
    # Like any other ruby code, require gtk3 after installing from "gem install gtk3"
    require 'gtk3'
    
    # Like in Rails, you import working functions from a higher class, in this case the GTK Window
    class RubyApp < Gtk::Window
    
    # Calling the original method from GTK Window and redefining the defaults
        def initialize
            super
            
            # Printing window title
            set_title "Center"

            # Invoking built-in GTK connection and calling destroy to replicate the quit action
            signal_connect "destroy" do 
                Gtk.main_quit 
            end
            
            # Sets the window size 500px wide by 400px tall
            set_default_size 500, 400
            
            # Where the window should be displayed on the screen
            set_window_position Gtk::Window::Position::CENTER
            
            # After initialization, show everything
            show
        end
    end
    
    # Call the class, just like any other ruby program
    window = RubyApp.new

    # GTK method. Runs until destroy is called
    Gtk.main

Save this file as `new.rb` and run from terminal `ruby new.rb`

