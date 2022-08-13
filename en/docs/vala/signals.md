---
title: "Signals"
slug: "signals"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Basic signal
Signals are only available to GObject classes. They can only be public, which means that any part of the code can connect handlers and trigger them.

<!-- language: lang-vala -->
    public class Emitter : Object {
        // A signal is declared like a method,
        // but with the signal keyword.
        public signal void my_signal ();

        public void send_signal () {
            this.my_signal (); // Send a signal by calling it like a method.
        }
    }

    void main () {
        var emitter = new Emitter ();
        // Use the connect method of the signal to add an handler.
        emitter.my_signal.connect (() => {
            print ("Received the signal.\n");
        });
        emitter.send_signal ();
        emitter.my_signal (); // You can send a signal from anywhere.
    }

You can also use normal functions as handlers if they have the same signature as the signal.

<!-- language: lang-vala -->
    void main () {
        var emitter = new Emitter ();
        emitter.connect (my_handler);
        emitter.my_signal ();
    }

    void my_handler () {
        print ("Received the signal.\n");
    }

## Detailed signal
You can write detailed signals with the `[Signal (detailed = true)]` attribute.

<!-- language: lang-vala -->
    public class Emitter : Object {
        [Signal (detailed = true)]
        public signal void detailed_signal ();

        public void emit_with_detail (string detail) {
            this.detailed_signal[detail] ();
        }
    }

    void main () {
        var emitter = new Emitter ();

        // Connect only when the detail is "foo".
        emitter.detailed_signal["foo"].connect (() => {
           print ("Received the signal with 'foo'.\n");
        });

        // Connect to the signal, whatever is the detail.
        emitter.detailed_signal.connect (() => {
            print ("Received the signal.\n");
        });

        emitter.emit_with_detail ("foo"); // Both handlers will be triggered.
        emitter.emit_with_detail ("bar"); // Only the general handler will be triggered.
    }

This feature is often used with the `notify` signal, that any `Object` based class has, and which is sent when a property changes. The detail here is the name of the property, so you can choose to connect to this signal only for some of them.

<!-- language: lang-vala -->
    public class Person : Object {
        public string name { get; set; }
        public int age { get; set; }
    }

    void main () {
        var john = new Person () { name = "John", age = 42 });
        john.notify["age"].connect (() => {
            print ("Happy birthday!");
        });
        john.age++;
    }

## Default handler and connect_after
Signals can have a default handler. All you need to do is to give it a body when you declare it.

<!-- language: lang-vala -->
    public class Emitter : Object {
        public signal void my_signal () {
            print ("Hello from the default handler!\n");
        }
    }

This handler will always be called after the `connect`ed ones. But you can use `connect_after` instead of `connect` if you want to add an handler after the default one.

<!-- language: lang-vala -->
    var emitter = new Emitter ();
    emitter.my_signal.connect_after (() => {
        print ("After the default handler!\n");
    });
    emitter.my_signal ();

