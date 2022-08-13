---
title: "Adapter"
slug: "adapter"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

## Adapter (Java)
<!-- language-all: lang-java -->

Lets assume that in your current codebase, there exists `MyLogger` interface like so:

    interface MyLogger {
        void logMessage(String message);
        void logException(Throwable exception);
    }

Lets say that you've created a few concrete implementations of these, such as `MyFileLogger` and `MyConsoleLogger`.

You have decided that you want to use a framework for controlling your application's Bluetooth connectivity. This framework contains a `BluetoothManager` with the following constructor:

    class BluetoothManager {
        private FrameworkLogger logger;
    
        public BluetoothManager(FrameworkLogger logger) {
            this.logger = logger;
        }
    }

The `BluetoothManager` also accepts a logger, which is great! However it expects a logger of which the interface was defined by the framework and they have used method overloading instead of naming their functions differently:

    interface FrameworkLogger {
        void log(String message);
        void log(Throwable exception);
    }

You already have a bunch of `MyLogger` implementations that you would like to reuse, but they do not fit the interface of the `FrameworkLogger`. This is where the adapter design-pattern comes in:

    class FrameworkLoggerAdapter implements FrameworkLogger {
        private MyLogger logger;
    
        public FrameworkLoggerAdapter(MyLogger logger) {
            this.logger = logger;
        }
    
        @Override
        public void log(String message) {
            this.logger.logMessage(message);
        }
    
        @Override
        public void log(Throwable exception) {
            this.logger.logException(exception);
        }
    }

By defining an adapter class that implements the `FrameworkLogger` interface and accepts a `MyLogger` implementation the functionality can be mapped between the different interfaces. Now it is possible to use the `BluetoothManager` with all of the `MyLogger` implementations like so:

    FrameworkLogger fileLogger = new FrameworkLoggerAdapter(new MyFileLogger());
    BluetoothManager manager = new BluetoothManager(fileLogger);

    FrameworkLogger consoleLogger = new FrameworkLoggerAdapter(new MyConsoleLogger());
    BluetoothManager manager2 = new BluetoothManager(consoleLogger);

## Adapter Pattern (PHP)


## Java Example
A great existing example of the Adapter pattern can be found in the SWT [MouseListener](http://help.eclipse.org/kepler/index.jsp?topic=%2Forg.eclipse.platform.doc.isv%2Freference%2Fapi%2Forg%2Feclipse%2Fswt%2Fevents%2FMouseListener.html) and [MouseAdapter](http://help.eclipse.org/kepler/index.jsp?topic=%2Forg.eclipse.platform.doc.isv%2Freference%2Fapi%2Forg%2Feclipse%2Fswt%2Fevents%2FMouseAdapter.html) classes.

The MouseListener interface looks as follows:

    public interface MouseListener extends SWTEventListener {
        public void mouseDoubleClick(MouseEvent e);
        public void mouseDown(MouseEvent e);
        public void mouseUp(MouseEvent e);
    }

Now imagine a scenario where you are building a UI and adding these listeners, but most of the time you don't care about anything other than when something is single clicked (mouseUp). You wouldn't want to constantly be creating empty implementations:

    obj.addMouseListener(new MouseListener() {

        @Override
        public void mouseDoubleClick(MouseEvent e) {
        }

        @Override
        public void mouseDown(MouseEvent e) {
        }

        @Override
        public void mouseUp(MouseEvent e) {
            // Do the things
        }

    });

Instead, we can use MouseAdapter:

    public abstract class MouseAdapter implements MouseListener {
        public void mouseDoubleClick(MouseEvent e) { }
        public void mouseDown(MouseEvent e) { }
        public void mouseUp(MouseEvent e) { }
    }

By providing empty, default implementations, we are free to override only those methods which we care about from the adapter. Following from the above example:

    obj.addMouseListener(new MouseAdapter() {

        @Override
        public void mouseUp(MouseEvent e) {
            // Do the things
        }

    });

## Adapter (UML & example situation)
To make the use of the adapter pattern and the kind of situation when it may be applied more imaginable, a small, simple and very concrete example is given here.
There will be no code in here, just UML and a description of the example situation and its problem. Admittedly, the UML content is written like Java.
(Well, the hint text said "Good examples are mostly code", I think design patterns are abstract enough to be introduced in a different way, too.)

In general, the adapter pattern is an adequate solution for a situation when you have incompatible interfaces and none of them can be directly rewritten.

Imagine you're running a nice little pizza delivery service. Customers can order online on your website and you have small system using a class `Pizza` to represent your pizzas and calculate bills, tax reports and more.
The price of your pizzas is given as a single integer representing the price in cent (of the currency of your choice).

[![enter image description here][1]][1]

Your delivery service is working out great, but at some point you cannot handle the growing number of customers on your own anymore but you still want to expand. 
You decide to add your pizzas to the menu of a big online meta delivery service.
They offer a lot of different meals — not only pizzas — so their system makes more use of abstraction and has an Interface `IMeal` representing meals coming along with a class `MoneyAmount` representing money.

[![enter image description here][2]][2]

`MoneyAmount` consists of two integers as input, one for the amount (or some random currency) before the comma, and one for the cent amount from 0 to 99 after the comma;

[![enter image description here][3]][3]

Due to the fact that the price of your `Pizza` is a single integer representing the total price as an amount of cent (> 99), it is not compatible with `IMeal`.
This is the point where the adapter pattern comes into play: In case it would take too much effort to change your own system or create a new one and you have to implement an incompatible interface, you may want to apply the adapter patttern.

There are two ways of applying the pattern: class adapter and object adapter. 

Both have in common that an adapter (`PizzaAdapter`) works as some kind of translator between the new interface and the adaptee (`Pizza` in this example).
The adapter implements the new interface (`IMeal`) and then either inherits from `Pizza` and converts its own price from one integer to two (class adapter) 

[![enter image description here][4]][4]

or has an object of type `Pizza` as an attribute and converts the values of that (object adapter).

[![enter image description here][5]][5]

By applying the adapter pattern, you will kind of "translate" between incompatible interfaces.

  [1]: https://i.stack.imgur.com/TRcPc.png
  [2]: https://i.stack.imgur.com/8EFkW.png
  [3]: https://i.stack.imgur.com/AsTNb.png
  [4]: https://i.stack.imgur.com/6jYhK.png
  [5]: https://i.stack.imgur.com/p9vbf.png

