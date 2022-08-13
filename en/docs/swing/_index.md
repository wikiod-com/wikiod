---
title : swing Tutorial
slug : swing-tutorial
weight : 9955
draft : false
images : []
type : docs
---

Swing has been [superseded by JavaFX][1]. Oracle generally recommends developing new applications with JavaFX. Still: Swing will be supported in Java for the foreseeable future. JavaFX also integrates well with Swing, to allow transitioning applications smoothly.

It is strongly recommended to have most of your Swing components on the Event Dispatch Thread. It's easy to forget to bundle your GUI setup into a `invokeLater` call. From the Java Documentation:
>Swing event handling code runs on a special thread known as the event dispatch thread. Most code that invokes Swing methods also runs on this thread. This is necessary because most Swing object methods are not "thread safe": invoking them from multiple threads risks thread interference or memory consistency errors. Some Swing component methods are labelled "thread safe" in the API specification; these can be safely invoked from any thread. All other Swing component methods must be invoked from the event dispatch thread. Programs that ignore this rule may function correctly most of the time, but are subject to unpredictable errors that are difficult to reproduce.

Also, unless for good reason, *always* make sure that you called `setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)` or else you might possibly have to deal with a memory leak if you forget to destroy the JVM.



  [1]: http://www.oracle.com/technetwork/java/javafx/overview/faq-1446554.html#6

