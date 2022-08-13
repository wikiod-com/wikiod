---
title: "Threading and Concurrency"
slug: "threading-and-concurrency"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

A few notes that are already mentioned in the official docs [here](http://doc.qt.io/qt-5/qobject.html#thread-affinity) and [here](http://doc.qt.io/qt-5/qobject.html#moveToThread):

 - If an object has a parent, it has to be in the same thread as the
   parent, i.e. it cannot be moved to a new thread, nor can you set a parent to an object if the parent and the object live in different threads
 - When an object is moved to a new thread, all of its children are also moved to the new thread
 - You can only *push* objects to a new thread. You cannot *pull* them to a new thread, i.e. you can only call `moveToThread` from the thread where the object is currently living in

## Basic usage of QThread
`QThread` is a handle to a platform thread. It lets you manage the thread by monitoring its lifetime, and requesting that it finishes its work.

In most cases inhering from the class is not recommended. The default `run` method starts an event loop that can dispatch events to objects living in the class. Cross-thread signal-slot connections are implemented by dispatching a `QMetaCallEvent` to the target object.

A `QObject` instance can be moved to a thread, where it will process its events, such as timer events or slot/method calls.

To do work on a thread, first create your own worker class that derives from `QObject`. Then move it to the thread. The object can run its own code automatically e.g. by using [`QMetaObject::invokeMethod()`](http://doc.qt.io/qt-5/qmetaobject.html#invokeMethod).

    #include <QObject>

    class MyWorker : public QObject
    {
        Q_OBJECT
    public:
        Q_SLOT void doWork() {
            qDebug() << "doWork()" << QThread::currentThread();
            // and do some long operation here
        }
        MyWorker(QObject * parent = nullptr) : QObject{parent} {}
    };

    class MyController : public QObject
    {
        Q_OBJECT
        Worker worker;
        QThread workerThread;
    public:
        MyController()  {
            worker.moveToThread(&workerThread);
            // provide meaningful debug output
            workerThread.setObjectName("workerThread");
            workerThread.start();
            // the thread starts the event loop and blocks waiting for events
        }
        ~MyController()  {
            workerThread.quit();
            workerThread.wait();
        }
        void operate() {
            // Qt::QueuedConnection ensures that the slot is invoked in its own thread
            QMetaObject::invokeMethod(&worker, "doWork", Qt::QueuedConnection);
        }
    };

If your worker should be ephemeral and only exist while its work is being done, it's best to submit a functor or a thread-safe method for execution in the thread pool via `QtConcurrent::run`.

## QtConcurrent Run
If you find managing QThreads and low-level primitives like mutexes or semaphores too complex, Qt Concurrent namespace is what you are looking for. It includes classes which allow more high-level thread management. 

Let's look at Concurrent Run. `QtConcurrent::run()` allows to run function in a new thread. When would you like to use it? When you have some long operation and you don't want to create thread manually. 

Now the code:

    #include <qtconcurrentrun.h>

    void longOperationFunction(string parameter)
    {
        // we are already in another thread
        // long stuff here
    } 
    
    void mainThreadFunction()
    {
        QFuture<void> f = run(longOperationFunction, "argToPass");
        f.waitForFinished();
    }

So things are simple: when we need to run another function in another thread, just call `QtConcurrent::run`, pass function and its parameters and that's it!

`QFuture` presents the result of our asynchronous computation. In case of `QtConcurrent::run` we can't cancel the function execution.


## Invoking slots from other threads
When a Qt event loop is used to perform operations and a non-Qt-saavy user needs to interact with that event loop, writing the slot to handle regular invocations from another thread can simplify things for other users.

main.cpp:

    #include "OperationExecutioner.h"
    #include <QCoreApplication>
    #include <QThread>
    
    int main(int argc, char** argv)
    {
        QCoreApplication app(argc, argv);
    
        QThread thrd;
        thrd.setObjectName("thrd");
        thrd.start();
        while(!thrd.isRunning())
            QThread::msleep(10);
    
        OperationExecutioner* oe = new OperationExecutioner;
        oe->moveToThread(&thrd);
        oe->doIt1(123,'A');
        oe->deleteLater();
        thrd.quit();
        while(!thrd.isFinished())
            QThread::msleep(10);
    
        return 0;
    }

OperationExecutioner.h:

    #ifndef OPERATION_EXECUTIONER_H
    #define OPERATION_EXECUTIONER_H
    
    #include <QObject>
    
    class OperationExecutioner : public QObject
    {
        Q_OBJECT
    public slots:
        void doIt1(int argi, char argc);
    };
    
    #endif  // OPERATION_EXECUTIONER_H

OperationExecutioner.cpp:

    #include "OperationExecutioner.h"
    #include <QMetaObject>
    #include <QThread>
    #include <QDebug>
    
    void OperationExecutioner::doIt1(int argi, char argc)
    {
        if (QThread::currentThread() != thread()) {
            qInfo() << "Called from thread" << QThread::currentThread();
            QMetaObject::invokeMethod(this, "doIt1", Qt::QueuedConnection,
                                      Q_ARG(int,argi), Q_ARG(char,argc));
            return;
        }
    
        qInfo() << "Called from thread" << QThread::currentThread()
                << "with args" << argi << argc;
    }

OperationExecutioner.pro:

    HEADERS += OperationExecutioner.h
    SOURCES += main.cpp OperationExecutioner.cpp
    QT -= gui
    

