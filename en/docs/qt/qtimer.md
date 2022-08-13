---
title: "QTimer"
slug: "qtimer"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

QTimer can also be used to request a function to run as soon as the event loop has processed all the other pending events.  To do this, use an interval of 0 ms.

    // option 1: Set the interval to 0 explicitly.
    QTimer *timer = new QTimer;
    timer->setInterval( 0 );
    timer->start();

    // option 2: Passing 0 with the start call will set the interval as well.
    QTimer *timer = new QTimer;
    timer->start( 0 );

    // option 3: use QTimer::singleShot with interval 0
    QTimer::singleShot(0, [](){
        // do something
    });

## Using QTimer to run code on main thread
    void DispatchToMainThread(std::function<void()> callback)
    {
        // any thread
        QTimer* timer = new QTimer();
        timer->moveToThread(qApp->thread());
        timer->setSingleShot(true);
        QObject::connect(timer, &QTimer::timeout, [=]()
        {
            // main thread
            callback();
            timer->deleteLater();
        });
        QMetaObject::invokeMethod(timer, "start", Qt::QueuedConnection, Q_ARG(int, 0));
    }

This is useful when you need to update a UI element from a thread. Keep in mind lifetime of anything the callback references.

    DispatchToMainThread([]
    {
        // main thread
        // do UI work here
    });

Same code could be adapted to run code on any thread that runs Qt event loop, thus implementing a simple dispatch mechanism.

## Simple example
The following example shows how to use a `QTimer` to call a slot every 1 second.

In the example, we use a `QProgressBar` to update its value and check the timer is working properly.

**main.cpp**

    #include <QApplication>
    
    #include "timer.h"
    
    int main(int argc, char *argv[])
    {
        QApplication app(argc, argv);
    
        Timer timer;
        timer.show();
    
        return app.exec();
    }

**timer.h**

    #ifndef TIMER_H
    #define TIMER_H
    
    #include <QWidget>
    
    class QProgressBar;
    
    class Timer : public QWidget
    {
        Q_OBJECT
    
    public:
        Timer(QWidget *parent = 0);
    
    public slots:
        void updateProgress();
    
    private:
        QProgressBar *progressBar;
    };
    
    #endif

**timer.cpp**

    #include <QLayout>
    #include <QProgressBar>
    #include <QTimer>
    
    #include "timer.h"
    
    Timer::Timer(QWidget *parent)
        : QWidget(parent)
    {
        QHBoxLayout *layout = new QHBoxLayout();
    
        progressBar = new QProgressBar();
        progressBar->setMinimum(0);
        progressBar->setMaximum(100);
    
        layout->addWidget(progressBar);
        setLayout(layout);
    
        QTimer *timer = new QTimer(this);
        connect(timer, &QTimer::timeout, this, &Timer::updateProgress);
        timer->start(1000);
    
        setWindowTitle(tr("Timer"));
        resize(200, 200);
    }
    
    void Timer::updateProgress()
    {
        progressBar->setValue(progressBar->value()+1);
    }

**timer.pro**

    QT += widgets
    
    HEADERS = \
              timer.h
    SOURCES = \
               main.cpp \
               timer.cpp

## Singleshot Timer with Lambda function as slot
If a singleshot timer is required, it is quiet handy to have the slot as lambda function right in the place where the timer is declared:

    QTimer::singleShot(1000, []() { /*Code here*/ } );

Due to [this Bug (QTBUG-26406)][1], this is way is only possible since Qt5.4.

In earlier Qt5 versions it has to be done with more boiler plate code:

      QTimer *timer = new QTimer(this);
      timer->setSingleShot(true);
    
      connect(timer, &QTimer::timeout, [=]() {
        /*Code here*/
        timer->deleteLater();
      } );



  [1]: https://bugreports.qt.io/browse/QTBUG-26406

## Basic Usage
`QTimer` add the functionality to have a specific function/slot called after a certain interval (repeatedly or just once). 

The `QTimer` thus allows a GUI application to "check" things regularly or handle timeouts **without** having to manually start an extra thread for this and be careful about race conditions, because the timer will be handled in the main-event loop.

A timer can simply be used like this:

    QTimer* timer = new QTimer(parent); //create timer with optional parent object
    connect(timer,&QTimer::timeout,[this](){ checkProgress(); }); //some function to check something
    timer->start(1000); //start with a 1s interval

The timer triggers the `timeout` signal when the time is over and this will be called in the main-event loop. 

## QTimer::singleShot simple usage
The **QTimer::singleShot** is used to call a slot/lambda **asynchronously** after n ms.

The basic syntax is :

    QTimer::singleShot(myTime, myObject, SLOT(myMethodInMyObject()));

with **myTime** the time in ms, **myObject** the object which contain the method and **myMethodInMyObject** the slot to call

So for example if you want to have a timer who write a debug line "hello !" every 5 seconds:

.cpp

    void MyObject::startHelloWave()
    {
        QTimer::singleShot(5 * 1000, this, SLOT(helloWave()));
    }
    
    void MyObject::helloWave()
    {
        qDebug() << "hello !";
        QTimer::singleShot(5 * 1000, this, SLOT(helloWave()));
    }

.hh

    class MyObject : public QObject {
        Q_OBJECT
        ...
        void startHelloWave();

    private slots:
        void helloWave();
        ...
    };

