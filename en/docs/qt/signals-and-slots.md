---
title: "Signals and Slots"
slug: "signals-and-slots"
draft: false
images: []
weight: 9785
type: docs
toc: true
---

Signals and slots are used for communication between objects. The signals and slots mechanism is a central feature of Qt.

In GUI programming, when we change one widget, we often want another widget to be notified. More generally, we want objects of any kind to be able to communicate with one another.

Signals are emitted by objects when they change their state in a way that may be interesting to other objects.

Slots can be used for receiving signals, but they are also normal member functions.

Official documentation on this topic can be found [here][1].

[1]: http://doc.qt.io/qt-5/signalsandslots.html

## The new Qt5 connection syntax
The conventional `connect` syntax that uses `SIGNAL` and `SLOT` macros works entirely at runtime, which has two drawbacks: it has some runtime overhead (resulting also in binary size overhead), and there's no compile-time correctness checking. The new syntax addresses both issues. Before checking the syntax in an example, we'd better know what happens in particular.

Let's say we are building a house and we want to connect the cables. This is exactly what connect function does. Signals and slots are the ones needing this connection. The point is if you do one connection, you need to be careful about the further overlaping connections. Whenever you connect a signal to a slot, you are trying to tell the compiler that whenever the signal was emitted, simply invoke the slot function. This is what exactly happens.

Here's a sample **main.cpp**:

    #include <QApplication>
    #include <QDebug>
    #include <QTimer>
    
    inline void onTick()
    {
       qDebug() << "onTick()";
    }
    
    struct OnTimerTickListener {
       void onTimerTick()
       {
           qDebug() << "OnTimerTickListener::onTimerTick()";
       }
    };
    
    int main(int argc, char *argv[])
    {
        QApplication app(argc, argv);
    
        OnTimerTickListener listenerObject;
    
        QTimer timer;
        // Connecting to a non-member function
        QObject::connect(&timer, &QTimer::timeout, onTick);
        // Connecting to an object member method
        QObject::connect(&timer, &QTimer::timeout, &listenerObject, &OnTimerTickListener::onTimerTick);
        // Connecting to a lambda
        QObject::connect(&timer, &QTimer::timeout, [](){
            qDebug() << "lambda-onTick";
        });    
    
        return app.exec();
    }

Hint: the old syntax (`SIGNAL`/`SLOT` macros) requires that the Qt metacompiler (MOC) is run for any class that has either slots or signals. From the coding standpoint that means that such classes need to have the `Q_OBJECT` macro (which indicates the necessity to run MOC on this class).

The new syntax, on the other hand, still requires MOC for signals to work, but **not** for slots. If a class only has slots and no signals, it need not have the `Q_OBJECT` macro and hence may not invoke the MOC, which not only reduces the final binary size but also reduces compilation time (no MOC call and no subsequent compiler call for the generated `*_moc.cpp` file).

## Connecting overloaded signals/slots
While being better in many regards, the new connection syntax in Qt5 has one big weakness: Connecting overloaded signals and slots. In order to let the compiler resolve the overloads we need to use `static_cast`s to member function pointers, or (starting in Qt 5.7) `qOverload` and friends:

    #include <QObject>
    
    class MyObject : public QObject
    {
        Q_OBJECT
    public:
        explicit MyObject(QObject *parent = nullptr) : QObject(parent) {}
    
    public slots:
        void slot(const QString &string) {}
        void slot(const int integer) {}

    signals:
        void signal(const QString &string) {}
        void signal(const int integer) {}
    };

    int main(int argc, char **argv)
    {
        QCoreApplication app(argc, argv);

        // using pointers to make connect calls just a little simpler
        MyObject *a = new MyObject;
        MyObject *b = new MyObject;

        // COMPILE ERROR! the compiler does not know which overloads to pick :(
        QObject::connect(a, &MyObject::signal, b, &MyObject::slot);

        // this works, now the compiler knows which overload to pick, it is very ugly and hard to remember though...
        QObject::connect(
            a,
            static_cast<void(MyObject::*)(int)>(&MyObject::signal),
            b,
            static_cast<void(MyObject::*)(int)>(&MyObject::slot));

        // ...so starting in Qt 5.7 we can use qOverload and friends:
        // this requires C++14 enabled:
        QObject::connect(
            a,
            qOverload<int>(&MyObject::signal),
            b,
            qOverload<int>(&MyObject::slot));

        // this is slightly longer, but works in C++11:
        QObject::connect(
            a,
            QOverload<int>::of(&MyObject::signal),
            b,
            QOverload<int>::of(&MyObject::slot));

        // there are also qConstOverload/qNonConstOverload and QConstOverload/QNonConstOverload, the names should be self-explanatory
    }

## A Small Example
Signals and slots are used for communication between objects. The signals and slots mechanism is a central feature of Qt and probably the part that differs most from the features provided by other frameworks.

The minimal example requires a class with one signal, one slot and one connection:

**counter.h**

    #ifndef COUNTER_H
    #define COUNTER_H
    
    #include <QWidget>
    #include <QDebug>
    
    class Counter : public QWidget
    {
        /*
         * All classes that contain signals or slots must mention Q_OBJECT
         * at the top of their declaration.
         * They must also derive (directly or indirectly) from QObject.
         */
        Q_OBJECT
    
    public:
        Counter (QWidget *parent = 0): QWidget(parent)
        {
                m_value = 0;
    
                /*
                 * The most important line: connect the signal to the slot.
                 */
                connect(this, &Counter::valueChanged, this, &Counter::printvalue);
        }
    
        void setValue(int value)
        {
            if (value != m_value) {
                m_value = value;
                /*
                 * The emit line emits the signal valueChanged() from
                 * the object, with the new value as argument.
                 */
                emit valueChanged(m_value);
            }
        }
    
    public slots:
        void printValue(int value)
        {
            qDebug() << "new value: " << value;
        }
    
    signals:
        void valueChanged(int newValue);
    
    private:
        int m_value;
    
    };
    
    #endif

The `main` sets a new value. We can check how the slot is called, printing the value.

    #include <QtGui>
    #include "counter.h"
    
    int main(int argc, char *argv[])
    {
        QApplication app(argc, argv);
    
        Counter counter;
        counter.setValue(10);
        counter.show();
    
        return app.exec();
    }

Finally, our project file:

    SOURCES   = \
                main.cpp
    HEADERS   = \
                counter.h



## Multi window signal slot connection
A simple multiwindow example using signals and slots.

There is a MainWindow class that controls the Main Window view. A second window controlled by Website class.

The two classes are connected so that when you click a button on the Website window something happens in the MainWindow (a text label is changed).

I made a simple example that is also on [GitHub][1]:

**mainwindow.h**

    #ifndef MAINWINDOW_H
    #define MAINWINDOW_H
    
    #include <QMainWindow>
    #include "website.h"
    
    namespace Ui {
    class MainWindow;
    }
    
    class MainWindow : public QMainWindow
    {
        Q_OBJECT
    
    public:
        explicit MainWindow(QWidget *parent = 0);
        ~MainWindow();
    
    public slots:
        void changeText();
    
    private slots:
        void on_openButton_clicked();
    
    private:
        Ui::MainWindow *ui;
    
        //You want to keep a pointer to a new Website window
        Website* webWindow;
    };
    
    #endif // MAINWINDOW_H


**mainwindow.cpp**

    #include "mainwindow.h"
    #include "ui_mainwindow.h"
    
    MainWindow::MainWindow(QWidget *parent) :
        QMainWindow(parent),
        ui(new Ui::MainWindow)
    {
        ui->setupUi(this);
    }
    
    MainWindow::~MainWindow()
    {
        delete ui;
    }
    
    void MainWindow::changeText()
    {
        ui->text->setText("New Text");
        delete webWindow;
    }
    
    void MainWindow::on_openButton_clicked()
    {
        webWindow = new Website();
        QObject::connect(webWindow, SIGNAL(buttonPressed()), this, SLOT(changeText()));
        webWindow->show();
    }

**website.h**

    #ifndef WEBSITE_H
    #define WEBSITE_H
    
    #include <QDialog>
    
    namespace Ui {
    class Website;
    }
    
    class Website : public QDialog
    {
        Q_OBJECT
    
    public:
        explicit Website(QWidget *parent = 0);
        ~Website();
    
    signals:
        void buttonPressed();
    
    private slots:
        void on_changeButton_clicked();
    
    private:
        Ui::Website *ui;
    };
    
    #endif // WEBSITE_H

**website.cpp**

    #include "website.h"
    #include "ui_website.h"
    
    Website::Website(QWidget *parent) :
        QDialog(parent),
        ui(new Ui::Website)
    {
        ui->setupUi(this);
    }
    
    Website::~Website()
    {
        delete ui;
    }
    
    void Website::on_changeButton_clicked()
    {
        emit buttonPressed();
    }

**Project composition:**

    SOURCES += main.cpp\
            mainwindow.cpp \
        website.cpp
    
    HEADERS  += mainwindow.h \
        website.h
    
    FORMS    += mainwindow.ui \
        website.ui

Consider the Uis to be composed:

 - Main Window: a label called "text" and a button called "openButton"
 - Website Window: a button called "changeButton"

So the keypoints are the connections between signals and slots and the management of windows pointers or references.


  [1]: https://github.com/LucaAngioloni/QTMultiwindowExample

