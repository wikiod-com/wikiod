---
title: "Communication between QML and C++"
slug: "communication-between-qml-and-c++"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

We may use QML to build hybrid applications, since it's much more easier than C++. So we should know how they communicate with each other.

## Call C++ in QML
**Register C++ classes in QML**

At C++ side, imagine we have a class named `QmlCppBridge`, it implements a method called `printHello()`.

    class QmlCppBridge : public QObject
    {
        Q_OBJECT
    public:
        Q_INVOKABLE static void printHello() {
            qDebug() << "Hello, QML!";
        }
    };

We want to use it in QML side. We should register the class by calling `qmlRegisterType()`:

    // Register C++ class as a QML module, 1 & 0 are the major and minor version of the QML module
    qmlRegisterType<QmlCppBridge>("QmlCppBridge", 1, 0, "QmlCppBridge");

In QML, use following code to call it:

    import QmlCppBridge 1.0    // Import this module, so we can use it in our QML script

    QmlCppBridge {
        id: bridge
    }
    bridge.printHello();

**Using `QQmlContext` to inject C++ classes or variables to QML**

We still use the C++ class in previous example:

    QQmlApplicationEngine engine;
    QQmlContext *context = engine.rootContext();

    // Inject C++ class to QML
    context->setContextProperty(QStringLiteral("qmlCppBridge"), new QmlCppBridge(&engine));

    // Inject C++ variable to QML
    QString demoStr = QStringLiteral("demo");
    context->setContextProperty(QStringLiteral("demoStr"), demoStr);

At QML side:

    qmlCppBridge.printHello();    // Call to C++ function
    str: demoStr                  // Fetch value of C++ variable

> **Note:** This example is based on Qt 5.7. Not sure if it fits earlier Qt versions.

## Call QML in C++
To call the QML classes in C++, you need to set the objectName property.

In your Qml:

    import QtQuick.Controls 2.0
    
    Button {
        objectName: "buttonTest"
    }

Then, in your C++, you can get the object with `QObject.FindChild<QObject*>(QString)`

Like that:

    QQmlApplicationEngine engine;
    QQmlComponent component(&engine, QUrl(QLatin1String("qrc:/main.qml")));

    QObject *mainPage = component.create();
    QObject* item = mainPage->findChild<QObject *>("buttonTest");

Now you have your QML object in your C++. But that could seems useless since we cannot really get the components of the object.

However, we can use it to send **signals** between the QML and the C++. To do that, you need to add a signal in your QML file like that: `signal buttonClicked(string str)`.
Once you create this, you need to emit the signal. For example:

    import QtQuick 2.0
    import QtQuick.Controls 2.1
    
        Button {
            id: buttonTest
            objectName: "buttonTest"
    
            signal clickedButton(string str)
            onClicked: {
                buttonTest.clickedButton("clicked !")
            }
        }

Here we have our qml button. When we click on it, it goes to the **onClicked** method (a base method for buttons which is called when you press the button). Then we use the id of the button and the name of the signal to emit the signal.

And in our cpp, we need to connect the signal with a slot. like that:

main.cpp

    #include <QGuiApplication>
    #include <QQmlApplicationEngine>
    #include <QQmlComponent>
    
    #include "ButtonManager.h"
    
    int main(int argc, char *argv[])
    {
        QCoreApplication::setAttribute(Qt::AA_EnableHighDpiScaling);
        QGuiApplication app(argc, argv);
    
        QQmlApplicationEngine engine;
        QQmlComponent component(&engine, QUrl(QLatin1String("qrc:/main.qml")));
    
        QObject *mainPage = component.create();
        QObject* item = mainPage->findChild<QObject *>("buttonTest");
    
        ButtonManager buttonManager(mainPage);
        QObject::connect(item, SIGNAL(clickedButton(QString)), &buttonManager, SLOT(onButtonClicked(QString)));
    
        return app.exec();
    }

As you can see, we get our qml button with `findChild` as before and we connect the signal to a Button manager which is a class created and who look like that.
ButtonManager.h 

    #ifndef BUTTONMANAGER_H
    #define BUTTONMANAGER_H
    
    #include <QObject>
    
    class ButtonManager : public QObject
    {
        Q_OBJECT
    public:
        ButtonManager(QObject* parent = nullptr);
    public slots:
        void onButtonClicked(QString str);
    };
    
    #endif // BUTTONMANAGER_H


ButtonManager.cpp

    #include "ButtonManager.h"
    #include <QDebug>
    
    ButtonManager::ButtonManager(QObject *parent)
        : QObject(parent)
    {
    
    }
    
    void ButtonManager::onButtonClicked(QString str)
    {
        qDebug() << "button: " << str;
    }

So when the signal will be received, it will call the method `onButtonClicked` which will write `"button: clicked !"`

output:

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/Qxb37.png

