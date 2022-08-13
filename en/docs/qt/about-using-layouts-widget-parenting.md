---
title: "About using layouts, widget parenting"
slug: "about-using-layouts-widget-parenting"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

The layouts are a necessary in every Qt application. They manage the object, their position, their size, how they are resized.

From [Qt layout documentation][1]:


> When you use a layout, you do not need to pass a parent when
> constructing the child widgets. The layout will automatically reparent
> the widgets (using QWidget::setParent()) so that they are children of
> the widget on which the layout is installed.

So do :

    QGroupBox *box = new QGroupBox("Information:", widget);
    layout->addWidget(box);

or do :

    QGroupBox *box = new QGroupBox("Information:", nullptr);
    layout->addWidget(box);

is exactly the same.


  [1]: http://doc.qt.io/qt-5/layout.html

## Basic Horizontal Layout
The horizontal layout set up the object inside it horizontally.

basic code:

    #include <QApplication>
    
    #include <QMainWindow>
    #include <QWidget>
    #include <QHBoxLayout>
    #include <QPushButton>
    
    int main(int argc, char *argv[])
    {
        QApplication a(argc, argv);
    
        QMainWindow window;
        QWidget *widget = new QWidget(&window);
        QHBoxLayout *layout = new QHBoxLayout(widget);
    
        window.setCentralWidget(widget);
        widget->setLayout(layout);
    
        
        
        layout->addWidget(new QPushButton("hello world", widget));
        layout->addWidget(new QPushButton("I would like to have a layout !", widget));
        layout->addWidget(new QPushButton("layouts are so great !", widget));
    
        window.show();
    
        return a.exec();
    }

this will output:

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/yTYgz.png

## Basic Vertical Layout
The vertical layout set up the object inside it vertically.

    #include "mainwindow.h"
    #include <QApplication>
    
    #include <QMainWindow>
    #include <QWidget>
    #include <QVBoxLayout>
    #include <QPushButton>
    
    int main(int argc, char *argv[])
    {
        QApplication a(argc, argv);
    
        QMainWindow window;
        QWidget *widget = new QWidget(&window);
        QVBoxLayout *layout = new QVBoxLayout(widget);
    
        window.setCentralWidget(widget);
        widget->setLayout(layout);
    
    
    
        layout->addWidget(new QPushButton("hello world", widget));
        layout->addWidget(new QPushButton("I would like to have a layout !", widget));
        layout->addWidget(new QPushButton("layouts are so great !", widget));
    
        window.show();
    
        return a.exec();
    }

output:

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/pN1wX.png

## Combining Layouts
You can combine mulple layout thanks to other QWidgets in your main layout to do more specifics effects like an information field: for example:

    #include <QApplication>
    
    #include <QMainWindow>
    #include <QWidget>
    #include <QVBoxLayout>
    #include <QPushButton>
    #include <QLabel>
    #include <QLineEdit>
    #include <QGroupBox>
    
    #include <QTextEdit>
    
    int main(int argc, char *argv[])
    {
        QApplication a(argc, argv);
    
        QMainWindow window;
        QWidget *widget = new QWidget(&window);
        QVBoxLayout *layout = new QVBoxLayout(widget);
    
        window.setCentralWidget(widget);
        widget->setLayout(layout);
    
        QGroupBox *box = new QGroupBox("Information:", widget);
        QVBoxLayout *boxLayout = new QVBoxLayout(box);
    
        layout->addWidget(box);
    
    
        QWidget* nameWidget = new QWidget(box);
        QWidget* ageWidget = new QWidget(box);
        QWidget* addressWidget = new QWidget(box);
    
        boxLayout->addWidget(nameWidget);
        boxLayout->addWidget(ageWidget);
        boxLayout->addWidget(addressWidget);
    
        QHBoxLayout *nameLayout = new QHBoxLayout(nameWidget);
        nameLayout->addWidget(new QLabel("Name:"));
        nameLayout->addWidget(new QLineEdit(nameWidget));
    
    
        QHBoxLayout *ageLayout = new QHBoxLayout(ageWidget);
        ageLayout->addWidget(new QLabel("Age:"));
        ageLayout->addWidget(new QLineEdit(ageWidget));
    
        QHBoxLayout *addressLayout = new QHBoxLayout(addressWidget);
        addressLayout->addWidget(new QLabel("Address:"));
        addressLayout->addWidget(new QLineEdit(addressWidget));
    
    
    
        QWidget* validateWidget = new QWidget(widget);
        QHBoxLayout *validateLayout = new QHBoxLayout(validateWidget);
        validateLayout->addWidget(new QPushButton("Validate", validateWidget));
        validateLayout->addWidget(new QPushButton("Reset", validateWidget));
        validateLayout->addWidget(new QPushButton("Cancel", validateWidget));
    
        layout->addWidget(validateWidget);
    
        window.show();
    
        return a.exec();
    }


will output :

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/6YOxe.png

## Grid layout example
The grid layout is a powerful layout with which you can do an horizontal and vertical layout a once.

example:

    #include "mainwindow.h"
    #include <QApplication>
    
    #include <QMainWindow>
    #include <QWidget>
    #include <QVBoxLayout>
    #include <QPushButton>
    #include <QLabel>
    #include <QLineEdit>
    #include <QGroupBox>
    
    #include <QTextEdit>
    
    int main(int argc, char *argv[])
    {
        QApplication a(argc, argv);
    
        QMainWindow window;
        QWidget *widget = new QWidget(&window);
        QGridLayout *layout = new QGridLayout(widget);
    
        window.setCentralWidget(widget);
        widget->setLayout(layout);
    
        QGroupBox *box = new QGroupBox("Information:", widget);
        layout->addWidget(box, 0, 0);
    
        QVBoxLayout *boxLayout = new QVBoxLayout(box);
    
        QWidget* nameWidget = new QWidget(box);
        QWidget* ageWidget = new QWidget(box);
        QWidget* addressWidget = new QWidget(box);
    
        boxLayout->addWidget(nameWidget);
        boxLayout->addWidget(ageWidget);
        boxLayout->addWidget(addressWidget);
    
        QHBoxLayout *nameLayout = new QHBoxLayout(nameWidget);
        nameLayout->addWidget(new QLabel("Name:"));
        nameLayout->addWidget(new QLineEdit(nameWidget));
    
    
        QHBoxLayout *ageLayout = new QHBoxLayout(ageWidget);
        ageLayout->addWidget(new QLabel("Age:"));
        ageLayout->addWidget(new QLineEdit(ageWidget));
    
        QHBoxLayout *addressLayout = new QHBoxLayout(addressWidget);
        addressLayout->addWidget(new QLabel("Address:"));
        addressLayout->addWidget(new QLineEdit(addressWidget));
    
    
    
        layout->addWidget(new QPushButton("Validate", widget), 1, 0);
        layout->addWidget(new QPushButton("Reset", widget), 1, 1);
        layout->addWidget(new QPushButton("Cancel", widget), 1, 2);
    
    
        window.show();
    
        return a.exec();
    }

give :

[![enter image description here][1]][1]


so you can see that the group box is only in the first column and first row as the addWidget was `layout->addWidget(box, 0, 0);`

However, if you change it to `layout->addWidget(box, 0, 0, 1, 3);`, the new 0 and 3 represent how many line and column you want for your widget so it give :

[![enter image description here][2]][2] 

exactly the same as you created a horizontal and then a vertical layout in a subwidget.


  [1]: https://i.stack.imgur.com/RwUw1.png
  [2]: https://i.stack.imgur.com/XhhzL.png

