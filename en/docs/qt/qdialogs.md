---
title: "QDialogs"
slug: "qdialogs"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

The QDialog class is the **base** class of dialog windows.
A dialog window is a top-level window mostly used for short-term tasks and brief communications with the user. QDialogs may be **modal** or **modeless**.

Note that QDialog (and any other widget that has type Qt::Dialog) uses the parent widget slightly differently from other classes in Qt. A dialog is **always a top-level widget**, but if **it has a parent, its default location is centered on top of the parent's top-level widget** (if it is not top-level itself). It will also share the parent's taskbar entry.

A **modal** dialog is a dialog that blocks input to other visible windows in the same application. Dialogs that are used to request a file name from the user or that are used to set application preferences are usually modal. Dialogs can be **application modal** (the default) or **window modal**.

The most common way to display a modal dialog is to call its exec() function. When the user closes the dialog, exec() will provide a useful return value.

A **modeless** dialog is a dialog that operates independently of other windows in the same application.
Modeless dialogs are displayed using show(), which returns control to the caller immediately.


## MyCompareFileDialog.h
    #ifndef MYCOMPAREFILEDIALOG_H
    #define MYCOMPAREFILEDIALOG_H
    
    #include <QtWidgets/QDialog>
    
    class MyCompareFileDialog : public QDialog
    {
        Q_OBJECT
    
    public:
        MyCompareFileDialog(QWidget *parent = 0);
        ~MyCompareFileDialog();
    };
    
    
    #endif // MYCOMPAREFILEDIALOG_H



## MyCompareFileDialogDialog.cpp
    #include "MyCompareFileDialog.h"
    #include <QLabel>
    
    MyCompareFileDialog::MyCompareFileDialog(QWidget *parent)
    : QDialog(parent)
    {
        setWindowTitle("Compare Files");
        setWindowFlags(Qt::Dialog);
        setWindowModality(Qt::WindowModal);
    
        resize(300, 100);
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);
        setSizePolicy(sizePolicy);
        setMinimumSize(QSize(300, 100));
        setMaximumSize(QSize(300, 100));
    
        QLabel* myLabel = new QLabel(this);
        myLabel->setText("My Dialog!");
    }
    
    MyCompareFileDialog::~MyCompareFileDialog()
    { }



## MainWindow.h
    #ifndef MAINWINDOW_H
    #define MAINWINDOW_H
    
    #include <QMainWindow>
    
    namespace Ui {
    class MainWindow;
    }
    
    class MyCompareFileDialog;
    
    class MainWindow : public QMainWindow
    {
        Q_OBJECT
    
    public:
        explicit MainWindow(QWidget *parent = 0);
        ~MainWindow();
    
    private:
        Ui::MainWindow *ui;
        MyCompareFileDialog* myDialog;
    };
    
    #endif // MAINWINDOW_H

## MainWindow.cpp
    #include "mainwindow.h"
    #include "ui_mainwindow.h"
    #include "mycomparefiledialog.h"
    
    MainWindow::MainWindow(QWidget *parent) :
        QMainWindow(parent),
        ui(new Ui::MainWindow)
    {
        ui->setupUi(this);
    
        myDialog = new MyCompareFileDialog(this);
    
        connect(ui->pushButton,SIGNAL(clicked()),myDialog,SLOT(exec()));
    }
    
    MainWindow::~MainWindow()
    {
        delete ui;
    }



## main.cpp
    #include "mainwindow.h"
    #include <QApplication>
    
    int main(int argc, char *argv[])
    {
        QApplication a(argc, argv);
        MainWindow w;
        w.show();
    
        return a.exec();
    }



## mainwindow.ui
    <?xml version="1.0" encoding="UTF-8"?>
    <ui version="4.0">
     <class>MainWindow</class>
     <widget class="QMainWindow" name="MainWindow">
      <property name="geometry">
       <rect>
        <x>0</x>
        <y>0</y>
        <width>400</width>
        <height>300</height>
       </rect>
      </property>
      <property name="windowTitle">
       <string>MainWindow</string>
      </property>
      <widget class="QWidget" name="centralWidget">
       <widget class="QPushButton" name="pushButton">
        <property name="geometry">
         <rect>
          <x>140</x>
          <y>80</y>
          <width>111</width>
          <height>23</height>
         </rect>
        </property>
        <property name="text">
         <string>Show My Dialog</string>
        </property>
       </widget>
      </widget>
      <widget class="QMenuBar" name="menuBar">
       <property name="geometry">
        <rect>
         <x>0</x>
         <y>0</y>
         <width>400</width>
         <height>21</height>
        </rect>
       </property>
      </widget>
      <widget class="QToolBar" name="mainToolBar">
       <attribute name="toolBarArea">
        <enum>TopToolBarArea</enum>
       </attribute>
       <attribute name="toolBarBreak">
        <bool>false</bool>
       </attribute>
      </widget>
      <widget class="QStatusBar" name="statusBar"/>
     </widget>
     <layoutdefault spacing="6" margin="11"/>
     <resources/>
     <connections/>
    </ui>



