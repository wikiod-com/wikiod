---
title: "Using Style Sheets Effectively"
slug: "using-style-sheets-effectively"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Setting a UI widget's stylesheet
  You can set the desired UI widget's stylesheet using any valid CSS. The example below will set a QLabel's text color a border around it.

    #include "mainwindow.h"
    #include "ui_mainwindow.h"
    
    MainWindow::MainWindow(QWidget *parent) :
        QMainWindow(parent),
        ui(new Ui::MainWindow)
    {    
        ui->setupUi(this);
        QString style = "color: blue; border: solid black 5px;";
        ui->myLabel->setStylesheet(style); //This can use colors RGB, HSL, HEX, etc.
    }
    
    MainWindow::~MainWindow()
    {
        delete ui;
    }

