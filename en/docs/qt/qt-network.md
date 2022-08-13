---
title: "Qt Network"
slug: "qt-network"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Qt Network provide tools to easily use many network protocols in your application.


## TCP Client
To create a **TCP** connection in Qt, we will use [QTcpSocket][1]. First, we need to connect with `connectToHost`.

So for example, to connect to a local tcp serveur: `_socket.connectToHost(QHostAddress("127.0.0.1"), 4242);`

Then, if we need to read datas from the server, we need to connect the signal readyRead with a slot. Like that:
 

    connect(&_socket, SIGNAL(readyRead()), this, SLOT(onReadyRead()));

and finally, we can read the datas like that:

    void MainWindow::onReadyRead()
    {
        QByteArray datas = _socket.readAll();
        qDebug() << datas;
    }

To write datas, you can use the `write(QByteArray)` method:

    _socket.write(QByteArray("ok !\n"));


So a basic TCP Client can look like that:


**main.cpp:**

    #include "mainwindow.h"
    #include <QApplication>
    
    int main(int argc, char *argv[])
    {
        QApplication a(argc, argv);
        MainWindow w;
        w.show();
    
        return a.exec();
    }

**mainwindow.h:**

    #ifndef MAINWINDOW_H
    #define MAINWINDOW_H
    
    #include <QMainWindow>
    #include <QTcpSocket>
    
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
        void onReadyRead();
    
    private:
        Ui::MainWindow *ui;
        QTcpSocket  _socket;
    };
    
    #endif // MAINWINDOW_H

**mainwindow.cpp:**

    #include "mainwindow.h"
    #include "ui_mainwindow.h"
    
    #include <QDebug>
    #include <QHostAddress>
    
    MainWindow::MainWindow(QWidget *parent) :
        QMainWindow(parent),
        ui(new Ui::MainWindow),
        _socket(this)
    {
        ui->setupUi(this);
        _socket.connectToHost(QHostAddress("127.0.0.1"), 4242);
        connect(&_socket, SIGNAL(readyRead()), this, SLOT(onReadyRead()));
    }
    
    MainWindow::~MainWindow()
    {
        delete ui;
    }
    
    void MainWindow::onReadyRead()
    {
        QByteArray datas = _socket.readAll();
        qDebug() << datas;
        _socket.write(QByteArray("ok !\n"));
    }

**mainwindow.ui:** (empty here)

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
      <widget class="QWidget" name="centralWidget"/>
      <widget class="QMenuBar" name="menuBar">
       <property name="geometry">
        <rect>
         <x>0</x>
         <y>0</y>
         <width>400</width>
         <height>25</height>
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

  [1]: http://doc.qt.io/qt-5/qtcpsocket.html





## TCP Server
Create a **TCP server** in Qt is also very easy, indeed, the class [QTcpServer][1] already provide all we need to do the server.

First, we need to listen to any ip, a random port and do something when a client is connected. like that:

  

     _server.listen(QHostAddress::Any, 4242);
     connect(&_server, SIGNAL(newConnection()), this, SLOT(onNewConnection()));

Then, When here is a new connection, we can add it to the client list and prepare to read/write on the socket. Like that:

       QTcpSocket *clientSocket = _server.nextPendingConnection();
       connect(clientSocket, SIGNAL(readyRead()), this, SLOT(onReadyRead()));
       connect(clientSocket, SIGNAL(stateChanged(QAbstractSocket::SocketState)), this, SLOT(onSocketStateChanged(QAbstractSocket::SocketState)));
       _sockets.push_back(clientSocket);

   The `stateChanged(QAbstractSocket::SocketState)` allow us to remove the socket to our list when the client is disconnected.

So here a basic chat server:

**main.cpp:**

    #include "mainwindow.h"
    #include <QApplication>
    
    int main(int argc, char *argv[])
    {
        QApplication a(argc, argv);
        MainWindow w;
        w.show();
    
        return a.exec();
    }

**mainwindow.h:**

    #ifndef MAINWINDOW_H
    #define MAINWINDOW_H
    
    #include <QMainWindow>
    #include <QTcpServer>
    #include <QTcpSocket>
    
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
        void onNewConnection();
        void onSocketStateChanged(QAbstractSocket::SocketState socketState);
        void onReadyRead();
    private:
        Ui::MainWindow *ui;
        QTcpServer  _server;
        QList<QTcpSocket*>  _sockets;
    };
    
    #endif // MAINWINDOW_H


**mainwindow.cpp:**

    #include "mainwindow.h"
    #include "ui_mainwindow.h"
    
    #include <QDebug>
    #include <QHostAddress>
    #include <QAbstractSocket>
    
    MainWindow::MainWindow(QWidget *parent) :
        QMainWindow(parent),
        ui(new Ui::MainWindow),
        _server(this)
    {
        ui->setupUi(this);
        _server.listen(QHostAddress::Any, 4242);
        connect(&_server, SIGNAL(newConnection()), this, SLOT(onNewConnection()));
    }
    
    MainWindow::~MainWindow()
    {
        delete ui;
    }
    
    void MainWindow::onNewConnection()
    {
       QTcpSocket *clientSocket = _server.nextPendingConnection();
       connect(clientSocket, SIGNAL(readyRead()), this, SLOT(onReadyRead()));
       connect(clientSocket, SIGNAL(stateChanged(QAbstractSocket::SocketState)), this, SLOT(onSocketStateChanged(QAbstractSocket::SocketState)));
    
        _sockets.push_back(clientSocket);
        for (QTcpSocket* socket : _sockets) {
            socket->write(QByteArray::fromStdString(clientSocket->peerAddress().toString().toStdString() + " connected to server !\n"));
        }
    }
    
    void MainWindow::onSocketStateChanged(QAbstractSocket::SocketState socketState)
    {
        if (socketState == QAbstractSocket::UnconnectedState)
        {
            QTcpSocket* sender = static_cast<QTcpSocket*>(QObject::sender());
            _sockets.removeOne(sender);
        }
    }
    
    void MainWindow::onReadyRead()
    {
        QTcpSocket* sender = static_cast<QTcpSocket*>(QObject::sender());
        QByteArray datas = sender->readAll();
        for (QTcpSocket* socket : _sockets) {
            if (socket != sender)
                socket->write(QByteArray::fromStdString(sender->peerAddress().toString().toStdString() + ": " + datas.toStdString()));
        }
    }

(use the same mainwindow.ui that the previous example)

  [1]: http://doc.qt.io/qt-5/qtcpserver.html


