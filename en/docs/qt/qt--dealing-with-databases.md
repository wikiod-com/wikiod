---
title: "Qt - Dealing with Databases"
slug: "qt---dealing-with-databases"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

- You will need the Qt SQL plugin corresponding to the type given to `QSqlDatabase::addDatabase` 
- If you don't have the required SQL plugin, Qt will warn you that it can't find the requested driver
- If you don't have the required SQL plugin you will have to compile them from the Qt source

## Using a Database on Qt
In the Project.pro file we add :

    CONFIG += sql

in MainWindow.h we write :

    #include <QMainWindow>
    #include <QSql>
    #include <QDebug>
    
    namespace Ui 
    {
        class MainWindow;
    }
    
    class MainWindow : public QMainWindow
    {
        Q_OBJECT
    
    public:
        explicit MainWindow(QWidget *parent = 0);
        ~MainWindow();
    
    private slots:
    
    private:
        Ui::MainWindow *ui;
        QSqlDatabase db;
    };

Now in MainWindow.cpp :

    #include "mainwindow.h"
    #include "ui_mainwindow.h"
    
    MainWindow::MainWindow(QWidget *parent) :
        QMainWindow(parent),
        ui(new Ui::MainWindow)
    {
        ui->setupUi(this);
    
        db = QSqlDatabase::addDatabase("QT SQL DRIVER" , "CONNECTION NAME");
        db.setDatabaseName("DATABASE NAME");
        if(!db.open())
        {
            qDebug() << "Can't Connect to DB !";
        }
        else
        {
            qDebug() << "Connected Successfully to DB !";
            QSqlQuery query;
            query.prepare("QUERY TO BE SENT TO THE DB");
            if(!query.exec())
            {
                qDebug() << "Can't Execute Query !";
            }
            else
            {
                qDebug() << "Query Executed Successfully !";
            }
        }
    }
    
    MainWindow::~MainWindow()
    {
        delete ui;
    }



## Qt - Dealing with Sqlite Databases
In the Project.pro file we add : `CONFIG += sql`

in MainWindow.h we write :

    #include <QMainWindow>
    #include <QSql>
    #include <QDebug>
    
    namespace Ui 
    {
        class MainWindow;
    }
    
    class MainWindow : public QMainWindow
    {
        Q_OBJECT
    
    public:
        explicit MainWindow(QWidget *parent = 0);
        ~MainWindow();
    
    private slots:
    
    private:
        Ui::MainWindow *ui;
        QSqlDatabase db;
    };

Now in MainWindow.cpp :

    #include "mainwindow.h"
    #include "ui_mainwindow.h"
    
    MainWindow::MainWindow(QWidget *parent) :
        QMainWindow(parent),
        ui(new Ui::MainWindow)
    {
        ui->setupUi(this);
    
        db = QSqlDatabase::addDatabase("QSQLITE" , "CONNECTION NAME");
        db.setDatabaseName("C:\\sqlite_db_file.sqlite");
        if(!db.open())
        {
            qDebug() << "Can't Connect to DB !";
        }
        else
        {
            qDebug() << "Connected Successfully to DB !";
            QSqlQuery query;
            query.prepare("SELECT name , phone , address FROM employees WHERE ID = 201");
            if(!query.exec())
            {
                qDebug() << "Can't Execute Query !";
            }
            else
            {
                qDebug() << "Query Executed Successfully !";
                while(query.next())
                {
                    qDebug() << "Employee Name : " << query.value(0).toString();
                    qDebug() << "Employee Phone Number : " << query.value(1).toString();
                    qDebug() << "Employee Address : " << query.value(1).toString();
                }
            }
        }
    }
    
    MainWindow::~MainWindow()
    {
        delete ui;
    }



## Qt - Dealing with ODBC Databases
In the Project.pro file we add : `CONFIG += sql`

in MainWindow.h we write :

    #include <QMainWindow>
    #include <QSql>
    #include <QDebug>
    
    namespace Ui 
    {
        class MainWindow;
    }
    
    class MainWindow : public QMainWindow
    {
        Q_OBJECT
    
    public:
        explicit MainWindow(QWidget *parent = 0);
        ~MainWindow();
    
    private slots:
    
    private:
        Ui::MainWindow *ui;
        QSqlDatabase db;
    };

Now in MainWindow.cpp :

    #include "mainwindow.h"
    #include "ui_mainwindow.h"
    
    MainWindow::MainWindow(QWidget *parent) :
        QMainWindow(parent),
        ui(new Ui::MainWindow)
    {
        ui->setupUi(this);
    
        db = QSqlDatabase::addDatabase("QODBC" , "CONNECTION NAME");
        db.setDatabaseName("DRIVER={SQL Server};SERVER=localhost;DATABASE=WorkDatabase"); // "WorkDatabase" is the name of the database we want
        db.setUserName("sa"); // Set Login Username
        db.setPassword(""); // Set Password if required
        if(!db.open())
        {
            qDebug() << "Can't Connect to DB !";
        }
        else
        {
            qDebug() << "Connected Successfully to DB !";
            QSqlQuery query;
            query.prepare("SELECT name , phone , address FROM employees WHERE ID = 201");
            if(!query.exec())
            {
                qDebug() << "Can't Execute Query !";
            }
            else
            {
                qDebug() << "Query Executed Successfully !";
                while(query.next())
                {
                    qDebug() << "Employee Name : " << query.value(0).toString();
                    qDebug() << "Employee Phone Number : " << query.value(1).toString();
                    qDebug() << "Employee Address : " << query.value(1).toString();
                }
            }
        }
    }
    
    MainWindow::~MainWindow()
    {
        delete ui;
    }

## Qt - Dealing with in-memory Sqlite Databases
In the Project.pro file we add : `CONFIG += sql`

in MainWindow.h we write :

    #include <QMainWindow>
    #include <QSql>
    #include <QDebug>
    
    namespace Ui 
    {
        class MainWindow;
    }
    
    class MainWindow : public QMainWindow
    {
        Q_OBJECT
    
    public:
        explicit MainWindow(QWidget *parent = 0);
        ~MainWindow();
    
    private slots:
    
    private:
        Ui::MainWindow *ui;
        QSqlDatabase db;
    };

Now in MainWindow.cpp :

    #include "mainwindow.h"
    #include "ui_mainwindow.h"
    
    MainWindow::MainWindow(QWidget *parent) :
        QMainWindow(parent),
        ui(new Ui::MainWindow)
    {
        ui->setupUi(this);
    
        db = QSqlDatabase::addDatabase("QSQLITE" , "CONNECTION NAME");
        db.setDatabaseName(":memory:");
        if(!db.open())
        {
            qDebug() << "Can't create in-memory Database!";
        }
        else
        {
            qDebug() << "In-memory Successfully created!";
            QSqlQuery query;
            
            if (!query.exec("CREATE TABLE employees (ID INTEGER, name TEXT, phone TEXT, address TEXT)"))
            {
                qDebug() << "Can't create table!";
                return;
            }
            if (!query.exec("INSERT INTO employees (ID, name, phone, address) VALUES (201, 'Bob', '5555-5555', 'Antarctica')"))
            {
                qDebug() << "Can't insert record!";
                return;
            }

            qDebug() << "Database filling completed!";
            if(!query.exec("SELECT name , phone , address FROM employees WHERE ID = 201"))
            {
                qDebug() << "Can't Execute Query !";
                return;
            }
            qDebug() << "Query Executed Successfully !";
            while(query.next())
            {
                qDebug() << "Employee Name : " << query.value(0).toString();
                qDebug() << "Employee Phone Number : " << query.value(1).toString();
                qDebug() << "Employee Address : " << query.value(1).toString();
            }
        }
    }
    
    MainWindow::~MainWindow()
    {
        delete ui;
    }



## Remove Database connection correctly
If we want to remove some database connection from the list of database connections.
we need to use `QSqlDatabase::removeDatabase()`,however it's a static function and the way it work is a little wired.

    // WRONG WAY
      QSqlDatabase db = QSqlDatabase::database("sales");
      QSqlQuery query("SELECT NAME, DOB FROM EMPLOYEES", db);
      QSqlDatabase::removeDatabase("sales"); // will output a warning
    
      // "db" is now a dangling invalid database connection,
      // "query" contains an invalid result set

The correct way that Qt Document suggest us is below.

      {
          QSqlDatabase db = QSqlDatabase::database("sales");
          QSqlQuery query("SELECT NAME, DOB FROM EMPLOYEES", db);
      }
      // Both "db" and "query" are destroyed because they are out of scope
      QSqlDatabase::removeDatabase("sales"); // correct

