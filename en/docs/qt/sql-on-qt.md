---
title: "SQL on Qt"
slug: "sql-on-qt"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Basic connection and query
The [QSqlDatabase][1] class provides an interface for accessing a database through a connection. An instance of QSqlDatabase represents the connection. The connection provides access to the database via one of the supported 
database drivers. Make sure to Add

    QT += SQL 
    
in the .pro file. Assume an SQL DB named TestDB with a countryTable that contines the next column:

    | country |
    -----------
    | USA     |

    
In order to query and get sql data from TestDB: 
    
    #include <QtGui>
    #include <QtSql>
     
    int main(int argc, char *argv[])
    {
        QCoreApplication app(argc, argv);

        QSqlDatabase db = QSqlDatabase::addDatabase("QPSQL"); // Will use the driver referred to by "QPSQL" (PostgreSQL Driver) 
        db.setHostName("TestHost");
        db.setDatabaseName("TestDB");
        db.setUserName("Foo");
        db.setPassword("FooPass");
        
        bool ok = db.open();
        if(ok)
        {
            QSqlQuery query("SELECT country FROM countryTable");
            while (query.next())
            {
              QString country = query.value(0).toString();
              qWarning() << country; // Prints "USA"
            }
        }

        return app.exec();
    }



[1]:http://doc.qt.io/qt-5/qsqldatabase.html

## Qt SQL query parameters
It's often convenient to separate the SQL query from the actual values. This can be done using placeholders. 
Qt supports two placeholder syntaxes: named binding and positional binding.


named binding:

    QSqlQuery query;
    query.prepare("INSERT INTO employee (id, name, salary) VALUES (:id, :name, :salary)");
    query.bindValue(":id", 1001);
    query.bindValue(":name", "Thad Beaumont");
    query.bindValue(":salary", 65000);
    query.exec();
        
positional binding:

    QSqlQuery query;
    query.prepare("INSERT INTO employee (id, name, salary) VALUES (?, ?, ?)");
    query.addBindValue(1001);
    query.addBindValue("Thad Beaumont");
    query.addBindValue(65000);
    query.exec();

    
Note that before calling `bindValue()` or `addBindValue()` you need to call [QSqlQuery][1]::prepare() once. 

[1]:http://doc.qt.io/qt-4.8/qsqlquery.html

## MS SQL Server Database Connection using QODBC
When trying to open a Database Connection with QODBC please ensure

 - You have QODBC driver available
 - Your server has an ODBC interface and is enabled to (this depends on your ODBC driver installations)
 - use shared memory access, TCP/IP connections or named pipe connection.

All connections only require the DatabaseName to be set by calling [QSqlDatabase][1]::setDatabaseName.

----


**Open Connection using shared memory access**

For this option to work you will need to have access to memory of the machine and must have permissions to access shared memory. For using a shared memory connection it is required to set lpc: in front of the Server string. Connection using the SQL Server Native Client 11 is made using these steps:


    QString connectString = "Driver={SQL Server Native Client 11.0};";                     // Driver is now {SQL Server Native Client 11.0}
    connectString.append("Server=lpc:"+QHostInfo::localHostName()+"\\SQLINSTANCENAME;");   // Hostname,SQL-Server Instance
    connectString.append("Database=SQLDBSCHEMA;");  // Schema
    connectString.append("Uid=SQLUSER;");           // User
    connectString.append("Pwd=SQLPASS;");           // Pass
    db.setDatabaseName(connectString);

    if(db.open())
    {
        ui->statusBar->showMessage("Connected");
    }
    else
    {
        ui->statusBar->showMessage("Not Connected");
    }

-------    
**Open Connection using Named Pipe**

This option requires your ODBC Connection to have a full DSN. The Server string is setup by using the Windows Computername and the Instancename of the SQL Server. The example connection will be opened using SQL Server Native Client 10.0


    QString connectString = "Driver={SQL Server Native Client 10.0};"; // Driver can also be {SQL Server Native Client 11.0}
    connectString.append("Server=SERVERHOSTNAME\\SQLINSTANCENAME;");   // Hostname,SQL-Server Instance
    connectString.append("Database=SQLDBSCHEMA;");  // Schema
    connectString.append("Uid=SQLUSER;");           // User
    connectString.append("Pwd=SQLPASS;");           // Pass
    db.setDatabaseName(connectString);

    if(db.open())
    {
        ui->statusBar->showMessage("Connected");
    }
    else
    {
        ui->statusBar->showMessage("Not Connected");
    }

----
**Open Connection using TCP/IP**

For opening a TCP/IP connection the server should be configured to allow connections on a fixed port, otherwise you will first have to query for the currently active port. In this example we have a fixed port at 5171. You can find an example for setting up the server to allow connections on a fixed port at [1]. For open a connection using TCP/IP use a tuple of the servers IP and Port:


    QString connectString = "Driver={SQL Server};"; // Driver is now {SQL Server}
    connectString.append("Server=10.1.1.15,5171;"); // IP,Port
    connectString.append("Database=SQLDBSCHEMA;");  // Schema
    connectString.append("Uid=SQLUSER;");           // User
    connectString.append("Pwd=SQLPASS;");           // Pass
    db.setDatabaseName(connectString);
    
    if(db.open())
    {
        ui->statusBar->showMessage("Connected");
    }
    else
    {
        ui->statusBar->showMessage("Not Connected");
    }

[1]:http://doc.qt.io/qt-5/qsqldatabase.html

