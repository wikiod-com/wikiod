---
title: "Getting started with sqlalchemy"
slug: "getting-started-with-sqlalchemy"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello, World! (SQLAlchemy Core)
This example shows how to create a table, insert data, and select from the database using SQLAlchemy Core. For information re: the [SQLAlchemy ORM, see here][1].

First, we'll need to [connect][2] to our database.

    from sqlalchemy import create_engine

    engine = create_engine('sqlite://')

> The engine is the starting point for any SQLAlchemy application. It’s a “home base” for the actual database and its DBAPI, delivered to an SQLAlchemy application through a connection pool and a dialect, which describes how to talk to a specific kind of database/DBAPI combination. The Engine references both a dialect and a connection pool, which together interpret the DBAPI’s module functions as well as the behaviour of the database.

After creating our engine, we need to 
[define and create our tables][3].
    
    from sqlalchemy import Column, Integer, Text, MetaData, Table

    metadata = MetaData()
    messages = Table(
        'messages', metadata,
        Column('id', Integer, primary_key=True),
        Column('message', Text),
    )

    messages.create(bind=engine)
To futher explain the MetaData object, see the below from the docs:
>A collection of Table objects and their associated child objects is referred to as database metadata

>We define our tables all within a catalog called MetaData, using the Table construct, which resembles regular SQL CREATE TABLE statements.

Now that we have our tables defined and created, we can start inserting data! Inserting involves two steps. Composing the [insert][4] construct, and [executing][5] the final query.
    
    insert_message = messages.insert().values(message='Hello, World!')
    engine.execute(insert_message)

Now that we have data, we can use the [select][6] function to query our data. Column objects are available as named attributes of the c attribute on the Table object, making it easy to select columns directly. Executing this select statement returns a `ResultProxy` object which has access to a few methods, [fetchone()][7], [fetchall()][8], and [fetchmany()][7], all of which return a number of database rows queried in our select statement. 

    from sqlalchemy import select
    stmt = select([messages.c.message]) 
    message, = engine.execute(stmt).fetchone()
    print(message)

#

    Hello, World!

And that's it! See the [SQLAlchemy SQL Expressions Tutorial][9] for more examples and information.


  [1]: http://docs.sqlalchemy.org/en/latest/orm/tutorial.html
  [2]: http://docs.sqlalchemy.org/en/latest/core/tutorial.html#connecting
  [3]: http://docs.sqlalchemy.org/en/latest/core/tutorial.html#define-and-create-tables
  [4]: http://docs.sqlalchemy.org/en/latest/core/tutorial.html#insert-expressions
  [5]: http://docs.sqlalchemy.org/en/latest/core/tutorial.html#executing
  [6]: http://docs.sqlalchemy.org/en/latest/core/tutorial.html#selecting
  [7]: http://docs.sqlalchemy.org/en/latest/core/connections.html#sqlalchemy.engine.ResultProxy.fetchone
  [8]: http://docs.sqlalchemy.org/en/latest/core/connections.html#sqlalchemy.engine.ResultProxy.fetchall
  [9]: http://docs.sqlalchemy.org/en/latest/core/tutorial.html

## Hello, World! (SQLAlchemy ORM)
This example shows how to create a table, insert data, and select from the database using the **SQLAlchemy ORM**. For information re: [SQLAlchemy Core, see here][1].

First things first, we need to connect to our database, which is identical to how we would connect using SQLAlchemy Core (Core).

    from sqlalchemy import create_engine

    engine = create_engine('sqlite://')

After connecting and creating our engine, we need to define and create our tables. This is where the SQLAlchemy ORM language starts to differ greatly from Core. In ORM, the table creation and definition process begins by defining the tables and the classes we'll use to map to those tables. This process is done in one step in ORM, which SQLAlchemy calls the [Declarative][2] system.

    from sqlalchemy.ext.declarative import declarative_base
    Base = declarative_base()

Now that our base mapper is declared, we can subclass from it to build our [declarative mappings][3], or `models`.

    from sqlalchemy import Column, Integer, String
    
    class Message(Base):
        __tablename__ = 'messages'
        
        id = Column(Integer, primary_key=True)
        message = Column(String)

Using the declarative base class, we end up creating a `Table` and `Mapper` object. From the docs: 

> The Table object is a member of a larger collection known as MetaData. When using Declarative, this object is available using the .metadata attribute of our declarative base class.

With that in mind, to create all tables that do not yet exist, we can call the below command, which utilizes SQLAlchemy Core's MetaData registry.

    Base.metadata.create_all(engine)

Now that our tables are mapped and created, we can insert data! Inserting is done through the [creation of mapper instances][4]. 

    message = Message(message="Hello World!")
    message.message # 'Hello World!

At this point, all we have is an instance of message at the level of the ORM abstraction level, but nothing has been saved to the database yet. To do this, first we need to create a [session][5].

    from sqlalchemy.orm import sessionmaker
    
    Session = sessionmaker(bind=engine)
    session = Session()

This session object is our database handler. As per the SQLAlchemy docs:

> it retrieves a connection from a pool of connections maintained by the Engine, and holds onto it until we commit all changes and/or close the session object.

Now that we have our session, we can [add][6] our new message to the session and commit our changes to the database.

    session.add(message)
    session.commit()

Now that we have data, we can take advantage of the ORM query language to pull up our data.

    query = session.query(Message)
    instance = query.first()
    print (instance.message) # Hello World!

But thats just the beginning! There are much more features that can be used to compose queries, like `filter`, `order_by`, and much more. See the [SQLAlchemy ORM Tutorial][6] for more examples and information.


  [1]: http://docs.sqlalchemy.org/en/latest/core/tutorial.html
  [2]: http://docs.sqlalchemy.org/en/latest/orm/extensions/declarative/index.html
  [3]: http://docs.sqlalchemy.org/en/latest/orm/tutorial.html#declare-a-mapping
  [4]: http://docs.sqlalchemy.org/en/latest/orm/tutorial.html#create-an-instance-of-the-mapped-class
  [5]: http://docs.sqlalchemy.org/en/latest/orm/tutorial.html#creating-a-session
  [6]: http://docs.sqlalchemy.org/en/latest/orm/tutorial.html#adding-and-updating-objects

## Installation or Setup
    pip install sqlalchemy

For most common applications, particularly web applications, it is usually recommended that beginners consider using a supplementary library, such as `flask-sqlalchemy`.

    pip install flask-sqlalchemy

