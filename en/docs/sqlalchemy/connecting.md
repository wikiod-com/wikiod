---
title: "Connecting"
slug: "connecting"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Engine
The engine is used to connect to different databases using a connection URL:

    from sqlalchemy import create_engine

    engine = create_engine('postgresql://user:pass@localhost/test')

Note, however, that the engine does not actually establish a connection until it is first used.

The engine automatically creates a connection pool, but opens new connections lazily (i.e. SQLAlchemy won't open 5 connections if you only ask for one).

## Using a Connection
You can open a connection (i.e. request one from the pool) using a context manager:

    with engine.connect() as conn:
        result = conn.execute('SELECT price FROM products')
        for row in result:
            print('Price:', row['price'])

Or without, but it must be closed manually:

    conn = engine.connect()
    result = conn.execute('SELECT price FROM products')
    for row in result:
        print('Price:', row['price'])
    conn.close()


## Implicit Execution
If you only want to execute a single statement, you can use the engine directly and it will open and close the connection for you:

    result = engine.execute('SELECT price FROM products')
    for row in result:
        print('Price:', row['price'])

## Transactions
You can use `engine.begin` to open a connection and begin a transaction that will be rolled back if an exception is raised, or committed otherwise. This is an implicit way of using a transaction, since you don't have the option of rolling back manually.

    with engine.begin() as conn:
        conn.execute(products.insert(), price=15)


More explicitly, you can begin a transaction using a connection:

    with conn.begin() as trans:
        conn.execute(products.insert(), price=15)

Note that we still call `execute` on the connection. As before, this transaction will be committed or rolled back if an exception is raised, but we also have access to the transaction, allowing us to rollback manually using `trans.rollback()`.

This could be done more explicitly like so:

    trans = conn.begin()
    try:
        conn.execute(products.insert(), price=15)
        trans.commit()
    except:
        trans.rollback()
        raise

