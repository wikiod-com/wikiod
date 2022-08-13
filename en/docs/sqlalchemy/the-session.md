---
title: "The Session"
slug: "the-session"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

A session keeps track of ORM objects and their changes, manages transactions and is used to perform [queries](http://docs.sqlalchemy.org/en/latest/orm/query.html#sqlalchemy.orm.query.Query).

## Creating a Session
A [session](http://docs.sqlalchemy.org/en/latest/orm/session_basics.html) is usually obtained using [`sessionmaker`](http://docs.sqlalchemy.org/en/latest/orm/session_api.html#sqlalchemy.orm.session.sessionmaker), which creates a `Session` class unique to your application. Most commonly, the `Session` class is bound to an engine, allowing instances to use the engine implicitly.

    from sqlalchemy.orm import sessionmaker

    # Initial configuration arguments
    Session = sessionmaker(bind=engine)

The `engine` and `Session` should only be created once.
    
A session is an instance of the class we created:

    # This session is bound to provided engine
    session = Session()

[`Session.configure()`](http://docs.sqlalchemy.org/en/latest/orm/session_api.html#sqlalchemy.orm.session.sessionmaker.configure) can be used to configure the class later, e.g. application startup rather than import time.

    Session = sessionmaker()

    # later
    Session.configure(bind=engine)

Arguments passed to `Session` directly override the arguments passed to `sessionmaker`.

    session_bound_to_engine2 = Session(bind=engine2)


## Adding Instances
New or detached objects may be added to the session using [`add()`](http://docs.sqlalchemy.org/en/latest/orm/session_api.html#sqlalchemy.orm.session.Session.add):

    session.add(obj)

A sequence of objects may be added using [`add_all()`](http://docs.sqlalchemy.org/en/latest/orm/session_api.html#sqlalchemy.orm.session.Session.add_all):

    session.add_all([obj1, obj2, obj3])

An INSERT will be emitted to the database during the next flush, which happens automatically. Changes are persisted when the session is committed.

## Deleting Instances
To delete persisted objects use [`delete()`](http://docs.sqlalchemy.org/en/latest/orm/session_api.html#sqlalchemy.orm.session.Session.delete):

    session.delete(obj)

Actual deletion from the database will happen on next [flush](http://docs.sqlalchemy.org/en/latest/orm/session_basics.html#flushing).

