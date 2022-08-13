---
title: "SQLAlchemy Core"
slug: "sqlalchemy-core"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Converting result to dict
In SQLAlchemy core, the result is `RowProxy`. In cases where you want an explicit dictionary, you can call `dict(row)`.

First the setup for the example:

    import datetime as dt
    from sqlalchemy import (
        Column, Date, Integer, MetaData, Table, Text, create_engine, select)
    
    metadata = MetaData()
    users = Table(
        'users', metadata,
        Column('id', Integer, primary_key=True),
        Column('name', Text, nullable=False),
        Column('birthday', Date),
    )
    
    engine = create_engine('sqlite://')
    metadata.create_all(bind=engine)
    
    engine.execute(users.insert(), name='Alice', birthday=dt.date(1990, 1, 1))

Then to create a dictionary from a result row:
    
    with engine.connect() as conn:
        result = conn.execute(users.select())
        for row in result:
            print(dict(row))
    
        result = conn.execute(select([users.c.name, users.c.birthday]))
        for row in result:
            print(dict(row))


