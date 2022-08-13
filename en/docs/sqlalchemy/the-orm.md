---
title: "The ORM"
slug: "the-orm"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

The SQLAlchemy ORM is built on top of [SQLAlchemy Core][1]. For example, although model classes use `Column` objects, they are part of the core and more relevant documentation will be found there.

The main parts of the ORM are the [session][2], query, and mapped classes (typically using the declarative extension in modern SQLAlchemy.)


  [1]: https://www.wikiod.com/sqlalchemy/sqlalchemy-core
  [2]: https://www.wikiod.com/sqlalchemy/the-session

## Converting a query result to dict
First the setup for the example:

    import datetime as dt
    from sqlalchemy import Column, Date, Integer, Text, create_engine, inspect
    from sqlalchemy.orm import sessionmaker
    from sqlalchemy.ext.declarative import declarative_base
    
    Base = declarative_base()
    Session = sessionmaker()
    
    class User(Base):
        __tablename__ = 'users'
    
        id = Column(Integer, primary_key=True)
        name = Column(Text, nullable=False)
        birthday = Column(Date)
    
    engine = create_engine('sqlite://')
    Base.metadata.create_all(bind=engine)
    Session.configure(bind=engine)
    
    session = Session()
    session.add(User(name='Alice', birthday=dt.date(1990, 1, 1)))
    session.commit()
    
If you're querying columns individually, the row is a `KeyedTuple` which has an `_asdict` method. The method name starts with a single underscore, to match the [`namedtuple`](https://docs.python.org/3/library/collections.html#collections.namedtuple) API (it's not private!).

    query = session.query(User.name, User.birthday)
    for row in query:
        print(row._asdict())

When using the ORM to retrieve objects, this is not available by default. The SQLAlchemy [inspection system](http://docs.sqlalchemy.org/en/latest/core/inspection.html) should be used.
    
    def object_as_dict(obj):
        return {c.key: getattr(obj, c.key)
                for c in inspect(obj).mapper.column_attrs}
    
    query = session.query(User)
    for user in query:
        print(object_as_dict(user))

Here, we created a function to do the conversion, but one option would be to add a method to the base class.

Instead of using `declarative_base` as above, you can create it from your own class:

    from sqlalchemy.ext.declarative import as_declarative

    @as_declarative()
    class Base:
        def _asdict(self):
            return {c.key: getattr(self, c.key)
                    for c in inspect(self).mapper.column_attrs}

## Filtering
Given the following model

    class User(Base):
        __tablename__ = 'users'

        id = Column(Integer, primary_key=True)
        name = Column(Text, nullable=False)
        birthday = Column(Date)

You can filter columns in the query:

    import datetime as dt
    session.query(User).filter(User.name == 'Bob')
    session.query(User).filter(User.birthday < dt.date(2000, 1, 1))

For the first case, there is a shortcut:

    session.query(User).filter_by(name='Bob')

Filters can be composed using an AND relation by chaining the `filter` method:

    (session.query(User).filter(User.name.like('B%'))
                        .filter(User.birthday < dt.date(2000, 1, 1)))

Or more flexibly, using the overloaded bitwise operators `&` and `|`:

    session.query(User).filter((User.name == 'Bob') | (User.name == 'George'))

Don't forget the inner parentheses to deal with operator precedence.

## Order By
Given a basic model:

    class SpreadsheetCells(Base):
        __tablename__ = 'spreadsheet_cells'

        id = Column(Integer, primary_key=True)
        y_index = Column(Integer)
        x_index = Column(Integer)

You can retrieve an ordered list by chaining the `order_by` method.

    query = session.query(SpreadsheetCells).order_by(SpreadsheetCells.y_index)

This could be chained on after a `filter`,

    query = session.query(...).filter(...).order_by(...)

or to further compose an existing query.

    query = session.query(...).filter(...)
    ordered_query = query.order_by(...)

You can also determine the sort direction in one of two ways:

1. Accessing the field properties `asc` and `dsc`:


    query.order_by(SpreadsheetCells.y_index.desc()) # desc
    query.order_by(SpreadsheetCells.y_index.asc()) # asc

2. Using the asc and desc module functions:


    from sqlalchemy import asc, desc
    
    query.order_by(desc(SpreadsheetCells.y_index)) # desc
    query.order_by(asc(SpreadsheetCells.y_index)) # asc

        
    

## Accessing query results
Once you have a query, you can do more with it than just iterating the results in a for loop.

Setup:

```
from datetime import date

class User(Base):
    __tablename__ = 'users'

    id = Column(Integer, primary_key=True)
    name = Column(Text, nullable=False)
    birthday = Column(Date)

# Find users who are older than a cutoff.
query = session.query(User).filter(User.birthday < date(1995, 3, 3))
```

To return the results as a list, use `all()`:

    reslist = query.all() # all results loaded in memory
    nrows = len(reslist)

You can get a count using `count()`:

    nrows = query.count()

To get only the first result, use `first()`. This is most useful in combination with `order_by()`.

    oldest_user = query.order_by(User.birthday).first()

For queries that should return only one row, use `one()`:

    bob = session.query(User).filter(User.name == 'Bob').one()

This raises an exception if the query returns multiple rows or if it returns none. If the row might not exist yet, use `one_or_none()`:

    bob = session.query(User).filter(User.name == 'Bob').one_or_none()
    if bob is None:
        create_bob()

This will still raise an exception if multiple rows have the name 'Bob'.

