---
title: "Flask-SQLAlchemy"
slug: "flask-sqlalchemy"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Flask-SQLAlchemy is a Flask extension that adds support for the popular Python object relational mapper(ORM) SQLAlchemy to Flask applications. It aims to simplify SQLAlchemy with Flask by providing some default implementations to common tasks.

## Installation and Initial Example
**Installation**

    pip install Flask-SQLAlchemy

**Simple Model**

    class User(db.Model):
        id = db.Column(db.Integer, primary_key=True)
        name = db.Column(db.String(80))
        email = db.Column(db.String(120), unique=True)

The code example above shows a simple Flask-SQLAlchemy model, we can add an optional tablename to the model declaration however it is often not necessary as Flask-SQLAlchemy will automatically use the class name as the table name during database creation.

Our class will inherit from the baseclass Model which is a configured declarative base hence there is no need for us to explicitly define the base as we would when using SQLAlchemy. 

**Reference**
        

 - Pypi URL: [https://pypi.python.org/pypi/Flask-SQLAlchemy][1]
- Documentation URL: [http://flask-sqlalchemy.pocoo.org/2.1/][1]


  [1]: http://flask-sqlalchemy.pocoo.org/2.1/

## Relationships: One to Many
        class User(db.Model):
            id = db.Column(db.Integer, primary_key=True)
            name = db.Column(db.String(80))
            email = db.Column(db.String(120), unique=True)
            posts = db.relationship('Post', backref='user')


        class Post(db.Model):
            id = db.Column(db.Integer, primary_key=True)
            content = db.Column(db.Text)
            user_id = db.Column(db.Integer, db.ForeignKey('user.id')

In this example we have two class the User class and the Post class, the User class will be our parent and the Post will be our post as only post can belong to one user but one user can have multiple posts. In order to achieve that we place a Foreign key on the child referencing the parent that is from our example we place a foreign key on Post class to reference the User class. We then use `relationship()` on the parent which we access via our SQLAlchemy object `db`. That then allows us to reference a collection of items represented by the Post class which is our child.

To create a bidirectional relationship we use`backref`, this will allow the child to reference the parent.

