---
title: "Sessions"
slug: "sessions"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Sessions are derived from dictionaries which means they will work with most common dictionary methods.

## Using the sessions object within a view
First, ensure you have imported sessions from flask

    from flask import session

To use session, a Flask application needs a defined **SECRET_KEY**.

    app = Flask(__name__)
    app.secret_key = 'app secret key'

Sessions are implemented by default using a **cookie** signed with the secret key. This ensures that the data is not modified except by your application, so make sure to pick a secure one! A browser will send the cookie back to your application along with each request, enabling the persistence of data across requests.

To use a session you just reference the object (It will behave like a dictionary)

    @app.route('/')
    def index():
        if 'counter' in session:
            session['counter'] += 1
        else:
            session['counter'] = 1
        return 'Counter: '+str(session['counter'])

To release a session variable use **pop()** method.

    session.pop('counter', None)

**Example Code:**

    from flask import Flask, session
    
    app = Flask(__name__)
    app.secret_key = 'app secret key'
    
    @app.route('/')
    def index():
        if 'counter' in session:
            session['counter'] += 1
        else:
            session['counter'] = 1
        return 'Counter: '+str(session['counter'])
    
    if __name__ == '__main__':
        app.debug = True
        app.run()

