---
title: "Authorization and authentication"
slug: "authorization-and-authentication"
draft: false
images: []
weight: 9961
type: docs
toc: true
---

## Using flask-login extension
One of the simpler ways of implementing an authorization system is using the [flask-login][1] extension. The project's website contains a detailed and well-written quickstart, a shorter version of which is available in this example.

# General idea
The extension exposes a set of functions used for:
* logging users in
* logging users out
* checking if a user is logged in or not and finding out which user is that

What it doesn't do and what you have to do on your own:
* doesn't provide a way of storing the users, for example in the database
* doesn't provide a way of checking user's credentials, for example username and password

Below there is a minimal set of steps needed to get everything working.

**I would recommend to place all auth related code in a separate module or package, for example `auth.py`. That way you can create the necessary classes, objects or custom functions separately.**


# Create a `LoginManager`
 The extension uses a [`LoginManager`][2] class which has to be registered on your [`Flask`][3] application object.

    from flask_login import LoginManager
    login_manager = LoginManager()
    login_manager.init_app(app) # app is a Flask object

As mentioned earlier `LoginManager` can for example be a global variable in a separate file or package. Then it can be imported in the file in which the `Flask` object is created or in your application factory function and initialized.

# Specify a callback used for loading users
A users will normally be loaded from a database. The callback must return an object which represents a user corresponding to the provided ID. It should return `None` if the ID is not valid.

    @login_manager.user_loader
    def load_user(user_id):
        return User.get(user_id) # Fetch the user from the database

This can be done directly below creating your `LoginManager`.

# A class representing your user
As mentioned the `user_loader` callback has to return an object which represent a user. What does that mean exactly? That object can for example be a wrapper around user objects stored in your database or simply directly a model from your database. That object has to implement the following methods and properties. That means that if the callback returns your database model you need to ensure that the mentioned properties and methods are added to your model.

* `is_authenticated`

    This property should return `True` if the user is authenticated, i.e. they have provided valid credentials. You will want to ensure that the objects which represent your users returned by the `user_loader` callback return `True` for that method.

* `is_active`

    This property should return True if this is an active user - in addition to being authenticated, they also have activated their account, not been suspended, or any condition your application has for rejecting an account. Inactive accounts may not log in. If you don't have such a mechanism present return `True` from this method.

* `is_anonymous`

    This property should return True if this is an anonymous user. That means that your user object returned by the `user_loader` callback should return `True`.

* `get_id()`

    This method must return a unicode that uniquely identifies this user, and can be used to load the user from the `user_loader` callback. Note that this must be a unicode - if the ID is natively an int or some other type, you will need to convert it to unicode. If the `user_loader` callback returns objects from the database this method will most likely return the database ID of this particular user. The same ID should of course cause the `user_loader` callback to return the same user later on.

If you want to make things easier for yourself (**it is in fact recommended) you can inherit from [`UserMixin`][4] in the object returned by the `user_loader` callback (presumably a database model). You can see how those methods and properties are implemented by default in this mixin     [here][5].

# Logging the users in
The extension leaves the validation of the username and  password entered by the user to you. In fact the extension doesn't care if you use a username and password combo or other mechanism. This is an example for logging users in using username and password.

    @app.route('/login', methods=['GET', 'POST'])
    def login():
        # Here we use a class of some kind to represent and validate our
        # client-side form data. For example, WTForms is a library that will
        # handle this for us, and we use a custom LoginForm to validate.
        form = LoginForm()
        if form.validate_on_submit():
            # Login and validate the user.
            # user should be an instance of your `User` class
            login_user(user)
    
            flask.flash('Logged in successfully.')
    
            next = flask.request.args.get('next')
            # is_safe_url should check if the url is safe for redirects.
            # See http://flask.pocoo.org/snippets/62/ for an example.
            if not is_safe_url(next):
                return flask.abort(400)
    
            return flask.redirect(next or flask.url_for('index'))
        return flask.render_template('login.html', form=form)

In general logging users in is accomplished by calling [login_user][6] and passing an instance of an object representing your user mentioned earlier to it. As shown this will usually happen after retrieving the user from the database and validating his credentials, however the user object just magically appears in this example.

# I have logged in a user, what now?

The object returned by the `user_loader` callback can be accessed in multiple ways.

* In templates:

    The extension automatically injects it under the name `current_user` using a template context processor. To disable that behaviour and use your custom processor set `add_context_processor=False` in your `LoginManager` constructor.

        {% if current_user.is_authenticated %}
          Hi {{ current_user.name }}!
        {% endif %}

* In Python code:  

    The extension provides a request-bound object called [`current_user`][7].

        from flask_login import current_user    

        @app.route("/hello")
        def hello():
            # Assuming that there is a name property on your user object
            # returned by the callback
            if current_user.is_authenticated:
                return 'Hello %s!' % current_user.name 
            else:
                return 'You are not logged in!'

* Limiting access quickly using a decorator
A [`login_required`][8] decorator can be used to limit access quickly.

        from flask_login import login_required

        @app.route("/settings")
        @login_required
        def settings():
            pass

# Logging users out
Users can be logged out by calling [`logout_user()`][9]. It appears that it is safe to do so even if the user is not logged in so the `@login_required` decorator can most likely be ommited.

    @app.route("/logout")
    @login_required
    def logout():
        logout_user()
        return redirect(somewhere)

# What happens if a user is not logged in and I access the `current_user` object?
By defult an [AnonymousUserMixin][10] is returned:
* `is_active` and `is_authenticated` are `False`
* `is_anonymous` is `True`
* `get_id()` returns `None`

To use a different object for anonymous users provide a callable (either a class or factory function) that creates anonymous users to your `LoginManager` with:

    login_manager.anonymous_user = MyAnonymousUser


# What next?
This concludes the basic introduction to the extension. To learn more about configuration and additional options it is highly recommended to [read the official guide][1].


  [1]: https://flask-login.readthedocs.io/en/latest/
  [2]: https://flask-login.readthedocs.io/en/latest/#flask_login.LoginManager
  [3]: http://flask.pocoo.org/docs/dev/api/#application-object
  [4]: https://flask-login.readthedocs.io/en/latest/#flask_login.UserMixin
  [5]: https://flask-login.readthedocs.io/en/latest/_modules/flask_login/mixins.html#UserMixin
  [6]: https://flask-login.readthedocs.io/en/latest/#flask_login.login_user
  [7]: https://flask-login.readthedocs.io/en/latest/#flask_login.current_user
  [8]: https://flask-login.readthedocs.io/en/latest/#flask_login.login_required
  [9]: https://flask-login.readthedocs.io/en/latest/#flask_login.logout_user
  [10]: https://flask-login.readthedocs.io/en/latest/#flask_login.AnonymousUserMixin

## Timing  out the login session
Its good practice to time out logged in session after specific time, you can achieve that with Flask-Login. 

    from flask import Flask, session
    from datetime import timedelta
    from flask_login import LoginManager, login_require, login_user, logout_user

    # Create Flask application

    app = Flask(__name__) 

    # Define Flask-login configuration 

    login_mgr = LoginManager(app)
    login_mgr.login_view = 'login'
    login_mgr.refresh_view = 'relogin'
    login_mgr.needs_refresh_message = (u"Session timedout, please re-login")
    login_mgr.needs_refresh_message_category = "info"

    
    @app.before_request
    def before_request():
        session.permanent = True
        app.permanent_session_lifetime = timedelta(minutes=5)


Default session lifetime is 31 days, user need to specify the login refresh view in case of timeout.

    app.permanent_session_lifetime = timedelta(minutes=5)


Above line will force user to re-login every 5 minutes. 



