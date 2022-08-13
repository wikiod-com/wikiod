---
title: "Static Files"
slug: "static-files"
draft: false
images: []
weight: 9932
type: docs
toc: true
---

## Using Static Files
Web applications often require static files like CSS or JavaScript files. To use static files in a Flask application, create a folder called `static` in your package or next to your module and it will be available at `/static` on the application. 

An example project structure for using templates is as follows:

    MyApplication/
        /static/
            /style.css
            /script.js
        /templates/
            /index.html
        /app.py

app.py is a basic example of Flask with template rendering. 

    from flask import Flask, render_template
    
    app = Flask(__name__)
    
    @app.route('/')
    def index():
        return render_template('index.html')
    

To use the static CSS and JavaScript file in the template index.html, we need to use the special 'static' endpoint name:

    {{url_for('static', filename = 'style.css')}}

So, **index.html** may contain:

    <html>
        <head>
            <title>Static File</title>
            <link href="{{url_for('static', filename = 'style.css')}}" rel="stylesheet">
            <script src="{{url_for('static', filename = 'script.js')}}"></script>
        </head>
        <body>
            <h3>Hello World!</h3>
        </body>
    </html>


After running app.py we will see the webpage in http://localhost:5000/.

## Static Files in Production (served by frontend webserver)
Flask's built-in webserver is able to serve static assets, and this works fine for development. However, for production deployments that are using something like uWSGI or Gunicorn to serve the Flask application, the task of serving static files is one that is typically offloaded to the frontend webserver (Nginx, Apache, etc.). This is a small/easy task with smaller apps, especially when all of the static assets are in one folder; for larger apps though, and/or ones that are using Flask plugin(s) that provide static assets, then it can become difficult to remember the locations of all of those files, and to manually copy/collect them into one directory. This document shows how to use the [Flask-Collect plugin](https://github.com/klen/Flask-Collect) to simplify that task.

Note that the focus of this documentation is on the collection of static assets. To illustrate that functionality, this example uses the Flask-Bootstrap plugin, which is one that provides static assets. It also uses the Flask-Script plugin, which is used to simplify the process of creating command-line tasks. Neither of these plugins are critical to this document, they are just in use here to demonstrate the functionality. If you choose not to use Flask-Script, you will want to review the [Flask-Collect docs for alternate ways to call the `collect` command](https://github.com/klen/Flask-Collect#id10).

Also note that configuration of your frontend webserver to serve these static assets is outside of the scope of this doc, you'll want to check out some examples using [Nginx](https://www.digitalocean.com/community/tutorials/how-to-deploy-flask-web-applications-using-uwsgi-behind-nginx-on-centos-6-4) and [Apache](http://stackoverflow.com/questions/24739925/serving-static-files-through-apache) for more info. Suffice it to say that you'll be aliasing URLs that start with "/static" to the centralized directory that Flask-Collect will create for you in this example.

The app is structured as follows:

    /manage.py - The app management script, used to run the app, and to collect static assets
    /app/ - this folder contains the files that are specific to our app
        | - __init__.py - Contains the create_app function
        | - static/ - this folder contains the static files for our app.
            | css/styles.css - custom styles for our app (we will leave this file empty)
            | js/main.js - custom js for our app (we will leave this file empty)
        | - templates/index.html - a simple page that extends the Flask-Bootstrap template


1. First, create your virtual environment and install the required packages:
   (your-virtualenv) $ pip install flask flask-script flask-bootstrap flask-collect

2. Establish the file structure described above:

    $ touch manage.py; mkdir -p app/{static/{css,js},templates}; touch app/{__init__.py,static/{css/styles.css,js/main.js}}

3. Establish the contents for the `manage.py`, `app/__init__.py`, and `app/templates/index.html` files:


    # manage.py
    #!/usr/bin/env python
    import os
    from flask_script import Manager, Server
    from flask import current_app
    from flask_collect import Collect
    from app import create_app

    class Config(object):
       # CRITICAL CONFIG VALUE: This tells Flask-Collect where to put our static files!
       # Standard practice is to use a folder named "static" that resides in the top-level of the project directory.
       # You are not bound to this location, however; you may use basically any directory that you wish.
       COLLECT_STATIC_ROOT = os.path.dirname(__file__) + '/static'
       COLLECT_STORAGE = 'flask_collect.storage.file'

    app = create_app(Config)

    manager = Manager(app)
    manager.add_command('runserver', Server(host='127.0.0.1', port=5000))

    collect = Collect()
    collect.init_app(app)

    @manager.command
    def collect():
      """Collect static from blueprints. Workaround for issue: https://github.com/klen/Flask-Collect/issues/22"""
      return current_app.extensions['collect'].collect()

    if __name__ == "__main__":
       manager.run()

----

    # app/__init__.py
    from flask import Flask, render_template
    from flask_collect import Collect
    from flask_bootstrap import Bootstrap

    def create_app(config):
      app = Flask(__name__)
      app.config.from_object(config)

      Bootstrap(app)
      Collect(app)

      @app.route('/')
      def home():
        return render_template('index.html')

      return app

----

    # app/templates/index.html
    {% extends "bootstrap/base.html" %}
    {% block title %}This is an example page{% endblock %}

    {% block navbar %}
    <div class="navbar navbar-fixed-top">
      <!-- ... -->
    </div>
    {% endblock %}

    {% block content %}
      <h1>Hello, Bootstrap</h1>
    {% endblock %}

4. With those files in place, you can now use the management script to run the app:


    $ ./manage.py runserver # visit http://localhost:5000 to verify that the app works correctly.

5. Now, to collect your static assets for the first time. Before doing this, it's worth noting again that you should NOT have a `static/` folder in the top-level of your app; this is where Flask-Collect is going to place all of the static files that it's going to be collecting from your app and the various plugins you might be using. If you *do* have a `static/` folder in the top level of your app, you should delete it entirely before proceeding, as starting with a clean slate is a critical part of witnessing/understanding what Flask-Collect does. Note that this instruction isn't applicable for day-to-day usage, it is simply to illustrate the fact that Flask-Collect is going to create this directory for you, and then it's going to place a bunch of files in there.

With that said, you can run the following command to collect your static assets:

    $ ./manage.py collect

After doing so, you should see that Flask-Collect has created this top-level `static/` folder, and it contains the following files:

    $ find ./static -type f # execute this from the top-level directory of your app, same dir that contains the manage.py script
    static/bootstrap/css/bootstrap-theme.css
    static/bootstrap/css/bootstrap-theme.css.map
    static/bootstrap/css/bootstrap-theme.min.css
    static/bootstrap/css/bootstrap.css
    static/bootstrap/css/bootstrap.css.map
    static/bootstrap/css/bootstrap.min.css
    static/bootstrap/fonts/glyphicons-halflings-regular.eot
    static/bootstrap/fonts/glyphicons-halflings-regular.svg
    static/bootstrap/fonts/glyphicons-halflings-regular.ttf
    static/bootstrap/fonts/glyphicons-halflings-regular.woff
    static/bootstrap/fonts/glyphicons-halflings-regular.woff2
    static/bootstrap/jquery.js
    static/bootstrap/jquery.min.js
    static/bootstrap/jquery.min.map
    static/bootstrap/js/bootstrap.js
    static/bootstrap/js/bootstrap.min.js
    static/bootstrap/js/npm.js
    static/css/styles.css
    static/js/main.js

And that's it: use the `collect` command whenever you make edits to your app's CSS or JavaScript, or when you've updated a Flask plugin that provides static assets (like Flask-Bootstrap in this example).

