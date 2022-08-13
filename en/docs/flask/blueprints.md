---
title: "Blueprints"
slug: "blueprints"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

Blueprints are a powerful concept in Flask application development that allow for flask applications to be more modular and be able to follow multiple patterns. They make administration of very large Flask applications easier and as such can be used to scale Flask applications. You can reuse Blueprint applications however you cannot run a blueprint on its own as it has to be registered on your main application.

## A basic flask blueprints example
*A minimal Flask application looks something like this:*

    from flask import Flask
    app = Flask(__name__)
    
    @app.route("/")
    def index():
        return "Hello World!"

----------

*A large Flask application can separate one file into multiple files by `blueprints`.* 

**Purpose**

Make it easier for others to maintain the application.

**Folder Structure of Large Application**

    /app
        /templates
        /static
        /views
            __init__.py
            index.py
        app.py

**views/index.py**

    from flask import Blueprint, render_template
    
    index_blueprint = Blueprint('index', __name__)
    
    @index_blueprint.route("/")
    def index():
        return "Hello World!"

**app.py**

    from flask import Flask
    from views.index import index_blueprint
    
    application = Flask(__name__)
    application.register_blueprint(index_blueprint)

**Run application**

    $ export FLASK_APP=app.py
    $ flask run


