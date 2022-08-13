---
title: "Getting started with Flask"
slug: "getting-started-with-flask"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
Create `hello.py`:

    from flask import Flask

    app = Flask(__name__)
    

    @app.route('/')
    def hello():
        return 'Hello, World!'

Then run it with:

    export FLASK_APP=hello.py
    flask run
     * Running on http://localhost:5000/

Adding the code below will allow running it directly with `python hello.py`. 

    if __name__ == '__main__':
        app.run()

## Installation - Stable
Use pip to install Flask in a virtualenv.

    pip install flask

---

Step by step instructions for creating a virtualenv for your project:

    mkdir project && cd project
    python3 -m venv env
    # or `virtualenv env` for Python 2
    source env/bin/activate
    pip install flask

---

**Never** use `sudo pip install` unless you understand exactly what you're doing.  Keep your project in a local virtualenv, do not install to the system Python unless you are using the system package manager.

## Installation - Latest
If you want to use the latest code, you can install it from the repository.  While you potentially get new features and fixes, only numbered releases are officially supported.

    pip install https://github.com/pallets/flask/tarball/master


## Installation - Development
If you want to develop and contribute to the Flask project, clone the repository and install the code in development mode.

    git clone ssh://github.com/pallets/flask
    cd flask
    python3 -m venv env
    source env/bin/activate
    pip install -e .

---

There are some extra dependencies and tools to be aware of as well.

<h1>sphinx</h1>

Used to build the documentation.

    pip install sphinx
    cd docs
    make html
    firefox _build/html/index.html

<h1>py.test</h1>

Used to run the test suite.

    pip install pytest
    py.test tests

<h1>tox</h1>

Used to run the test suite against multiple Python versions.

    pip install tox
    tox

<sub>Note that tox only uses interpreters that are already installed, so if you don't have Python 3.3 installed on your path, it won't be tested.</sub>

