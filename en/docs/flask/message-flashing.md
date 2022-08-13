---
title: "Message Flashing"
slug: "message-flashing"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Flashing message to the template by `flash()` function.

## Syntax
 - flash(message, category='message')
 - flash('hello, world!')
 - flash('This is a warning message', 'warning')

## Parameters
| message  | the message to be flashed.   |
| -------- | --------------------------- |
| category | the message's category, the default is `message`. |

 - [Template
   Inheritance](http://flask.pocoo.org/docs/0.12/patterns/templateinheritance/)
 - [API](http://flask.pocoo.org/docs/0.12/api/#message-flashing)

## Simple Message Flashing
Set `SECKET_KEY`, then flashing message in view function:

    from flask import Flask, flash, render_template

    app = Flask(__name__)
    app.secret_key = 'some_secret'

    @app.route('/')
    def index():
        flash('Hello, I'm a message.')
        return render_template('index.html')

Then render the messages in `layout.html` (which the `index.html` extended from):

    {% with messages = get_flashed_messages() %}
      {% if messages %}
        <ul class=flashes>
        {% for message in messages %}
          <li>{{ message }}</li>
        {% endfor %}
        </ul>
      {% endif %}
    {% endwith %}
    {% block body %}{% endblock %}

## Flashing With Categories
Set second argument when use `flash()` in view function:

    flash('Something was wrong!', 'error')

In the template, set `with_categories=true` in `get_flashed_messages()`, then you get a list of tuples in the form of `(message, category)`, so you can use category as a HTML class.

    {% with messages = get_flashed_messages(with_categories=true) %}
      {% if messages %}
        <ul class=flashes>
        {% for category, message in messages %}
          <li class="{{ category }}">{{ message }}</li>
        {% endfor %}
        </ul>
      {% endif %}
    {% endwith %}

