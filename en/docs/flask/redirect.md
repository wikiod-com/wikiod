---
title: "Redirect"
slug: "redirect"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Syntax
- redirect(location, code, Response)



## Parameters
| Parameter| Details |
| ------ | ------ |
| location | The location the response should redirect to. |
| code | (Optional) The redirect status code, 302 by default.  Supported codes are 301, 302, 303, 305, and 307.  |
| Response | (Optional) A Response class to use when instantiating a response. The default is werkzeug.wrappers.Response if unspecified. |

The location parameter must be a URL.  It can be input raw, such as 'http://www.webpage.com' or it can be built with the url_for() function.

## Passing along data
    # ... 
    # same as above

    @app.route('/welcome/<name>')
    def welcome(name):
        return render_template('main.html', name=name)

    @app.route('/login', methods=['GET', 'POST'])
    def login():
        if request.method == 'POST':
            # ...
            # check for valid login, assign username
            if valid:
                return redirect(url_for('main_page', name=username))
            else:
                return redirect(url_for('login_error'))
        else:
            return render_template('login.html')



## Simple example
    from flask import Flask, render_template, redirect, url_for

    app = Flask(__name__)

    @app.route('/')
    def main_page():
        return render_template('main.html')

    @app.route('/main')
    def go_to_main():
        return redirect(url_for('main_page'))



