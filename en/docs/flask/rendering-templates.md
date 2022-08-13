---
title: "Rendering Templates"
slug: "rendering-templates"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Syntax
- `render_template(template_name_or_list, **context)`

## render_template Usage
Flask lets you use templates for dynamic web page content. An example project structure for using templates is as follows:

    myproject/
        /app/
            /templates/
                /index.html
            /views.py

`views.py`:

    from flask import Flask, render_template


    app = Flask(__name__)
    
    @app.route("/")
    def index():
        pagetitle = "HomePage"
        return render_template("index.html",
                                mytitle=pagetitle,
                                mycontent="Hello World")

Note that you can pass dynamic content from your route handler to the template by appending key/value pairs to the render_templates function.  In the above example, the "pagetitle" and "mycontent" variables will be passed to the template for inclusion in the rendered page.  Include these variables in the template by enclosing them in double braces: `{{mytitle}}`

`index.html`:

    <html>
        <head>
            <title>{{ mytitle }}</title>
        </head>
        <body>
            <p>{{ mycontent }}</p>
        </body>
    </html>

When executed same as the first example, `http://localhost:5000/` will have the title "HomePage" and a paragraph with the content "Hello World".
    


  [1]: https://www.wikiod.com/flask/getting-started-with-flask#Hello World

