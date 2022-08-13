---
title: "Getting started with jinja2"
slug: "getting-started-with-jinja2"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Jinja2 installation and setup
Install the dependencies:

`pip install jinja2`

Install a framework:

`pip install flask`

Create the following structure

    ├── run.py
    └── templates

Put a file `template.html` in the templates directory. The file can contain a jinja 2 variable named `my_string`. 

    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <title>Title</title>
    </head>
    <body>
    {{my_string}}
    </body>
    </html>

Open the file `run.py` and put in the following contents. 

    from flask import Flask, render_template
    app = Flask(__name__)
    
    
    @app.route("/")
    def template_test():
        return render_template('template.html', my_string="Wheeeee!", my_list=[0,1,2,3,4,5])
    
    
    if __name__ == '__main__':
        app.run(debug=True)

Now you can run your webapp using `python run.py` and the output will appear with your local host `http://localhost:5000`



