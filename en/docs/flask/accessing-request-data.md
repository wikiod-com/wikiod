---
title: "Accessing request data"
slug: "accessing-request-data"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

When working with an web application it's sometimes important to access data included in the request, beyond the URL.

In Flask this is stored under the global **request** object, which you can access in your code via `from flask import request`.

## Combined form and query string
Flask also allows access to a CombinedMultiDict that gives access to both the `request.form` and `request.args` attributes under one variable.

This example pulls data from a form field `name` submitted along with the `echo` field in the query string.

**Flask Example**:

```python
from flask import Flask, request

app = Flask(import_name=__name__)


@app.route("/echo", methods=["POST"])
def echo():

    name = request.values.get("name", "")
    to_echo = request.values.get("echo", "")

    response = "Hey there {}! You said {}".format(name, to_echo)

    return response

app.run()

```

## Accessing query string
The query string is the part of a request following the URL, preceded by a `?` mark.

Example: `https://encrypted.google.com/search`**`?hl=en&q=stack%20overflow`**

For this example, we are making a simple echo webserver that echos back everything submitted to it via the `echo` field in `GET` requests.

Example: `localhost:5000/echo`**`?echo=echo+this+back+to+me`**

**Flask Example**:

```python
from flask import Flask, request

app = Flask(import_name=__name__)

@app.route("/echo")
def echo():

    to_echo = request.args.get("echo", "")
    response = "{}".format(to_echo)

    return response

if __name__ == "__main__":
    app.run()
```

## Accessing form fields
You can access the form data submitted via a `POST` or `PUT` request in Flask via the `request.form` attribute.

```python
from flask import Flask, request

app = Flask(import_name=__name__)


@app.route("/echo", methods=["POST"])
def echo():

    name = request.form.get("name", "")
    age = request.form.get("age", "")

    response = "Hey there {}! You said you are {} years old.".format(name, age)

    return response

app.run()
```

