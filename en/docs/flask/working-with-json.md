---
title: "Working with JSON"
slug: "working-with-json"
draft: false
images: []
weight: 9956
type: docs
toc: true
---

## Receiving JSON from an HTTP Request
If the mimetype of the HTTP request is `application/json`, calling `request.get_json()` will return the parsed JSON data (otherwise it returns `None`)

    from flask import Flask, jsonify
    
    app = Flask(__name__)

    @app.route('/api/echo-json', methods=['GET', 'POST', 'DELETE', 'PUT'])                                                                                                    
    def add():                                                                                                                              
        data = request.get_json()
        # ... do your business logic, and return some response
        # e.g. below we're just echo-ing back the received JSON data
        return jsonify(data)

Try it with `curl`
-

The parameter `-H 'Content-Type: application/json'` specifies that this is a JSON request:

     curl -X POST -H 'Content-Type: application/json' http://127.0.0.1:5000/api/echo-json -d '{"name": "Alice"}'               
    {
      "name": "Alice"
    }

To send requests using other HTTP methods, substitute `curl -X POST` with the desired method e.g. `curl -X GET`, `curl -X PUT`, etc.


## Return a JSON Response from Flask API
Flask has a utility called `jsonify()` that makes it more convenient to return JSON responses

    from flask import Flask, jsonify
    
    app = Flask(__name__)

    @app.route('/api/get-json')
    def hello():
        return jsonify(hello='world') # Returns HTTP Response with {"hello": "world"}

Try it with `curl`
-

    curl -X GET http://127.0.0.1:5000/api/get-json
    {
      "hello": "world"
    }

Other ways to use `jsonify()`
-
Using an existing dictionary:

    person = {'name': 'Alice', 'birth-year': 1986}
    return jsonify(person)

Using a list:

    people = [{'name': 'Alice', 'birth-year': 1986},
              {'name': 'Bob', 'birth-year': 1985}]
    return jsonify(people) 

 


