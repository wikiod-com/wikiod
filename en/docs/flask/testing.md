---
title: "Testing"
slug: "testing"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

## Testing a JSON API implemented in Flask
<sub>This example assumes you know how to [test a Flask app using pytest](https://www.wikiod.com/flask/testing#Testing our Hello World app)</sub>

Below is an API that takes a JSON input with integer values `a` and `b` e.g. `{"a": 1, "b": 2}`, adds them up and returns sum `a + b` in a JSON response e.g. `{"sum": 3}`.

    # hello_add.py
    from flask import Flask, request, jsonify

    app = Flask(__name__)

    @app.route('/add', methods=['POST'])
    def add():
        data = request.get_json()
        return jsonify({'sum': data['a'] + data['b']})


Testing this API with `pytest`
-

We can test it with `pytest`

    # test_hello_add.py
    from hello_add import app
    from flask import json
  
    def test_add():        
        response = app.test_client().post(
            '/add',
            data=json.dumps({'a': 1, 'b': 2}),
            content_type='application/json',
        )

        data = json.loads(response.get_data(as_text=True))

        assert response.status_code == 200
        assert data['sum'] == 3




Now run the test with `py.test` command.

## Testing our Hello World app
Introduction
-
In this minimalist example, using [`pytest`](http://docs.pytest.org/en/latest/) we're going to test that indeed our Hello World app does return "Hello, World!" with an HTTP OK status code of 200, when hit with a GET request on the URL `/`

First let's install `pytest` into our virtualenv

    pip install pytest

And just for reference, this our hello world app:

    # hello.py
    from flask import Flask

    app = Flask(__name__)

    @app.route('/')
    def hello():
        return 'Hello, World!'



Defining the test
-
Along side our `hello.py`, we define a test module called `test_hello.py` that is going to be discovered by `py.test`

    # test_hello.py
    from hello import app
    
    def test_hello():
        response = app.test_client().get('/')

        assert response.status_code == 200
        assert response.data == b'Hello, World!'

Just to review, at this point our project structure obtained with the `tree` command is:

    .
    ├── hello.py
    └── test_hello.py


Running the test
-
Now we can run this test with the `py.test` command that will automatically discover our `test_hello.py` and the test function inside it

    $ py.test

You should see some output and an indication that 1 test has passed, e.g.

    === test session starts ===
    collected 1 items 
    test_hello.py .
    === 1 passed in 0.13 seconds ===


## Accessing and manipulating session variables in your tests using Flask-Testing
Most of the web applications use the session object to store some important information. This examples show how you can test such application using Flask-Testing. Full working example is also available on [github](https://github.com/oggo/Flask-Testing).

So first install Flask-Testing in your virtualenv

    pip install flask_testing

To be able to use the session object you have to set the secret key

    app.secret_key = 'my-seCret_KEy'

Let's imagine you have in your application function that need to store some data in session variables like this



    @app.route('/getSessionVar', methods=['GET', 'POST'])
    def getSessionVariable():
      if 'GET' == request.method:
        session['sessionVar'] = 'hello'
      elif 'POST' == request.method:
        session['sessionVar'] = 'hi'
      else:
        session['sessionVar'] = 'error'

      return 'ok'

To test this function you can import flask_testing and let your test class inherit flask_testing.TestCase. Import also all the necessary libraries 

    import flask
    import unittest
    import flask_testing
    from myapp.run import app
        
    class TestMyApp(flask_testing.TestCase):

Very important before you start testing is to implement the function **create_app** otherwise there will be exception. 

      def create_app(self):
        return app


To test your application is working as wanted you have a couple of possibilities. If you want to just assure your function is setting particular values to a session variable you can just keep the context around and access **flask.session**

    def testSession1(self):
        with app.test_client() as lTestClient:
          lResp= lTestClient.get('/getSessionVar')
          self.assertEqual(lResp.status_code, 200)
          self.assertEqual(flask.session['sessionVar'], 'hello')

One more useful trick is to differentiate between *GET* and *POST* methods like in the next test function

    def testSession2(self):
        with app.test_client() as lTestClient:
          lResp= lTestClient.post('/getSessionVar')
          self.assertEqual(lResp.status_code, 200)
          self.assertEqual(flask.session['sessionVar'], 'hi')

Now imagine your function expects a session variable to be set and reacts different on particular values like this

    @app.route('/changeSessionVar')
    def changeSessionVariable():
      if session['existingSessionVar'] != 'hello':
        raise Exception('unexpected session value of existingSessionVar!')

      session['existingSessionVar'] = 'hello world'
      return 'ok'

To test this function you have to use so called *session transaction* and open the session in the context of the test client. This function is available since **Flask 0.8**

    def testSession3(self):
        with app.test_client() as lTestClient:
          #keep the session
          with lTestClient.session_transaction() as lSess:
            lSess['existingSessionVar'] = 'hello'

          #here the session is stored
          lResp = lTestClient.get('/changeSessionVar')
          self.assertEqual(lResp.status_code, 200)
          self.assertEqual(flask.session['existingSessionVar'], 'hello world')

Running the tests is as usual for unittest

    if __name__ == "__main__":
        unittest.main()
    
And in the command line

    python tests/test_myapp.py

Another nice way to run your tests is to use unittest Discovery like this: 

    python -m unittest discover -s tests




