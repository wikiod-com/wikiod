---
title: "Getting started with tornado"
slug: "getting-started-with-tornado"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Python3 - `sudo pip3 install tornado`
<br>
Python2 - `sudo pip install tornado`
<br><br>
Packages which will are optional but recommended to install alongside Tornado :

 - [concurrent.futures][1]
 - [pycurl][2]
 - [pycares][3]
 - [Twisted][4]
 - [monotonic][5] or [monotime][6]


  [1]: https://pypi.python.org/pypi/futures
  [2]: http://pycurl.io/
  [3]: https://pypi.python.org/pypi/pycares
  [4]: http://www.twistedmatrix.com/
  [5]: https://pypi.python.org/pypi/monotonic
  [6]: https://pypi.python.org/pypi/Monotime

## Hello World
    # hello_server.py
    import tornado.ioloop
    import tornado.web

    class MainHandler(tornado.web.RequestHandler):
        def get(self):
            self.write("Hello, world")

        def make_app():
            return tornado.web.Application([ (r"/", MainHandler), ])  # URL Mapping

    if __name__ == "__main__":
        app = make_app()
        app.listen(8888)    # Port Number
        tornado.ioloop.IOLoop.current().start()

This App is run by typing `python3 hello_server.py` or `python hello_server.py` depending on the version of Python being used.
<br>When run locally the server can be accessed by going to `127.0.0.1:8888` from the browser.
<br>The server will return "Hello World".
<br>In `make_app()` function, the root `/` is mapped to `MainHandler`. This means that requests to the root IP `127.0.0.1:8888` will be mapped to the `MainHandler` function.

