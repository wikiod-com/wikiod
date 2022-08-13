---
title: "Routing"
slug: "routing"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

## Basic Routes
Routes in Flask can be defined using the `route` decorator of the Flask application instance:

    app = Flask(__name__)

    @app.route('/')
    def index():
        return 'Hello Flask'

The `route` decorator takes a string which is the URL to match.  When a request for a URL that matches this string is received by the application, the function decorated (also called a *view function*) will be invoked.  So for an about route we would have:

    @app.route('/about')
    def about():
        return 'About page'

It's important to note that these routes are **not** regular expressions like they are in Django.  

You can also define *variable rules* to extract URL segment values into variables:

    @app.route('/blog/posts/<post_id>')
    def get_blog_post(post_id):
        # look up the blog post with id post_id
        # return some kind of HTML

Here the variable rule is in the last segment of the URL.  Whatever value is in the last segment of the URL will be passed to the view function (`get_blog_post`) as the `post_id` parameter.  So a request to `/blog/posts/42` will retrieve (or attempt to retrieve) the blog post with an id of 42.

It is also common to reuse URLs.  For example maybe we want to have `/blog/posts` return a list of all blog posts.  So we could have two routes for the same view function:

    @app.route('/blog/posts')
    @app.route('/blog/posts/<post_id>')
    def get_blog_post(post_id=None):
        # get the post or list of posts

Note here that we also have to supply the default value of `None` for the `post_id` in `get_blog_post`.  When the first route is matched, there will be no value to pass to the view function.

Also note that by default the type of a variable rule is a string.  However, you can specify several different types such as `int` and `float` by prefixing the variable:

    @app.route('/blog/post/<int:post_id>')

Flask's built-in URL-converters are:

    string | Accepts any text without a slash (the default).
    int    | Accepts integers.
    float  | Like int but for floating point values.
    path   | Like string but accepts slashes.
    any    | Matches one of the items provided
    uuid   | Accepts UUID strings

Should we try to visit the URL `/blog/post/foo` with a value in the last URL segment that cannot be converted to an integer, the application would return a 404 error.  This is the correct action because there is not a rule with `/blog/post` and a string in the last segment.

Finally, routes can be configured to accept HTTP methods as well.  The `route` decorator takes a `methods` keyword argument which is a list of string representing the acceptable HTTP methods for this route.  As you might have assumed, the default is `GET` only.  If we had a form to add a new blog post and wanted to return the HTML for the `GET` request and parse the form data for the `POST` request, the route would look something like this:

    @app.route('/blog/new', methods=['GET', 'POST'])
    def new_post():
        if request.method == 'GET':
            # return the form
        elif request.method == 'POST':
            # get the data from the form values

The `request` is found in the `flask` package.  Note that when using the `methods` keyword argument, we must be explicit about the HTTP methods to accept.  If we had listed only `POST`, the route would no longer respond to `GET` requests and return a 405 error.

## Catch-all route
It may be useful to have one catch-all view where you handle complex logic yourself based on the path.  This example uses two rules: The first rule specifically catches `/` and the second rule catches arbitrary paths with the built-in `path` converter. The `path` converter matches any string (including slashes) See [Flask Variable-Rules][1]

    @app.route('/', defaults={'u_path': ''})
    @app.route('/<path:u_path>')
    def catch_all(u_path):
        print(repr(u_path))
        ...

<!-- break -->

    c = app.test_client()
    c.get('/')  # u_path = ''
    c.get('/hello')  # u_path = 'hello'
    c.get('/hello/stack/overflow/')  # u_path = 'hello/stack/overflow/'


  [1]: http://flask.pocoo.org/docs/0.12/quickstart/#variable-rules "Flask Variable-Rules"

## Routing and HTTP methods
By default, routes only respond to `GET` requests. You can change this behavior by supplying the `methods` argument to the `route()` decorator.

    from flask import request

    @app.route('/login', methods=['GET', 'POST'])
    def login():
        if request.method == 'POST':
            do_the_login()
        else:
            show_the_login_form()


You can also map different functions to the same endpoint based on the HTTP method used.

    @app.route('/endpoint', methods=['GET'])
    def get_endpoint():
        #respond to GET requests for '/endpoint'


    @app.route('/endpoint', methods=['POST', 'PUT', 'DELETE'])
    def post_or_put():
        #respond to POST, PUT, or DELETE requests for '/endpoint'

