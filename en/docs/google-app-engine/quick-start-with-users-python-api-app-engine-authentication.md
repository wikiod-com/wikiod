---
title: "Quick start with Users Python API, App Engine Authentication"
slug: "quick-start-with-users-python-api-app-engine-authentication"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Using the [Users API](https://cloud.google.com/appengine/docs/standard/python/users/) is a very simple and flexible way to work the authentication in App Engine, but please make sure that your application cases don't require more elements for the authentication environment.

**Note:** If you need more information about the traditional structure of an App Engine app, please review this [info](https://cloud.google.com/appengine/docs/standard/python/getting-started/creating-guestbook).

**The Users API allows:**
* Detect whether the current user has signed in.
* Redirect the user to the appropriate sign-in page to sign in.
* Request that your application user create a new Google account if they don't have one already.

[Reference and more details](https://cloud.google.com/appengine/docs/standard/python/users/)


----------

Important elements into the view:

**Import:**

    from google.appengine.api import users

**User-object and methods:**

    user = users.get_current_user()

----------

**Note:** the implementation of jinja2 is optional, but into the article is used to explaining the completely workflow.




## MainPage Handler [views.py]
General Imports, using jinja2 to populate templates into htmls.
 
    import jinja2
    import webapp2

Important import to use Users API:

    from google.appengine.api import users

Setting of Jinja environment: [into the example the tehcnology selected to populate the information into the frontend]    
    
    JINJA_ENVIRONMENT = jinja2.Environment(
           loader=jinja2.FileSystemLoader(os.path.dirname(__file__)),
           extensions=['jinja2.ext.autoescape'],
           autoescape=True)
    
Concrete Handler:
    
    class MainPage(webapp2.RequestHandler):
       def get(self):
    
           user = users.get_current_user()
           if user:
               url = users.create_logout_url(self.request.uri)
You can include more logic here for *users*

           else:
               url = users.create_login_url(self.request.uri)

Templates to pass information using jinja2. For this example, the user object and the url string.
   
           template_values = {
               'user': user,
               'url': url,
           }
    
           JINJA_ENVIRONMENT.add_extension('jinja2.ext.do')

Using index.html example. [traditional html page]

           template = JINJA_ENVIRONMENT.get_template('index.html')
           self.response.write(template.render(template_values))

## App Routing [urls.py]
I used for this example webapp2 to cover the routing.
 
    from webapp2_extras.routes import RedirectRoute as Route

Import from views:

    from views import MainPage

MainPage is the handler set into root "/":

    urlpatterns = [
       Route('/', MainPage),
    ]

## Html, frontend example of how use Users API [index.html]
Simple extract of index.html:

        <div class="sign-in">

            {% if user %}

[Passing the url we have the opportunity to logout the user]

                <a href="{{ url|safe }}">LOG OUT</a>

[You can include here operations for user authenticated]

            {% else %}
[Passing the url we have the opportunity to login the user]

                <a href="{{ url|safe }}">SIGN IN</a>

            {% endif %}
        </div>

This is a simple example of how the operation is used on the index.html page. 

