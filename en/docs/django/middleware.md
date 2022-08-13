---
title: "Middleware"
slug: "middleware"
draft: false
images: []
weight: 9714
type: docs
toc: true
---

Middleware in Django is a framework that allows code to hook into the response / request processing and alter the input or output of Django.

Middleware needs to be added to your settings.py `MIDDLEWARE_CLASSES` list before it will be included in execution.  The default list that Django provides when creating a new project is as follows:

```
MIDDLEWARE_CLASSES = [
    'django.middleware.security.SecurityMiddleware',
    'django.contrib.sessions.middleware.SessionMiddleware',
    'django.middleware.common.CommonMiddleware',
    'django.middleware.csrf.CsrfViewMiddleware',
    'django.contrib.auth.middleware.AuthenticationMiddleware',
    'django.contrib.auth.middleware.SessionAuthenticationMiddleware',
    'django.contrib.messages.middleware.MessageMiddleware',
    'django.middleware.clickjacking.XFrameOptionsMiddleware',
]
```

These are all functions that will run in **order** on every request (once before it reaches your view code in `views.py` and once in reverse order for `process_response` callback, before version 1.10). They do a variety of things such as injecting the [Cross Site Request Forgery (csrf)][1] token. 

The order matters because if some middleware does a redirect, then the all the subsequent middleware will never run. Or if a middleware expects the csrf token to be there, it has to run after the `CsrfViewMiddleware`. 


  [1]: https://docs.djangoproject.com/en/1.9/ref/csrf/

## Understanding Django 1.10 middleware's new style
Django 1.10 introduced a new middleware style where `process_request` and `process_response` are merged together.

In this new style, *a middleware is a callable that returns another callable*. Well, actually the **former is a middleware factory** and **the latter is the actual middleware**.

The *middleware factory* takes as single argument the next *middleware* in the middlewares stack, or the view itself when the bottom of the stack is reached.

The *middleware* takes the request as single argument and **always returns an `HttpResponse`**.

The best example to illustrate how new-style *middleware* works is probably to show how to make a backward-compatible *middleware*:

    class MyMiddleware:

        def __init__(self, next_layer=None):
            """We allow next_layer to be None because old-style middlewares
            won't accept any argument.
            """
            self.get_response = next_layer

        def process_request(self, request):
            """Let's handle old-style request processing here, as usual."""
            # Do something with request
            # Probably return None
            # Or return an HttpResponse in some cases

        def process_response(self, request, response):
            """Let's handle old-style response processing here, as usual."""
            # Do something with response, possibly using request.
            return response

        def __call__(self, request):
            """Handle new-style middleware here."""
            response = self.process_request(request)
            if response is None:
                # If process_request returned None, we must call the next middleware or
                # the view. Note that here, we are sure that self.get_response is not
                # None because this method is executed only in new-style middlewares.
                response = self.get_response(request)
            response = self.process_response(request, response)
            return response

## Middleware to filter by IP address
**First: The path structure**

If you don't have it you need to create the **middleware** folder within your app following the structure:

    yourproject/yourapp/middleware

*The folder middleware should be placed in the same folder as settings.py, urls, templates...*

**Important: Don't forget to create the __init__.py empty file inside the middleware folder so your app recognizes this folder**

*Instead of having a separate folder containing your middleware classes, it is also possible to put your functions in a single file, `yourproject/yourapp/middleware.py`*.

**Second: Create the middleware**

Now we should create a file for our custom middleware. In this example let's suppose we want a middleware that filter the users based on their IP address, we create a file called **filter_ip_middleware.py**:

    #yourproject/yourapp/middleware/filter_ip_middleware.py
    from django.core.exceptions import PermissionDenied    

    class FilterIPMiddleware(object):
        # Check if client IP address is allowed
        def process_request(self, request):
            allowed_ips = ['192.168.1.1', '123.123.123.123', etc...] # Authorized ip's
            ip = request.META.get('REMOTE_ADDR') # Get client IP address
            if ip not in allowed_ips:
                raise PermissionDenied # If user is not allowed raise Error

           # If IP address is allowed we don't do anything
           return None

**Third: Add the middleware in our 'settings.py'**

We need to look for the `MIDDLEWARE_CLASSES` inside the settings.py and there we need to add our middleware (*Add it in the last position*). It should be like:

    MIDDLEWARE_CLASSES = (
        'django.middleware.common.CommonMiddleware',
        'django.contrib.sessions.middleware.SessionMiddleware',
        'django.middleware.csrf.CsrfViewMiddleware',
        'django.contrib.auth.middleware.AuthenticationMiddleware',
        'django.contrib.messages.middleware.MessageMiddleware',
         # Above are Django standard middlewares

         # Now we add here our custom middleware
         'yourapp.middleware.filter_ip_middleware.FilterIPMiddleware'
    )

**Done!** Now every request from every client will call your custom middleware and process your custom code!
    

## Add data to requests
Django makes it really easy to add additional data onto requests for use within the view.  For example, we can parse out the subdomain on the request's META and attach it as a separate property on the request by using middleware.
```
class SubdomainMiddleware:
    def process_request(self, request):
        """
        Parse out the subdomain from the request
        """
        host = request.META.get('HTTP_HOST', '')
        host_s = host.replace('www.', '').split('.')
        request.subdomain = None
        if len(host_s) > 2:
            request.subdomain = host_s[0]
```
If you add data with middleware to your request, you can access that newly added data further down the line.  Here we'll use the parsed subdomain to determine something like what organization is accessing your application.  This approach is useful for apps that are deployed with a DNS setup with wildcard subdomains that all point to a single instance and the person accessing the app wants a skinned version dependent on the access point.
```
class OrganizationMiddleware:
    def process_request(self, request):
        """
        Determine the organization based on the subdomain
        """
        try:
            request.org = Organization.objects.get(domain=request.subdomain)
        except Organization.DoesNotExist:
            request.org = None
```
Remember that order matters when having middleware depend on one another.  For requests, you'll want the dependent middleware to be placed after the dependency.

```
MIDDLEWARE_CLASSES = [
    ...
    'myapp.middleware.SubdomainMiddleware',
    'myapp.middleware.OrganizationMiddleware',
    ...
]

```

## Globally handling exception
Say you have implemented some logic to detect attempts to modify an object in the database while the client that submitted changes didn't have the latest modifications. If such case happens, you raise a custom exception `ConfictError(detailed_message)`.

Now you want to return an [HTTP 409 (Confict)][1] status code when this error occurs. You may typically use as middleware for this instead of handling it in each view that might raise this exception.

    class ConfictErrorHandlingMiddleware:
        def process_exception(self, request, exception):
            if not isinstance(exception, ConflictError):
                return  # Propagate other exceptions, we only handle ConflictError
            context = dict(confict_details=str(exception))
            return TemplateResponse(request, '409.html', context, status=409)

 [1]: https://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.10

