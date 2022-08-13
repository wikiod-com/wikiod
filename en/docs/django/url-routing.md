---
title: "URL routing"
slug: "url-routing"
draft: false
images: []
weight: 9765
type: docs
toc: true
---

## Set the URL namespace for a reusable app (Django 1.9+)
Configure your app's URLconf to automatically use a URL namespace by setting the `app_name` attribute:

<!-- lang: python -->
    # In <myapp>/urls.py
    from django.conf.urls import url
    
    from .views import overview
    
    app_name = 'myapp'
    urlpatterns = [
        url(r'^$', overview, name='overview'),
    ]

This will set the [application namespace](https://docs.djangoproject.com/en/stable/topics/http/urls/#term-application-namespace) to `'myapp'` when it is included in the root URLconf>. The user of your reusable app does not need to do any configuration other than including your URLs:

<!-- lang: python -->
    # In <myproject>/urls.py
    from django.conf.urls import include, url

    urlpatterns = [
        url(r'^myapp/', include('myapp.urls')),
    ]

Your reusable app can now reverse URLs using the application namespace:

<!-- lang: python -->
    >>> from django.urls import reverse
    >>> reverse('myapp:overview')
    '/myapp/overview/'

The root URLconf can still set an instance namespace with the `namespace` parameter:

<!-- lang: python -->
    # In <myproject>/urls.py
    urlpatterns = [
        url(r'^myapp/', include('myapp.urls', namespace='mynamespace')),
    ]

Both the application namespace and instance namespace can be used to reverse the URLs:

<!-- lang: python -->
    >>> from django.urls import reverse
    >>> reverse('myapp:overview')
    '/myapp/overview/'
    >>> reverse('mynamespace:overview')
    '/myapp/overview/'

The instance namespace defaults to the application namespace if it is not explicitly set.

## How Django handles a request
Django handles a request by routing the incoming URL path to a view function. The view function is responsible for returning a response back to the client making the request. Different URLs are usually handled by different view functions. To route the request to a specific view function, Django looks at your URL configuration (or URLconf for short). The default project template defines the URLconf in `<myproject>/urls.py`. 

Your URLconf should be a python module that defines an attribute named `urlpatterns`, which is a list of [`django.conf.urls.url()`](https://docs.djangoproject.com/en/1.9/ref/urls/#django.conf.urls.url) instances. Each `url()` instance must at minimum define a [regular expression](https://www.wikiod.com/python/regular-expressions-regex) (a regex) to match against the URL, and a target, which is either a view function or a different URLconf. If a URL pattern targets a view function, it is a good idea to give it a name to easily reference the pattern later on.

Let's take a look at a basic example:

<!-- language: python -->

    # In <myproject>/urls.py

    from django.conf.urls import url
    
    from myapp.views import home, about, blog_detail
    
    urlpatterns = [
        url(r'^$', home, name='home'),
        url(r'^about/$', about, name='about'),
        url(r'^blog/(?P<id>\d+)/$', blog_detail, name='blog-detail'),
    ]

This URLconf defines three URL patterns, all targeting a view: `home`, `about` and `blog-detail`. 

* `url(r'^$', home, name='home'),`

The regex contains a start anchor '^', immediately followed by an end anchor '$'. This pattern will match requests where the URL path is an empty string, and route them to the `home` view defined in `myapp.views`. 

* `url(r'^about/$', about, name='about'),`

This regex contains a start anchor, followed by the literal string `about/`, and the end anchor. This will match the URL `/about/` and route it to the `about` view. Since every non-empty URL start with a `/`, Django conveniently cuts of the first slash for you. 

* `url(r'^blog/(?P<id>\d+)/$', blog_detail, name='blog-detail'),`

This regex is a bit more complex. It defines the start anchor and the literal string `blog/`, like the previous pattern. The next part, `(?P<id>\d+)`, is called a capturing group. A capturing group, like its name suggest, captures a part of the string, and Django passes the captured string as an argument to the view function.

The syntax of a capturing group is `(?P<name>pattern)`. `name` defines the name of the group, which is also the name that Django uses to pass the argument to the view. The pattern defines which characters are matched by the group. 

In this case, the name is `id`, so the function `blog_detail` must accept a parameter named `id`. The pattern is `\d+`. `\d` signifies that the pattern only matches number characters. `+` signifies that the pattern must match one or more characters. 

Some common patterns:

| Pattern | Used for | Matches |
| ------- | :------: | ----------- |
| `\d+` | id | One or more numerical characters |
| `[\w-]+` | slug | One or more alphanumerical characters, underscores or dashes |
| `[0-9]{4}` | year (long) | Four numbers, zero through nine |
| `[0-9]{2}` | year (short)<br /> month<br /> day of month | Two numbers, zero through nine |
| `[^/]+` | path segment | Anything except a slash |

The capturing group in the `blog-detail` pattern is followed by a literal `/`, and the end anchor.

Valid URLs include:
* `/blog/1/  # passes id='1'`
* `/blog/42/  # passes id='42'`

Invalid URLs are for example:
* `/blog/a/  # 'a' does not match '\d'`
* `/blog//  # no characters in the capturing group does not match '+'`


----------

Django processes each URL pattern in the same order they are defined in `urlpatterns `. This is important if multiple patterns can match the same URL. For example:

<!-- language: python -->

    urlpatterns = [
        url(r'blog/(?P<slug>[\w-]+)/$', blog_detail, name='blog-detail'),
        url(r'blog/overview/$', blog_overview, name='blog-overview'),
    ]

In the above URLconf, the second pattern is not reachable. The pattern would match the URL `/blog/overview/`, but instead of calling the `blog_overview` view, the URL will first match the `blog-detail` pattern and call the `blog_detail` view with an argument `slug='overview'`. 

To make sure that the URL `/blog/overview/` is routed to the `blog_overview` view, the pattern should be put above the `blog-detail` pattern:

<!-- language: python -->

    urlpatterns = [
        url(r'blog/overview/$', blog_overview, name='blog-overview'),
        url(r'blog/(?P<slug>[\w-]+)/$', blog_detail, name='blog-detail'),
    ]

