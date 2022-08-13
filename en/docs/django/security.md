---
title: "Security"
slug: "security"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Cross Site Scripting (XSS)  protection
XSS attacks consist in injecting HTML (or JS) code in a page. See [What is cross site scripting][1] for more information.

To prevent from this attack, by default, Django escapes strings passed through a template variable.

Given the following context:

    context = {
        'class_name': 'large" style="font-size:4000px',
        'paragraph': (
            "<script type=\"text/javascript\">alert('hello world!');</script>"),
    }

<!-- language: html -->
    <p class="{{ class_name }}">{{ paragraph }}</p>
    <!-- Will be rendered as: -->
    <p class="large&quot; style=&quot;font-size: 4000px">&lt;script&gt;alert(&#39;hello world!&#39;);&lt;/script&gt;</p>

If you have variables containing HTML that you trust and actually want to render, you must explicitly say it is safe:

<!-- language: html -->
    <p class="{{ class_name|safe }}">{{ paragraph }}</p>
    <!-- Will be rendered as: -->
    <p class="large" style="font-size: 4000px">&lt;script&gt;alert(&#39;hello world!&#39;);&lt;/script&gt;</p>

If you have a block containing multiple variables that are all safe, you can locally disable auto escaping:

<!-- language: html -->
    {% autoescape off %}
    <p class="{{ class_name }}">{{ paragraph }}</p>
    {% endautoescape %}
    <!-- Will be rendered as: -->
    <p class="large" style="font-size: 4000px"><script>alert('hello world!');</script></p>

You can also mark a string as safe outside of the  template:

    from django.utils.safestring import mark_safe

    context = {
        'class_name': 'large" style="font-size:4000px',
        'paragraph': mark_safe(
            "<script type=\"text/javascript\">alert('hello world!');</script>"),
    }

<!-- language: html -->
    <p class="{{ class_name }}">{{ paragraph }}</p>
    <!-- Will be rendered as: -->
    <p class="large&quot; style=&quot;font-size: 4000px"><script>alert('hello world!');</script></p>

Some Django utilities such as `format_html` already return strings marked as safe:

    from django.utils.html import format_html

    context = {
        'var': format_html('<b>{}</b> {}', 'hello', '<i>world!</i>'),
    }

<!-- language: html -->

    <p>{{ var }}</p>
    <!-- Will be rendered as -->
    <p><b>hello</b> &lt;i&gt;world!&lt;/i&gt;</p>

[1]: http://stackoverflow.com/questions/15755323/what-is-cross-site-scripting

## Clickjacking protection
> Clickjacking is a malicious technique of tricking a Web user into clicking on something different from what the user perceives they are clicking on. [Learn more][1]

To enable clickjacking protection, add the `XFrameOptionsMiddleware` to your middleware classes. This should already be there if you didn't remove it.

    # settings.py
    MIDDLEWARE_CLASSES = [
        ...
        'django.middleware.clickjacking.XFrameOptionsMiddleware',
        ...
    ]

This middleware sets the 'X-Frame-Options' header to your all your responses, unless explicitly exempted or already set (not overridden if already set in the response). By default it is set to "SAMEORIGIN". To change this, use the `X_FRAME_OPTIONS` setting:

    X_FRAME_OPTIONS = 'DENY'

You can override the default behaviour on a per-view basis.

    from django.utils.decorators import method_decorator
    from django.views.decorators.clickjacking import (
        xframe_options_exempt, xframe_options_deny, xframe_options_sameorigin,
    )

    xframe_options_exempt_m = method_decorator(xframe_options_exempt, name='dispatch')

    @xframe_options_sameorigin
    def my_view(request, *args, **kwargs):
        """Forces 'X-Frame-Options: SAMEORIGIN'."""
        return HttpResponse(...)

    @method_decorator(xframe_options_deny, name='dispatch')
    class MyView(View):
        """Forces 'X-Frame-Options: DENY'."""

    @xframe_options_exempt_m
    class MyView(View):
        """Does not set 'X-Frame-Options' header when passing through the
        XFrameOptionsMiddleware.
        """
    

 [1]: https://en.wikipedia.org/wiki/Clickjacking

## Cross-site Request Forgery (CSRF) protection
> Cross-site request forgery, also known as one-click attack or session
> riding and abbreviated as CSRF or XSRF, is a type of malicious exploit
> of a website where unauthorized commands are transmitted from a user
> that the website trusts. [Learn more](https://en.wikipedia.org/wiki/Cross-site_request_forgery)

To enable CSRF protection, add the `CsrfViewMiddleware` to your middleware classes. This middleware is enabled by default. 

    # settings.py
    MIDDLEWARE_CLASSES = [
        ...
        'django.middleware.csrf.CsrfViewMiddleware',
        ...
    ]

This middleware will set a token in a cookie on the outgoing response. Whenever an incoming request uses an unsafe method (any method except `GET`, `HEAD`, `OPTIONS` and `TRACE`), the cookie must match a token that is send as the `csrfmiddlewaretoken` form data or as the `X-CsrfToken` header. This ensures that the client initiating the request is also the owner of the cookie and, by extension, the (authenticated) session.

If a request is made over `HTTPS`, strict referrer checking is enabled. If the `HTTP_REFERER` header does not match the host of the current request or a host in `CSRF_TRUSTED_ORIGINS` ([new in 1.9](https://docs.djangoproject.com/en/stable/ref/settings/#std:setting-CSRF_TRUSTED_ORIGINS)), the request is denied.

Forms that use the `POST` method should include the CSRF token in the template. The `{% csrf_token %}` template tag will output a hidden field, and will ensure that the cookie is set on the response:

    <form method='POST'>
    {% csrf_token %}
    ...
    </form>

Individual views that are not vulnerable to CSRF attacks can be made exempt using the `@csrf_exempt` decorator:

    from django.views.decorators.csrf import csrf_exempt
    
    @csrf_exempt
    def my_view(request, *args, **kwargs):
        """Allows unsafe methods without CSRF protection"""
        return HttpResponse(...)

Although not recommended, you can disable the `CsrfViewMiddleware` if many of your views are not vulnerable to CSRF attacks. In this case you can use the `@csrf_protect` decorator to protect individual views:

    from django.views.decorators.csrf import csrf_protect
    
    @csrf_protect
    def my_view(request, *args, **kwargs):
        """This view is protected against CSRF attacks if the middleware is disabled"""
        return HttpResponse(...)



