---
title: "Authentication"
slug: "authentication"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Setting up authentication globally
Authentication in Django REST Framework can be configured globally as a subkey of the `REST_FRAMEWORK` variable in settings.py, just like the rest of the default framework configurations.

<!-- language lang-python -->
    REST_FRAMEWORK = {
        'DEFAULT_AUTHENTICATION_CLASSES': (
            'rest_framework.authentication.BasicAuthentication',
            'rest_framework.authentication.SessionAuthentication',
        )
    }

## Setting up authentication for a specific APIView
Authentication can be set for an specific APIView endpoint, by using the     `authentication_classes` variable:

<!-- language lang-python -->
    from rest_framework.authentication import SessionAuthentication, BasicAuthentication
    from rest_framework.permissions import IsAuthenticated
    from rest_framework.response import Response
    from rest_framework.views import APIView

    class ExampleView(APIView):
        authentication_classes = (SessionAuthentication, BasicAuthentication)
        permission_classes = (IsAuthenticated,)
    
        def get(self, request):
            content = {
                'user': unicode(request.user),  # `django.contrib.auth.User` instance.
                'auth': unicode(request.auth),  # None
            }
            return Response(content)


## Using basic token-based authentication
Django REST Framework provides a basic token-based authentication mechanism which needs to be configured as an application in Django before being usable, so that tokens are created in the database, and their lifecycle handled.

# Add Token-based authentication to settings.py

<!-- language lang-python -->
    INSTALLED_APPS = (
        ...
        'rest_framework.authtoken'
    )

# Run the database migration

<!-- language lang-bash -->
    ./manage.py migrate

# Create tokens for your users
Somehow, you will have to create a token and return it:

    def some_api(request):
        token = Token.objects.create(user=request.user)
        return Response({'token': token.key})

There is already an API endpoint in the token application, so that you can simply add the following to your urls.py:

## urls.py

<!-- language lang-python -->
    from rest_framework.authtoken import views
    urlpatterns += [
        url(r'^auth-token/', views.obtain_auth_token)
    ]

# Clients can now authenticate
Using a `Authorization` header like:

    Authorization: Token 123456789

Prefixed by a literal "Token" and the token itself after whitespace.

The literal can be changed by subclassing `TokenAuthentication` and changing the `keyword` class variable.

If authenticated, `request.auth` will contain the `rest_framework.authtoken.models.BasicToken` instance.

## Setting up OAuth2 authentication
OAuth is not handled by Django REST Framework, but there are a couple of pip modules that implement an OAuth client. The REST Framework documentation suggests one of the following modules:

- [Django OAuth Toolkit](https://github.com/evonove/django-oauth-toolkit)
- [Django REST Framework OAuth](https://github.com/jpadilla/django-rest-framework-oauth/)

# Django OAuth Toolkit

    pip install django-oauth-toolkit

## settings.py

    INSTALLED_APPS = (
        ...
        'oauth2_provider',
    )

    REST_FRAMEWORK = {
        'DEFAULT_AUTHENTICATION_CLASSES': (
            'oauth2_provider.ext.rest_framework.OAuth2Authentication',
        )
    }

## urls.py

<!-- language lang-python -->
    urlpatterns = patterns(
        ...
        url(r'^o/', include('oauth2_provider.urls', namespace='oauth2_provider')),
    )

---

# Django REST Framework OAuth

<!-- language lang-bash -->
    pip install djangorestframework-oauth django-oauth2-provider

## settings.py

<!-- language lang-python -->
    INSTALLED_APPS = (
        ...
        'provider',
        'provider.oauth2',
    )

    REST_FRAMEWORK = {
        'DEFAULT_AUTHENTICATION_CLASSES': (
            'rest_framework.authentication.OAuth2Authentication',
        )
    }

## urls.py

<!-- language lang-python -->
    urlpatterns = patterns(
        ...
        url(r'^oauth2/', include('provider.oauth2.urls', namespace='oauth2')),
    )

## Admin

Go to the admin panel and create a new `Provider.Client` to have a `client_id` and a `client_secret`.



## Using better Token-based authorization with several client-tokens
The most interesting package for managing real tokens is [django-rest-knox](https://github.com/James1345/django-rest-knox) which supports multiple tokens per user (and cancelling each token independently), as well as having support for token expiration and several other security mechanisms.

`django-rest-knox` depends on `cryptography`. You can find more information on how to install it at: http://james1345.github.io/django-rest-knox/installation/

# Installing knox

<!-- language lang-bash -->
    pip install django-rest-knox

## settings.py

<!-- language lang-python -->
    INSTALLED_APPS = (
      ...
      'rest_framework',
      'knox',
      ...
    )

Apply migrations:

<!-- language lang-bash -->
    ./manage.py migrate



