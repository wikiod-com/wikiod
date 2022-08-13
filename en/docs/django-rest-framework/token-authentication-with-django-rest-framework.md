---
title: "Token Authentication With Django Rest Framework"
slug: "token-authentication-with-django-rest-framework"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## ADDING AUTH FUNCTIONALITY
Django REST Framework has some authentication methods already built in, one of them is Token based, so first thing to do is to tell our project we’re going to use rest framework’s authentication.
Open `settings.py` file and add the highlighted line.

    INSTALLED_APPS = (
        'django.contrib.admin',
        'django.contrib.auth',
        'django.contrib.contenttypes',
        'django.contrib.sessions',
        'django.contrib.messages',
        'django.contrib.staticfiles',
        'rest_framework',
        'rest_framework.authtoken',
        'test_app',
    )
As we’ve added a new app to the project, we must synchronize
   

    python manage.py migrate

**CREATING A SUPERUSER IN DJANGO**

In order to use authentication, we can rely on django users model, so the first thing to do is to create a superuser.

    python manage.py createsuperuser



## GETTING THE USER TOKEN
Token authentication functionality assigns a token to a user, so each time you use that token, the request object will have a user attribute that holds the user model information. Easy, isn’t it?

We’ll create a new POST method to return the token for this user, as long as the request holds a correct user and password. Open **views.py** located at test_app application folder.


    from rest_framework.response import Response
     
    from rest_framework.authtoken.models import Token
    from rest_framework.exceptions import ParseError
    from rest_framework import status
     
    from django.contrib.auth.models import User
     
     
    # Create your views here.
    class TestView(APIView):
        """
        """
     
        def get(self, request, format=None):
            return Response({'detail': "GET Response"})
     
        def post(self, request, format=None):
            try:
                data = request.DATA
            except ParseError as error:
                return Response(
                    'Invalid JSON - {0}'.format(error.detail),
                    status=status.HTTP_400_BAD_REQUEST
                )
            if "user" not in data or "password" not in data:
                return Response(
                    'Wrong credentials',
                    status=status.HTTP_401_UNAUTHORIZED
                )
     
            user = User.objects.first()
            if not user:
                return Response(
                    'No default user, please create one',
                    status=status.HTTP_404_NOT_FOUND
                )
     
            token = Token.objects.get_or_create(user=user)
     
            return Response({'detail': 'POST answer', 'token': token[0].key})

## USING THE TOKEN
Let’s create a new View that requires this authentication mechanism.

We need to add these import lines:

    from rest_framework.authentication import TokenAuthentication
    from rest_framework.permissions import IsAuthenticated

and then create the new View in the same `views.py` file

    class AuthView(APIView):
        """
        Authentication is needed for this methods
        """
        authentication_classes = (TokenAuthentication,)
        permission_classes = (IsAuthenticated,)
     
        def get(self, request, format=None):
            return Response({'detail': "I suppose you are authenticated"})


As we did on previous post, we need to tell our project that we have a new REST path listening, on `test_app/urls.py`

    from rest_framework.urlpatterns import format_suffix_patterns
    from test_app import views
     
    urlpatterns = patterns('test_app.views',
        url(r'^', views.TestView.as_view(), name='test-view'),
        url(r'^auth/', views.AuthView.as_view(), name='auth-view'),
    )
     
    urlpatterns = format_suffix_patterns(urlpatterns)

## Using CURL
If a curl would be run against this endpoint

    curl http://localhost:8000/auth/    
    op : {"detail": "Authentication credentials were not provided."}%

would return a **401 error UNAUTHORIZED**

but in case we get a token before:

    curl -X POST -d "user=Pepe&password=aaaa"  http://localhost:8000/
    {"token": "f7d6d027025c828b65cee5d38240aec60dffa150", "detail": "POST answer"}%

and then we put that token into the header of the request like this:

    curl http://localhost:8000/auth/ -H 'Authorization: Token f7d6d027025c828b65cee5d38240aec60dffa150'

    op: {"detail": "I suppose you are authenticated"}%

