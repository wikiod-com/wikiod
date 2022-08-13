---
title: "Authentication Backends"
slug: "authentication-backends"
draft: false
images: []
weight: 9829
type: docs
toc: true
---

## Email Authentication Backend
Django's default authentication works on `username` and `password` fields. Email authentication backend will authenticate users based on `email` and `password`.

    from django.contrib.auth import get_user_model

    class EmailBackend(object):
        """
        Custom Email Backend to perform authentication via email
        """
        def authenticate(self, username=None, password=None):
            user_model = get_user_model() 
            try:
                user = user_model.objects.get(email=username)
                if user.check_password(password): # check valid password
                    return user # return user to be authenticated
            except user_model.DoesNotExist: # no matching user exists 
                return None 
    
        def get_user(self, user_id):
            user_model = get_user_model() 
            try:
                return user_model.objects.get(pk=user_id)
            except user_model.DoesNotExist:
                return None

Add this authentication backend to the `AUTHENTICATION_BACKENDS` setting. 

    # settings.py
    AUTHENTICATION_BACKENDS = (
        'my_app.backends.EmailBackend', 
        ... 
        )


