---
title: "Getting started with django-admin"
slug: "getting-started-with-django-admin"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Remove a Model from Admin pages
Django Admin comes with some Models registerd by default. There a some occasions where you might want to remove a Model from the admin pages.

This is done in the `admin` submodule. If your app wass created using `manage.py startapp`, the `admin.py` file should already lay in your app module. Otherwise create it.

    #myapp/admin.py
    from django.contrib import admin
    from django.contrib.auth.models import User

    admin.site.unregister(User)

## Setup Django Admin
<!-- language-all: python -->

Everything you need to get started with Django admin is already setup in Django's default project layout. This includes:

    # settings.py

    # `django.contrib.admin` and its dependancies.
    INSTALLED_APPS = [
        'django.contrib.admin',
        'django.contrib.auth',
        'django.contrib.contenttypes',
        'django.contrib.sessions',
        'django.contrib.messages',
        ...,
    ]

    MIDDLEWARE = [
        ...
        'django.contrib.auth.middleware.AuthenticationMiddleware',
        'django.contrib.messages.middleware.MessageMiddleware',
        ...
    ]

    TEMPLATES = [
        {
            ...,
            'OPTIONS': {
                'context_processors': [
                    'django.contrib.auth.context_processors.auth',
                    'django.contrib.messages.context_processors.messages',
                    ...
                ],
            },
        },
    ]

Be careful about `urls.py` that is slightly different in Django >= 1.9 than in older versions.

<!-- if version [gte 1.9] -->
    from django.conf.urls import url
    from django.contrib import admin
    
    urlpatterns = [
        url(r'^admin/', admin.site.urls),
    ]
<!-- end version if -->

<!-- if version [lt 1.9] -->
    from django.conf.urls import url, include
    from django.contrib import admin

    urlpatterns = [
        url(r'^admin/', include(admin.site.urls)),
    ]
<!-- end version if -->

Version with `include` will still work in Django 1.9 but is deprecated and will be removed in the future.

If not already done, you must apply the base migrations:

    $ python manage.py migrate

To access the admin, you also have to create a superuser with:

    $ python manage.py createsuperuser

Once this is done, you can run your server:

    $ python manage.py runserver

And visit the admin page at http://127.0.0.1:8000/admin/ 

## Add a Model to Admin pages
<!-- language-all: python -->

When you created your own Models in a app, they still need to be *registered* in order to become available in the admin pages.

This is done in the `admin` submodule. If your app was created using `manage.py startapp`, an `admin.py` file should already lay in you app module. Otherwise create it. 

    #myapp/admin.py
    from django.contrib import admin
    from myproject.myapp.models import MyModel
    
    admin.site.register(MyModel)

All options are defined on the ModelAdmin subclass.
some options:

    class MyCustomAdmin(admin.ModelAdmin):
        list_display = ('name','age','email')  # fields to display in the listing
        empty_value_display = '-empty-'        # display value when empty 
        list_filter = ('name', 'company')      # enable results filtering
        list_per_page = 25                     # number of items per page 
        ordering = ['-pub_date', 'name']       # Default results ordering

    # and register it 
    admin.site.register(MyModel, MyCustomAdmin)

A more concise way to register a model is to use the `admin.register` decorator:

    @admin.register(MyModel)
    class MyCustomAdmin(admin.ModelAdmin)
        ...

## Customize django User Admin Model
    from django.contrib.auth.models import User    
    class UserAdmin(admin.ModelAdmin):
        list_display = ('email', 'first_name', 'last_name')
        list_filter = ('is_staff', 'is_superuser')
        
    admin.site.unregister(User) 
    admin.site.register(User, UserAdmin)
    
We need to unregister before register custom UserAdmin because in django User Model Admin already registered, So we need to firstly unregister User Model in our admin.py then we can register User Model with custom ModelAdmin

