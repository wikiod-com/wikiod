---
title: "Django Framework"
slug: "django-framework"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Installation & Setup
Django is a full stack framework for web development. It powers some of the most popular websites on the Internet.

To install the framework; use the `pip` tool:

    pip install django

If you are installing this on OSX or Linux, the above command may result in a permission error; to avoid this error, install the package for your user account or use a virtual environment:

    pip install --user django

Once it is installed - you will have access to `django-admin` bootstrapping tool, which will create a directory with some defaults to start development.

    django-admin startproject myproject

This will create a directory _`myproject`_ with the default project layout.

## Django Core Concepts
Django is a full stack, feature rich web development framework. It bundles a lot of functionality together to provide a common, quick and productive experience for web developers.

Django projects consist of common settings, and one or more _applications_. Each application is a set of functionality along with dependencies (such as templates and models) that are bundled together as Python modules.

The django bootstrapping script automatically creates a settings file for your project, with most common features enabled.

This concept of applications allows easy plug-and-play of functionality, and there is a large library of applications available to handle most common tasks. This concept of applications is fundamental to django; a lot of the built-in functionality (such as user authentication and the admin site) are simply django apps.

To create your first application, from within the project directory:

    python manage.py startapp yourapp

_`yourapp`_ is the name of your custom application.

Each application allows you to develop:

1. A series of _views_ - these are pieces of code that are executed in response to a request.

2. One or more _models_; which are an abstraction to databases. These allow you to define your objects as Python objects, and the built-in ORM provides a friendly API to storing, retrieving and filtering objects from databases.

3. Closely related to models are _migrations_ which are scripts that are generated to provide a consistent and reliable method of applying changes in your models, to the database.

4. A set of _urls_ that the application will respond to.

5. One or more admin classes; to customize how the application behaves in the built-in django admin application.

6. Any tests that you may write.

## Core Concepts - Views
A  `view` is any piece of code that responds to a request and returns a response. Views normally return templates along with a dictionary (called the _context_) which usually contains data for placeholders in the template. In django projects, views are located in the `views.py` module of applications.

The simplest view, returns a direct response:

    from django.http import HttpResponse

    def simple_view(request):
       return HttpResponse('<strong>Hello World</strong>')

However, most views utilize a template:

    from django.shortcuts import render

    def simple_template_view(request):
        return render(request, 'some_template.html')

A template is simply any file, and it can optionally contain special markup for added functionality; what this means is that django views can return any kind of response, not just HTML.

## Core Concepts - Templates
In django, a template is simply a file that contains special tags which may be replaced by data from the view.

The canonical template example would be:

    <strong>Hello {{ name }}, I am a template!</strong>

Here, the string _`{{ name }}`_ identifies a placeholder that may be replaced by a context.

To render this template from a view, we can pass in the value as a dictionary:

    from django.shortcuts import render

    def simple_view(request):
        return render(request, 'template.html', {'name': 'Jim'})

Once this view is rendered, the resulting HTML will be **Hello Jim, I am a template!**.

## Core Concepts - URLs
In django, there is a url mapper which maps URLs to specific functions (views) which return responses. This strict separation between the file system layout and the URL layout allows great flexibility when writing applications.

All url patterns are stored in one or more `urls.py` files, and there is a master `urls.py` file which is read by django first.

Django parses the patterns in the order they are written, and stops when it finds a match to the URL being requested by the user. If no matches are found, an error is raised.

In debug mode (activated by setting `DEBUG = True` in `settings.py`), django will print out a detailed error message when a url requested doesn't match any patterns. In production, however, django will display a normal 404 message.

A url pattern consists of a Python regular expression, followed by a _callable_ (a method or function) to be called when that pattern is matched. This function must return a HTTP response:

    url(r'/hello$', simple_view) 

