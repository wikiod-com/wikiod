---
title: "CRUD in Django"
slug: "crud-in-django"
draft: false
images: []
weight: 9907
type: docs
toc: true
---

## **Simplest CRUD example**
If you find these steps unfamiliar, consider starting [here instead][1]. Note these steps come from Stack Overflow Documentation.
    
    django-admin startproject myproject
    cd myproject
    python manage.py startapp myapp

**myproject/settings.py** Install the app
    
    INSTALLED_APPS = [
        'django.contrib.admin',
        'django.contrib.auth',
        'django.contrib.contenttypes',
        'django.contrib.sessions',
        'django.contrib.messages',
        'django.contrib.staticfiles',
        'myapp',
    ]
Create a file called `urls.py` within the **myapp** directory and updated it with the following view.
    
    from django.conf.urls import url
    from myapp import views

    urlpatterns = [
        url(r'^$', views.index, name='index'),
        ]

Update the other `urls.py` file with the following content.

    from django.conf.urls import url
    from django.contrib import admin
    from django.conf.urls import include 
    from myapp import views

    urlpatterns = [
        url(r'^$', views.index, name='index'),
        url(r'^myapp/', include('myapp.urls')),
        url(r'^admin/', admin.site.urls),
    ]

Create a folder named `templates` within the **myapp** directory. Then create a file named `index.html` inside of the **templates** directory. Fill it with the following content.

    <!DOCTYPE html>
    <html>
    <head>
        <title>myapp</title>
    </head>
    <body>
        <h2>Simplest Crud Example</h2>
        <p>This shows a list of names and lets you Create, Update and Delete them.</p>
        <h3>Add a Name</h3>
        <button>Create</button>
    </body>
    </html>

We also need a view to show **index.html** which we can create by editing the **views.py** file like so:
    
    from django.shortcuts import render, redirect

    # Create your views here.
    def index(request):
        return render(request, 'index.html', {})

You now have the base that you're going to work off of. The next step is create a Model. This is the simplest example possible so in your **models.py** folder add the following code.

    from __future__ import unicode_literals

    from django.db import models

    # Create your models here.
    class Name(models.Model):
        name_value = models.CharField(max_length=100)

        def __str__(self): # if Python 2 use __unicode__
            return self.name_value

This creates a model of a Name object which we'll add to the database with the following commands from the command line.

    python manage.py createsuperuser 
    python manage.py makemigrations
    python manage.py migrate

You should see some operations performed by Django. These setup up the tables and create a superuser that can access the admin database from a Django powered admin view. Speaking of which, lets register our new model with the admin view. Go to **admin.py** and add the following code.

    from django.contrib import admin
    from myapp.models import Name
    # Register your models here.

    admin.site.register(Name)

Back at the command line you can now spin up the server with the `python manage.py runserver` command. You should be able to visit http://localhost:8000/ and see your app. Please then navigate to http://localhost:8000/admin so that you can add a name to your project. Log in and add a Name under the MYAPP table, we kept it simple for the example so ensure it's less than 100 characters.

In order to access the name you need to display it somewhere. Edit the index function within **views.py** to get all of the Name objects out of the database.

    from django.shortcuts import render, redirect
    from myapp.models import Name

    # Create your views here.
    def index(request):
        names_from_db = Name.objects.all()
        context_dict = {'names_from_context': names_from_db}
        return render(request, 'index.html', context_dict)

Now edit the **index.html** file to the following.

    <!DOCTYPE html>
    <html>
    <head>
        <title>myapp</title>
    </head>
    <body>
        <h2>Simplest Crud Example</h2>
        <p>This shows a list of names and lets you Create, Update and Delete them.</p>
        {% if names_from_context %}
            <ul>
                {% for name in names_from_context %}
                    <li>{{ name.name_value }}  <button>Delete</button> <button>Update</button></li>
                {% endfor %}
            </ul>
        {% else %}
            <h3>Please go to the admin and add a Name under 'MYAPP'</h3>
        {% endif %}
        <h3>Add a Name</h3>
        <button>Create</button>
    </body>
    </html>

That demonstrates the Read in CRUD. Within the **myapp** directory create a forms.py file. Add the following code:

    from django import forms
    from myapp.models import Name

    class NameForm(forms.ModelForm):
        name_value = forms.CharField(max_length=100, help_text = "Enter a name")

        class Meta:
            model = Name
            fields = ('name_value',)

Update the **index.html** in the following manner:

    <!DOCTYPE html>
    <html>
    <head>
        <title>myapp</title>
    </head>
    <body>
        <h2>Simplest Crud Example</h2>
        <p>This shows a list of names and lets you Create, Update and Delete them.</p>
        {% if names_from_context %}
            <ul>
                {% for name in names_from_context %}
                    <li>{{ name.name_value }}  <button>Delete</button> <button>Update</button></li>
                {% endfor %}
            </ul>
        {% else %}
            <h3>Please go to the admin and add a Name under 'MYAPP'</h3>
        {% endif %}
        <h3>Add a Name</h3>
        <form id="name_form" method="post" action="/">
            {% csrf_token %}
            {% for field in form.visible_fields %}
                {{ field.errors }}
                {{ field.help_text }}
                {{ field }}
            {% endfor %}
            <input type="submit" name="submit" value="Create">
        </form>
    </body>
    </html>

Next update the **views.py** in the following manner:

    from django.shortcuts import render, redirect
    from myapp.models import Name
    from myapp.forms import NameForm

    # Create your views here.
    def index(request):
        names_from_db = Name.objects.all()

        form = NameForm()

        context_dict = {'names_from_context': names_from_db, 'form': form}

        if request.method == 'POST':
            form = NameForm(request.POST)

            if form.is_valid():
                form.save(commit=True)
                return render(request, 'index.html', context_dict)
            else:
                print(form.errors)    

        return render(request, 'index.html', context_dict)

Restart your server and you should now have a working version of the app with the C in create completed.

**TODO** add update and delete


  [1]: https://www.wikiod.com/django/getting-started-with-django

