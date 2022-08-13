---
title: "Getting started with Django"
slug: "getting-started-with-django"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Starting a Project
Django is a web development framework based on Python. Django **1.11** (the latest stable release) requires Python **2.7**, **3.4**, **3.5** or **3.6** to be installed. Assuming `pip` is available, installation is as simple as running the following command. Keep in mind, omitting the version as shown below will install the latest version of django:

    $ pip install django

For installing specific version of django, let's suppose the version is django **1.10.5** , run the following command:

    $ pip install django==1.10.5

Web applications built using Django must reside within a Django project. You can use the `django-admin` command to start a new project in the current directory:

    $ django-admin startproject myproject

where `myproject` is a name that uniquely identifies the project and can consist of **numbers**, **letters**, and **underscores**.

This will create the following project structure:

    myproject/
        manage.py
        myproject/
            __init__.py
            settings.py
            urls.py
            wsgi.py

To run the application, start the development server

    $ cd myproject
    $ python manage.py runserver


Now that the server’s running, visit `http://127.0.0.1:8000/` with your web browser. You’ll see the following page:

[![enter image description here][1]][1]

By default, the `runserver` command starts the development server on the internal IP at port `8000`. This server will automatically restart as you make changes to your code. But in case you add new files, you’ll have to manually restart the server.

If you want to change the server’s port, pass it as a command-line argument.

    $ python manage.py runserver 8080

If you want to change the server’s IP, pass it along with the port.

    $ python manage.py runserver 0.0.0.0:8000

*Note that `runserver` is only for debug builds and local testing. Specialised server programs (such as Apache) should always be used in production.*

**Adding a Django App**

A Django project usually contains multiple `apps`. This is simply a way to structure your project in smaller, maintainable modules. To create an app, go to your projectfolder (where `manage.py` is), and run the `startapp` command (change *myapp* to whatever you want):

    python manage.py startapp myapp

This will generate the *myapp* folder and some necessary files for you, like `models.py` and `views.py`.

In order to make Django aware of *myapp*, add it to your `settings.py`:

    # myproject/settings.py

    # Application definition
    INSTALLED_APPS = [
        ...
        'myapp',
    ]

The folder-structure of a Django project can be changed to fit your preference. Sometimes the project folder is renamed to `/src` to avoid repeating folder names. A typical folder structure looks like this:

[![directory structure][2]][2]


  [1]: http://i.stack.imgur.com/sABAE.png
  [2]: http://i.stack.imgur.com/LRsRO.png

## Virtual Environment
Although not strictly required, it is highly recommended to start your project in a "virtual environment." A virtual environment is a **container** (a directory) that holds a specific version of Python and a set of modules (dependencies), and which does not interfere with the operating system's native Python or other projects on the same computer.

By setting up a different virtual environment for each project you work on, various Django projects can run on different versions of Python, and can maintain their own sets of dependencies, without risk of conflict.


Python 3.3+
===========

Python 3.3+ already includes a standard `venv` module, which you can usually call as `pyvenv`. In environments where the `pyvenv` command is not available, you can access the same functionality by directly invoking the module as `python3 -m venv`.

To create the Virtual environment:

    $ pyvenv <env-folder>
    # Or, if pyvenv is not available
    $ python3 -m venv <env-folder>

Python 2
========

If using Python 2, you can first install it as a separate module from pip:

    $ pip install virtualenv

And then create the environment using the `virtualenv` command instead:

    $ virtualenv <env-folder>


Activate (any version)
======================

The virtual environment is now set up. In order to use it, it must be *activated* in the terminal you want to use it. 

To 'activate' the virtual environment (any Python version) 

Linux like:

    $ source <env-folder>/bin/activate

Windows like:

    <env-folder>\Scripts\activate.bat

This changes your prompt to indicate the virtual environment is active. 
`(<env-folder>) $`

From now on, everything installed using `pip` will be installed to your virtual env folder, not system-wide. 


To leave the virtual environment use `deactivate` :

    (<env-folder>) $ deactivate


Alternatively: use virtualenvwrapper
====================================

You may also consider using [virtualenvwrapper][1] which makes virtualenv creation and activation very handy as well as separating it from your code:

    # Create a virtualenv
    mkvirtualenv my_virtualenv

    # Activate a virtualenv
    workon my_virtualenv

    # Deactivate the current virtualenv
    deactivate

Alternatively: use pyenv + pyenv-viritualenv
====================================
In environments where you need to handle multiple Python versions you can benefit from virtualenv together with pyenv-virtualenv:

    # Create a virtualenv for specific Python version
    pyenv virtualenv 2.7.10 my-virtual-env-2.7.10
    
    # Create a vritualenv for active python verion
    pyenv virtualenv venv34
    
    # Activate, deactivate virtualenv
    pyenv activate <name>
    pyenv deactivate

When using virtualenvs, it is often useful to set your `PYTHONPATH` and `DJANGO_SETTINGS_MODULE` in the [`postactivate` script][2].

    #!/bin/sh
    # This hook is sourced after this virtualenv is activated

    # Set PYTHONPATH to isolate the virtualenv so that only modules installed
    # in the virtualenv are available
    export PYTHONPATH="/home/me/path/to/your/project_root:$VIRTUAL_ENV/lib/python3.4"

    # Set DJANGO_SETTINGS_MODULE if you don't use the default `myproject.settings`
    # or if you use `django-admin` rather than `manage.py`
    export DJANGO_SETTINGS_MODULE="myproject.settings.dev"

Set your Project Path
=====================

It is often also helpful to set your project path inside a special `.project` file located in your base `<env-folder>`. When doing this, everytime you activate your virtual environment, it will change the active directory to the specified path.

Create a new file called `<env-folder>/.project`. The contents of the file should ONLY be the path of the project directory.

    /path/to/project/directory

Now, initiate your virtual environment (either using `source <env-folder>/bin/activate` or `workon my_virtualenv`) and your terminal will change directories to `/path/to/project/directory`.

[1]: http://virtualenvwrapper.readthedocs.io/
[2]: http://virtualenvwrapper.readthedocs.io/en/latest/scripts.html#postactivate


## Django Concepts
**django-admin** is a command line tool that ships with Django. It comes with [several useful commands][1] for getting started with and managing a Django project.
The command is the same as `./manage.py` , with the difference that you don't need to be in the project directory. The `DJANGO_SETTINGS_MODULE` environment variable needs to be set.

A **Django project** is a Python codebase that contains a Django settings file.  A project can be created by the Django admin through the command [`django-admin startproject NAME`][2].  The project typically has a file called `manage.py` at the top level and a root URL file called `urls.py`.  `manage.py` is a project specific version of `django-admin`, and lets you run management commands on that project.  For example, to run your project locally, use `python manage.py runserver`. A project is made up of Django apps.

A **Django app** is a Python package that contains a models file (`models.py` by default) and other files such as app-specific urls and views.  An app can be created through the command [`django-admin startapp NAME`][3] (this command should be run from inside your project directory).  For an app to be part of a project, it must be included in the [`INSTALLED_APPS`][4] list in `settings.py`.  If you used the standard configuration, Django comes with several apps of it's own apps preinstalled which will handle things like [authentication][5] for you. Apps can be used in multiple Django projects.

The **Django ORM** collects all of the database models defined in `models.py` and creates database tables based on those model classes.  To do this, first, setup your database by modifying the [`DATABASES`][6] setting in `settings.py`.  Then, once you have defined your [database models][7], run [`python manage.py makemigrations`][8] followed by [`python manage.py migrate`][9] to create or update your database's schema based on your models.


  [1]: https://docs.djangoproject.com/en/stable/ref/django-admin/
  [2]: https://docs.djangoproject.com/en/stable/ref/django-admin/#startproject
  [3]: https://docs.djangoproject.com/en/stable/ref/django-admin/#startapp
  [4]: https://docs.djangoproject.com/en/stable/ref/settings/#installed-apps
  [5]: https://docs.djangoproject.com/en/stable/topics/auth/
  [6]: https://docs.djangoproject.com/en/stable/ref/settings/#databases
  [7]: https://docs.djangoproject.com/en/stable/topics/db/models/
  [8]: https://docs.djangoproject.com/en/stable/ref/django-admin/#makemigrations
  [9]: https://docs.djangoproject.com/en/stable/ref/django-admin/#migrate

## Single File Hello World Example
This example shows you a minimal way to create a Hello World page in Django. This will help you realize that the `django-admin startproject example` command basically creates a bunch of folders and files and that you don't necessarily need that structure to run your project.

1. Create a file called `file.py`.

2. Copy and paste the following code in that file.

        import sys
        
        from django.conf import settings
        
        settings.configure(
            DEBUG=True,
            SECRET_KEY='thisisthesecretkey',
            ROOT_URLCONF=__name__,
            MIDDLEWARE_CLASSES=(
                'django.middleware.common.CommonMiddleware',
                'django.middleware.csrf.CsrfViewMiddleware',
                'django.middleware.clickjacking.XFrameOptionsMiddleware',
            ),
        )
        
        from django.conf.urls import url
        from django.http import HttpResponse
        
        # Your code goes below this line.

        def index(request):
            return HttpResponse('Hello, World!')
    
        urlpatterns = [
            url(r'^$', index),
        ]
    
        # Your code goes above this line
        
        if __name__ == "__main__":
            from django.core.management import execute_from_command_line
        
            execute_from_command_line(sys.argv)
    
3. Go to the terminal and run the file with this command `python file.py runserver`.

4. Open your browser and go to [127.0.0.1:8000][1].


  [1]: http://127.0.0.1:8000

## A complete hello world example.
**Step 1**
If you already have Django installed, you can skip this step.

    pip install Django

**Step 2**
Create a new project

    django-admin startproject hello

That will create a folder named `hello` which will contain the following files:

    hello/
    ├── hello/
    │   ├── __init__.py
    │   ├── settings.py
    │   ├── urls.py
    │   └── wsgi.py
    └── manage.py


**Step 3**
Inside the `hello` module (the folder containing the `__init.py__`) create a file called `views.py`:

    hello/
    ├── hello/
    │   ├── __init__.py
    │   ├── settings.py
    │   ├── urls.py
    │   ├── views.py  <- here
    │   └── wsgi.py
    └── manage.py

 and put in the following content:

    from django.http import HttpResponse

    def hello(request):
        return HttpResponse('Hello, World')

This is called a view function.

**Step 4**
Edit `hello/urls.py` as follows:

    from django.conf.urls import url
    from django.contrib import admin
    from hello import views
    
    urlpatterns = [
        url(r'^admin/', admin.site.urls),
        url(r'^$', views.hello)
    ]

which links the view function `hello()` to a URL.

**Step 5**
Start the server.
   
    python manage.py runserver

**Step 6**

Browse to `http://localhost:8000/` in a browser and you will see: 

> Hello, World


## Deployment friendly Project with Docker support.
The default Django project template is fine but once you get to deploy your code and for example devops put their hands on the project things get messy. What you can do is separate your source code from the rest that is required to be in your repository.

> You can find a usable Django project template on [GitHub](https://github.com/pkucmus/django-project-template).

# Project Structure

    PROJECT_ROOT
    ├── devel.dockerfile
    ├── docker-compose.yml
    ├── nginx
    │   └── project_name.conf
    ├── README.md
    ├── setup.py
    └── src
        ├── manage.py
        └── project_name
            ├── __init__.py
            └── service
                ├── __init__.py
                ├── settings
                │   ├── common.py
                │   ├── development.py
                │   ├── __init__.py
                │   └── staging.py
                ├── urls.py
                └── wsgi.py

I like to keep the `service` directory named `service` for every project thanks to that I can use the same `Dockerfile` across all my projects.
The split of requirements and settings are already well documented here:  
https://www.wikiod.com/django/settings#Using multiple requirements files  
https://www.wikiod.com/django/settings#Using multiple settings

# Dockerfile

With the assumption that only developers make use of Docker (not every dev ops trust it these days). This could be a dev environment `devel.dockerfile`:

    FROM python:2.7
    ENV PYTHONUNBUFFERED 1

    RUN mkdir /run/service
    ADD . /run/service
    WORKDIR /run/service

    RUN pip install -U pip
    RUN pip install -I -e .[develop] --process-dependency-links

    WORKDIR /run/service/src
    ENTRYPOINT ["python", "manage.py"]
    CMD ["runserver", "0.0.0.0:8000"]


Adding only requirements will leverage Docker cache while building - you only need to rebuild on requirements change.

# Compose

Docker compose comes in handy - especially when you have multiple services to run locally. `docker-compose.yml`:

    version: '2'
    services:
      web:
        build:
          context: .
          dockerfile: devel.dockerfile
        volumes:
          - "./src/{{ project_name }}:/run/service/src/{{ project_name }}"
          - "./media:/run/service/media"
        ports:
          - "8000:8000"
        depends_on:
          - db
      db:
        image: mysql:5.6
        environment:
          - MYSQL_ROOT_PASSWORD=root
          - MYSQL_DATABASE={{ project_name }}
      nginx:
        image: nginx
        ports:
          - "80:80"
        volumes:
          - "./nginx:/etc/nginx/conf.d"
          - "./media:/var/media"
        depends_on:
          - web
# Nginx

Your development environment should be as close to the prod environment as possible so I like using Nginx from the start. Here is an example nginx configuration file:

    server {
        listen   80;
        client_max_body_size 4G;
        keepalive_timeout 5;

        location /media/ {
            autoindex on;
            alias /var/media/;
        }

        location / {
            proxy_pass_header Server;
            proxy_set_header Host $http_host;
            proxy_redirect off;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Scheme $scheme;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Ssl on;
            proxy_connect_timeout 600;
            proxy_read_timeout 600;
            proxy_pass http://web:8000/;
        }
    }


# Usage

    $ cd PROJECT_ROOT
    $ docker-compose build web  # build the image - first-time and after requirements change
    $ docker-compose up  # to run the project
    $ docker-compose run --rm --service-ports --no-deps  # to run the project - and be able to use PDB
    $ docker-compose run --rm --no-deps <management_command>  # to use other than runserver commands, like makemigrations
    $ docker exec -ti web bash  # For accessing django container shell, using it you will be inside /run/service directory, where you can run ./manage shell, or other stuff
    $ docker-compose start  # Starting docker containers
    $ docker-compose stop  # Stopping docker containers

