---
title: "Settings"
slug: "settings"
draft: false
images: []
weight: 9727
type: docs
toc: true
---

## Hiding secret data using a JSON file
When using a VCS such as Git or SVN, there are some secret data that must never be versioned (whether the repository is public or private).

Among those data, you find the `SECRET_KEY` setting and the database password.

A common practice to hide these settings from version control is to create a file `secrets.json` at the root of your project ([thanks "*Two Scoops of Django*" for the idea][1]):

    {
        "SECRET_KEY": "N4HE:AMk:.Ader5354DR453TH8SHTQr",
        "DB_PASSWORD": "v3ry53cr3t"
    }

And add it to your ignore list (`.gitignore` for git):

    *.py[co]
    *.sw[po]
    *~
    /secrets.json

Then add the following function to your `settings` module:

    import json
    import os
    from django.core.exceptions import ImproperlyConfigured

    with open(os.path.join(BASE_DIR, 'secrets.json')) as secrets_file:
        secrets = json.load(secrets_file)
    
    def get_secret(setting, secrets=secrets):
        """Get secret setting or fail with ImproperlyConfigured"""
        try:
            return secrets[setting]
        except KeyError:
            raise ImproperlyConfigured("Set the {} setting".format(setting))

Then fill the settings this way:

    SECRET_KEY = get_secret('SECRET_KEY')
    DATABASES = {
        'default': {
            'ENGINE': 'django.db.backends.postgres',
            'NAME': 'db_name',
            'USER': 'username',
            'PASSWORD': get_secret('DB_PASSWORD'),
        },
    }

*Credits: [Two Scoops of Django: Best Practices for Django 1.8, by Daniel Roy Greenfeld and Audrey RoyGreenfeld. Copyright 2015 Two Scoops Press (ISBN 978-0981467344)][2]*

 [1]: https://github.com/twoscoops/two-scoops-of-django-1.8/blob/master/code/chapter_05_example_19.py
 [2]: https://www.twoscoopspress.com/

## Using Environment variables to manage Settings across servers
Using environment variables is a widely used way to setting an app's config depending on it environment, as stated in [The Twelve-Factor App](http://12factor.net/config).

As configurations are likely to change between deployment environments, this is a very interesting way to modify the configuration without having to dig in the app's source code, as well as keeping secrets outside the application files and source code repository.

In Django, the main settings are located as `settings.py` in your project's folder. As it is a simple Python file, you can use Python's `os` module from the standard library to access the environment (and even have appropriate defaults).

## settings.py

<!-- language lang-python -->
    import os
    
    SECRET_KEY = os.environ.get('APP_SECRET_KEY', 'unsafe-secret-key')
    
    DEBUG = bool(os.environ.get('DJANGO_DEBUG', True) == 'False')

    ALLOWED_HOSTS = os.environ.get('DJANGO_ALLOWED_HOSTS', '').split()

    DATABASES = {
        'default': {
            'ENGINE': os.environ.get('APP_DB_ENGINE', 'django.db.backends.sqlite3'),
            'NAME': os.environ.get('DB_NAME', 'db.sqlite'),    
            'USER': os.environ.get('DB_USER', ''),
            'PASSWORD': os.environ.get('DB_PASSWORD', ''),
            'HOST': os.environ.get('DB_HOST', None),
            'PORT': os.environ.get('DB_PORT', None),
            'CONN_MAX_AGE': 600,
        }
    }

With Django you can change your database technology, so that you can use sqlite3 on your development machine (and that should be a sane default for committing to a source control system). Although this is possible it is not advisable:
> Backing services, such as the app’s database, queueing system, or cache, is one area where dev/prod parity is important. ([The Twelve-Factor App - Dev/prod parity][1])

For using a DATABASE_URL parameter for database connection, please take a look at the [related example][2].


  [1]: https://12factor.net/dev-prod-parity
  [2]: https://www.wikiod.com/django/settings#Using a DATABASE_URL from the environment

## Using BASE_DIR to ensure app portability
It's a bad idea to hard code paths in your application. One should always use relative urls so that your code can work seamlessly across different machines.
The best way to set this up is to define a variable like this
    
    import os
    BASE_DIR = os.path.dirname(os.path.dirname(__file__))

Then use this `BASE_DIR` variable to define all your other settings.

    TEMPLATE_PATH = os.path.join(BASE_DIR, "templates")
    STATICFILES_DIRS = [
        os.path.join(BASE_DIR, "static"),

    ]

And so on. This ensures that you can port your code across different machines without any worries.

However, `os.path` is a bit verbose. For instance if your settings module is `project.settings.dev`, you will have to write:

    BASE_DIR = os.path.dirname(os.path.dirname(os.path.dirname(__file__)))

An alternative is to use the `unipath` module (which you can install with `pip install unipath`).

    from unipath import Path

    BASE_DIR = Path(__file__).ancestor(2)  # or ancestor(3) if using a submodule

    TEMPLATE_PATH = BASE_DIR.child('templates')
    STATICFILES_DIRS = [
        BASE_DIR.child('static'),
    ]

## Using multiple settings
Django default project layout creates a single `settings.py`. This is often useful to split it like this:

    myprojectroot/
        myproject/
            __init__.py
            settings/
                __init__.py
                base.py
                dev.py
                prod.py
                tests.py

This enables you to work with different settings according to whether you are in development, production, tests or whatever.

When moving from the default layout to this layout, the original `settings.py` becomes `settings/base.py`. When every other submodule will "subclass" `settings/base.py` by starting with `from .base import *`. For instance, here is what `settings/dev.py` may look like:

    # -*- coding: utf-8 -*-
    from .base import *  # noqa

    DEBUG = True
    INSTALLED_APPS.extend([
        'debug_toolbar',
    ])
    EMAIL_BACKEND = 'django.core.mail.backends.console.EmailBackend'
    INTERNAL_IPS = ['192.168.0.51', '192.168.0.69']

# Alternative #1

For `django-admin` commands to work properly, you will have to set `DJANGO_SETTINGS_MODULE` environment variable (which defaults to `myproject.settings`). In development, you will set it to `myproject.settings.dev`. In production, you will set it to `myproject.settings.prod`. If you use a virtualenv, best is to set it in your `postactivate` script:

    #!/bin/sh
    export PYTHONPATH="/home/me/django_projects/myproject:$VIRTUAL_ENV/lib/python3.4"
    export DJANGO_SETTINGS_MODULE="myproject.settings.dev"

If you want to use a settings module that is not pointed by `DJANGO_SETTINGS_MODULE` for one time, you can use the `--settings` option of `django-admin`:

    django-admin test --settings=myproject.settings.tests

# Alternative #2

If you want to leave `DJANGO_SETTINGS_MODULE` at its default configuration (`myproject.settings`), you can simply tell the `settings` module which configuration to load by placing the import in your `__init__.py` file.

In the above example, the same result could be achieved by having an `__init__.py` set to:

    from .dev import *

## Using multiple requirements files
Each requirements files should match the name of a settings files. Read [Using multiple settings][1] for more information.

# Structure
    
    djangoproject
    ├── config
    │   ├── __init__.py
    │   ├── requirements
    │   │   ├── base.txt
    │   │   ├── dev.txt
    │   │   ├── test.txt
    │   │   └── prod.txt
    │   └── settings
    └── manage.py

In `base.txt` file, place dependencies used in all environments. 
    
    # base.txt
    Django==1.8.0
    psycopg2==2.6.1
    jinja2==2.8

And in all other files, include base dependencies with `-r base.txt`, and add  specific dependencies needed for the current environment.

<!-- -->
    # dev.txt
    -r base.txt # includes all dependencies in `base.txt`

    # specific dependencies only used in dev env
    django-queryinspect==0.1.0

<!-- -->
    # test.txt
    -r base.txt # includes all dependencies in `base.txt`

    # specific dependencies only used in test env
    nose==1.3.7
    django-nose==1.4

<!-- -->

    # prod.txt
    -r base.txt # includes all dependencies in `base.txt`

    # specific dependencies only used in production env
    django-queryinspect==0.1.0
    gunicorn==19.3.0
    django-storages-redux==1.3
    boto==2.38.0

Finally, to install dependencies. Example, on dev env : `pip install -r config/requirements/dev.txt`


  [1]: https://www.wikiod.com/django/settings#Using multiple settings

## Setting the timezone
You can set the timezone that will be used by Django in the `settings.py` file. Examples:

    TIME_ZONE = 'UTC'  # use this, whenever possible
    TIME_ZONE = 'Europe/Berlin'
    TIME_ZONE = 'Etc/GMT+1'


[Here is the list of valid timezones](http://en.wikipedia.org/wiki/List_of_tz_database_time_zones)

*When running in a **Windows** environment this must be set to the same as your **system time zone**.*

If you do not want Django to use timezone-aware datetimes:     

    USE_TZ = False


Django best practices call for using `UTC` for storing information in the database:

> Even if your website is available in only one time zone, it’s still
> good practice to store data in UTC in your database. The main reason
> is Daylight Saving Time (DST). Many countries have a system of DST,
> where clocks are moved forward in spring and backward in autumn. If
> you’re working in local time, you’re likely to encounter errors twice
> a year, when the transitions happen.

https://docs.djangoproject.com/en/stable/topics/i18n/timezones/

## Accessing settings
Once you've got all your settings, you'll want to use them in your code. To do so, add the following import to your file:

    from django.conf import settings

You may then access your settings as attributes of the `settings` module, for example:

    if not settings.DEBUG:
        email_user(user, message)

## Using a DATABASE_URL from the environment
In PaaS sites such as Heroku, it is usual to receive the database information as a single URL environment variable, instead of several parameters (host, port, user, password...).

There is a module, [`dj_database_url`](https://github.com/kennethreitz/dj-database-url) which automatically extracts the DATABASE_URL environment variable to a Python dictionary appropriate for injecting the database settings in Django.

Usage:

<!-- language lang-python -->
    import dj_database_url

    if os.environ.get('DATABASE_URL'):
        DATABASES['default'] =
            dj_database_url.config(default=os.environ['DATABASE_URL'])

