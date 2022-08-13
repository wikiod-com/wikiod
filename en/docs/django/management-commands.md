---
title: "Management Commands"
slug: "management-commands"
draft: false
images: []
weight: 8419
type: docs
toc: true
---

Management commands are powerful and flexible scripts that can perform actions on your Django project or the underlying database. In addition to various default commands, it's possible to write your own!

Compared to regular Python scripts, using the management command framework means that some tedious setup work is automatically done for you behind the scenes.

Management commands can be called either from:

  - `django-admin <command> [options]`
  - `python -m django <command> [options]`
  - `python manage.py <command> [options]`
  - `./manage.py <command> [options]` if manage.py has execution permissions (`chmod +x manage.py`)
   
To use management commands with Cron:

    */10 * * * * pythonuser /var/www/dev/env/bin/python /var/www/dev/manage.py <command> [options] > /dev/null

## Creating and Running a Management Command
To perform actions in Django using commandline or other services (where the user/request is not used), you can use the `management commands`.  

Django modules can be imported as needed.

For each command a separate file needs to be created: 
 `myapp/management/commands/my_command.py` \
(The `management` and `commands` directories must have an empty \_\_init__.py file)

    from django.core.management.base import BaseCommand, CommandError

    # import additional classes/modules as needed
    # from myapp.models import Book
    
    class Command(BaseCommand):
        help = 'My custom django management command'
    
        def add_arguments(self, parser):
           parser.add_argument('book_id', nargs='+', type=int)
           parser.add_argument('author' , nargs='+', type=str)

        def handle(self, *args, **options):
           bookid = options['book_id'] 
           author = options['author']
           # Your code goes here
            
            # For example:
            # books = Book.objects.filter(author="bob")
            # for book in books:
            #    book.name = "Bob"
            #    book.save()

Here class name **Command** is mandatory which extends **BaseCommand** or one of its subclasses.

The name of the management command is the name of the file containing it.  To run the command in the example above, use the following in your project directory:
    
    python manage.py my_command

> Note that starting a command can take a few second (because of the
> import of the modules). So in some cases it is advised to create
> `daemon` processes instead of `management commands`.

 

[More on management commands][1]


  [1]: https://docs.djangoproject.com/en/dev/howto/custom-management-commands/

## Builtin Management Commands
 Django comes with a number of builtin management commands, using `python manage.py [command]` or, when manage.py has +x (executable) rights simply `./manage.py [command]` . 
The following are some of the most frequently used:

Get a list of all available commands

    ./manage.py help

Run your Django server on localhost:8000; essential for local testing

    ./manage.py runserver

Run a python (or ipython if installed) console with the Django settings of your project preloaded (attempting to access parts of your project in a python terminal without doing this will fail).

    ./manage.py shell

Create a new database migration file based on the changes you have made to your models. See [Migrations][1]

    ./manage.py makemigrations

Apply any unapplied migrations to the current database.

    ./manage.py migrate

Run your project's test suite. See [Unit Testing][2]

    ./manage.py test

Take all of the static files of your project and puts them in the folder specified in `STATIC_ROOT` so they can be served in production.

    ./manage.py collectstatic

Allow to create superuser.

    ./manage.py createsuperuser

Change the password of a specified user.

    ./manage.py changepassword username


[Full list of available commands][3]


  [1]: https://www.wikiod.com/django/migrations
  [2]: https://www.wikiod.com/django/unit-testing
  [3]: https://docs.djangoproject.com/en/stable/ref/django-admin/#available-commands.

## Using django-admin instead of manage.py
You can get rid of `manage.py` and use the `django-admin` command instead. To do so, you will have to manually do what `manage.py` does:

- Add your project path to your PYTHONPATH
- Set the DJANGO_SETTINGS_MODULE


    export PYTHONPATH="/home/me/path/to/your_project"
    export DJANGO_SETTINGS_MODULE="your_project.settings"

This is especially useful in a [virtualenv][1] where you can set these environment variables in the `postactivate` script.

 [1]: https://www.wikiod.com/django/getting-started-with-django#Virtual Environment

`django-admin` command has the advantage of being available wherever you are on your filesystem.

## Get list of existing commands
You can get list of available commands by following way: 

    >>> python manage.py help
If you don't understand any command or looking for optional arguments then you can use **-h** argument like this

    >>> python manage.py command_name -h
Here command_name will be your desire command name, this will show you help text from the command.

    >>> python manage.py runserver -h          
    >>> usage: manage.py runserver [-h] [--version] [-v {0,1,2,3}]
                               [--settings SETTINGS] [--pythonpath PYTHONPATH]
                               [--traceback] [--no-color] [--ipv6] [--nothreading]
                               [--noreload] [--nostatic] [--insecure]
                               [addrport]
    
    Starts a lightweight Web server for development and also serves static files.
    
    positional arguments:
      addrport              Optional port number, or ipaddr:port
    
    optional arguments:
      -h, --help            show this help message and exit
      --version             show program's version number and exit
      -v {0,1,2,3}, --verbosity {0,1,2,3}
                            Verbosity level; 0=minimal output, 1=normal output,
                            2=verbose output, 3=very verbose output
      --settings SETTINGS   The Python path to a settings module, e.g.
                            "myproject.settings.main". If this isn't provided, the
                            DJANGO_SETTINGS_MODULE environment variable will be
                            used.
      --pythonpath PYTHONPATH
                            A directory to add to the Python path, e.g.
                            "/home/djangoprojects/myproject".
      --traceback           Raise on CommandError exceptions
      --no-color            Don't colorize the command output.
      --ipv6, -6            Tells Django to use an IPv6 address.
      --nothreading         Tells Django to NOT use threading.
      --noreload            Tells Django to NOT use the auto-reloader.
      --nostatic            Tells Django to NOT automatically serve static files
                            at STATIC_URL.
      --insecure            Allows serving static files even if DEBUG is False.
[List of available command][1]


  [1]: https://docs.djangoproject.com/en/1.9/ref/django-admin/#available-commands.

