---
title: "Debugging"
slug: "debugging"
draft: false
images: []
weight: 9934
type: docs
toc: true
---

**Pdb**

Pdb can also print out all existing variables in global or local scope, by typing `globals()` or `locals()` in (Pdb) prompt respectively.

## Using Django Debug Toolbar
First, you need to install [django-debug-toolbar][1]:

    pip install django-debug-toolbar

**settings.py**:

Next, include it to project's installed apps, but be careful - it's always a good practice to use a different `settings.py` file for such development-only apps and middlewares as debug toolbar:

    # If environment is dev...
    DEBUG = True

    INSTALLED_APPS += [
        'debug_toolbar',
    ]

    MIDDLEWARE += ['debug_toolbar.middleware.DebugToolbarMiddleware']

Debug toolbar also relies on static files, so appropriate app should be included as well:

    INSTALLED_APPS = [
        # ...
        'django.contrib.staticfiles',
        # ...
    ]
    
    STATIC_URL = '/static/'

    # If environment is dev...
    DEBUG = True

    INSTALLED_APPS += [
        'debug_toolbar',
    ]

In some cases, it's also required to set `INTERNAL_IPS` in `settings.py`:

    INTERNAL_IPS = ('127.0.0.1', )

**urls.py**:

In `urls.py`, as official documentation suggests, the next snippet should enable debug toolbar routing:

    if settings.DEBUG and 'debug_toolbar' in settings.INSTALLED_APPS:
        import debug_toolbar
        urlpatterns += [
            url(r'^__debug__/', include(debug_toolbar.urls)),
        ]

Collect toolbar's static after installation:

    python manage.py collectstatic

That's it, debug toolbar will appear on you project's pages, providing various useful information about execution time, SQL, static files, signals, etc.

**HTML:**

Also, `django-debug-toolbar` requires a *Content-type* of `text/html`, `<html>` and `<body>` tags to render properly.

---

**In case if you sure you've configured everything right, but debug toolbar is still not rendered:** use [this "nuclear" solution][2] to try to figure it out. 

  [1]: https://github.com/jazzband/django-debug-toolbar
  [2]: http://stackoverflow.com/questions/10517765/django-debug-toolbar-not-showing-up

## Using Python Debugger (Pdb)
Most basic Django debugging tool is [pdb][1], a part of Python standard library.

**Init view script**

Let's examine a simple `views.py` script:

    from django.http import HttpResponse


    def index(request):
        foo = 1
        bar = 0
    
        bug = foo/bar
    
        return HttpResponse("%d goes here." % bug)

Console command to run server:

    python manage.py runserver

It's obvious that Django would throw a `ZeroDivisionError` when you try to load index page, but if we'll pretend that the bug is very deep in the code, it could get really nasty.

**Setting a breakpoint**

Fortunately, we can set a *breakpoint* to trace down that bug:

    from django.http import HttpResponse
    
    # Pdb import
    import pdb


    def index(request):
        foo = 1
        bar = 0
        
        # This is our new breakpoint
        pdb.set_trace()
        
        bug = foo/bar
        
        return HttpResponse("%d goes here." % bug)

Console command to run server with pdb:

    python -m pdb manage.py runserver

Now on page load breakpoint will trigger (Pdb) prompt in the shell, which will also hang your browser in pending state.

**Debugging with pdb shell**

It's time to debug that view by interacting with script via shell:
    
    > ../views.py(12)index()
    -> bug = foo/bar
    # input 'foo/bar' expression to see division results:
    (Pdb) foo/bar
    *** ZeroDivisionError: division by zero
    # input variables names to check their values:
    (Pdb) foo
    1
    (Pdb) bar
    0
    # 'bar' is a source of the problem, so if we set it's value > 0...
    (Pdb) bar = 1
    (Pdb) foo/bar
    1.0
    # exception gone, ask pdb to continue execution by typing 'c':
    (Pdb) c
    [03/Aug/2016 10:50:45] "GET / HTTP/1.1" 200 111

In the last line we see that our view returned an `OK` response and executing as it should.

To stop pdb loop, just input `q` in a shell.

  [1]: https://docs.python.org/3.6/library/pdb.html

## Using "assert False"
While developing, inserting the following line to your code:

    assert False, value

will cause django to raise an `AssertionError` with the value supplied as an error message when this line is executed.

If this occurs in a view, or in any code called from a view, and `DEBUG=True` is set, a full and detailed stacktrace with a lot of debugging information will be displayed in the browser.

Don't forget to remove the line when you are done!

## Consider Writing More Documentation, Tests, Logging and Assertions Instead of Using a Debugger
Debugging takes time and effort.

Instead of chasing bugs with a debugger, consider spending more time on making your code better by:

* **Write and run [Tests][1].**  Python and Django have great builtin testing frameworks - that can be used to test your code much faster than manually with a debugger.
* **Writing proper documentation** for your functions, classes and modules.  [PEP 257][2] and 
[Google's Python Style Guide][3] supplies good practices for writing good docstrings.
* **[Use Logging][4]** to produce output from your program - during development and after deploying.
* **Add `assert`ions** to your code in important places: Reduce ambiguity, catch problems as they are created.

Bonus: Write [doctests][5] for combining documentation and testing!



  [1]: https://docs.djangoproject.com/en/1.10/topics/testing/
  [2]: https://www.python.org/dev/peps/pep-0257/
  [3]: https://google.github.io/styleguide/pyguide.html?showone=Comments#Comments
  [4]: https://docs.djangoproject.com/en/1.10/topics/logging/
  [5]: https://docs.python.org/3/library/doctest.html

