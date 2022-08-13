---
title: "Async Tasks (Celery)"
slug: "async-tasks-celery"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

Celery is a task queue which can run background or scheduled jobs and integrates with Django pretty well. 
Celery requires something known as **message broker** to pass messages from invocation to the workers. This message broker can be redis, rabbitmq or even Django ORM/db although that is not a recommended approach. 

Before you get started with the example, You will have to configure celery.  To configure celery, create a `celery_config.py` file in the main app, parallel to the `settings.py` file.

    from __future__ import absolute_import
    import os
    from celery import Celery
    from django.conf import settings
    
    # broker url 
    BROKER_URL = 'redis://localhost:6379/0'
    
    # Indicate Celery to use the default Django settings module
    os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'config.settings')
    
    app = Celery('config')
    app.config_from_object('django.conf:settings')
    # if you do not need to keep track of results, this can be turned off
    app.conf.update(
        CELERY_RESULT_BACKEND=BROKER_URL,
    )
    
    # This line will tell Celery to autodiscover all your tasks.py that are in your app folders
    app.autodiscover_tasks(lambda: settings.INSTALLED_APPS)


And in the main app's `__init__.py` file import the celery app. like this

    # -*- coding: utf-8 -*- 
    # Not required for Python 3. 
    from __future__ import absolute_import
    
    from .celery_config import app as celery_app  # noqa

To run celery worker, use this command at the level where manage.py is. 

```
# pros is your django project, 
celery -A proj worker -l info
```

## Simple example to add 2 numbers
To get started:

1. Install celery `pip install celery`
2. configure celery (head to the remarks section)


    from __future__ import absolute_import, unicode_literals
    
    from celery.decorators import task
    
    
    @task
    def add_number(x, y):
        return x + y


You can run this asynchronously by using the `.delay()` method. 

`add_number.delay(5, 10)`, where 5 and 10 are the arguments for the function `add_number`

To check if the async function has finished the operation, you can use the `.ready()` function on the async object returned by the `delay` method.

To fetch the result of the computation, you can use the `.result` attribute on the async object.

**Example**

    async_result_object = add_number.delay(5, 10)
    if async_result_object.ready():
        print(async_result_object.result)



