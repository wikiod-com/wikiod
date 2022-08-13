---
title: "Using Redis with Django - Caching Backend"
slug: "using-redis-with-django---caching-backend"
draft: false
images: []
weight: 9940
type: docs
toc: true
---

Using django-redis-cache or django-redis are both effective solutions for storing all cached items. While it is certainly possible for Redis to be setup directly as a `SESSION_ENGINE`, one effective strategy is to setup the caching (as above) and declare your default cache as a `SESSION_ENGINE`. While this is really the topic for another documentaiton article, its relevance leads to inclusion.

Simply add the following to `settings.py`:

    SESSION_ENGINE = "django.contrib.sessions.backends.cache"


## Using django-redis-cache
One potential implementation of Redis as a backend caching utility is the [django-redis-cache][1] package.

This example assumes you already have [a Redis server operating][2].

    $ pip install django-redis-cache

Edit your `settings.py` to include a `CACHES` object (see [Django documentation on caching][3]).

<!-- language: lang-python -->

    CACHES = {
        'default': {
            'BACKEND': 'redis_cache.RedisCache',
            'LOCATION': 'localhost:6379',
            'OPTIONS': {
                'DB': 0,
            }
        }
    }


  [1]: http://django-redis-cache.readthedocs.io/en/latest/intro_quick_start.html#
  [2]: https://www.wikiod.com/redis/getting-started-with-redis#Redis "Hello World"
  [3]: https://docs.djangoproject.com/en/1.9/topics/cache/

## Using django-redis
One potential implementation of Redis as a backend caching utility is the [django-redis][1] package.

This example assumes you already have [a Redis server operating][2].

    $ pip install django-redis

Edit your `settings.py` to include a `CACHES` object (see [Django documentation on caching][3]).

<!-- language: lang-python -->

    CACHES = {
        'default': {
            'BACKEND': 'django_redis.cache.RedisCache',
            'LOCATION': 'redis://127.0.0.1:6379/1',
            'OPTIONS': {
                'CLIENT_CLASS': 'django_redis.client.DefaultClient',
            }
        }
    }


  [1]: http://niwinz.github.io/django-redis/latest/
  [2]: https://www.wikiod.com/redis/getting-started-with-redis#Redis "Hello World"
  [3]: https://docs.djangoproject.com/en/1.10/topics/cache/

