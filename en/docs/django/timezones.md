---
title: "Timezones"
slug: "timezones"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Timezones are often a hassle for developers. Django offers some great utilities at your disposal to make timezones easy to work with.

Even if your project is operating in a single time zone, it is still good practice to store data as UTC in your database to handle cases of daylight savings. If you are operating on multiple timezones then storing time data as UTC is a must.

## Enable Time Zone Support
First is first, ensure that `USE_TZ = True` in your `settings.py` file. Also set a default time zone value to `TIME_ZONE` such as `TIME_ZONE='UTC'`. View a complete list of timezones [here][1]. 

If `USE_TZ` is False, `TIME_ZONE` will be the time zone that Django will use to store all datetimes. When `USE_TZ` is enabled,  `TIME_ZONE` is the default time zone that Django will use to display datetimes in templates and to interpret datetimes entered in forms.


With time zone support enabled, django will store `datetime` data in the database as the time zone `UTC`

  [1]: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones

## Setting Session Timezones
Python's `datetime.datetime` objects have a `tzinfo` attribute that is used to store time zone information. When the attribute is set the object is considered Aware, when the attribute is not set it is considered a Naive.

To ensure that a timezone is naive or aware, you can use [`.is_naive()`][4] and [`.is_aware()`][1]

If you have `USE_TZ` enabled in your `settings.py` file, a `datetime` will have time zone information attached to it as long as your default `TIME_ZONE` is set in `settings.py`

While this default time zone may be good in some cases it is likely not enough especially if you are handling users in multiple time zones. In order to accomplish this, middleware must be used.


    import pytz

    from django.utils import timezone

    # make sure you add `TimezoneMiddleware` appropriately in settings.py
    class TimezoneMiddleware(object):
        """
        Middleware to properly handle the users timezone
        """
    
        def __init__(self, get_response):
            self.get_response = get_response
    
        def __call__(self, request):
            # make sure they are authenticated so we know we have their tz info.
            if request.user.is_authenticated():
                # we are getting the users timezone that in this case is stored in 
                # a user's profile
                tz_str = request.user.profile.timezone
                timezone.activate(pytz.timezone(tz_str))
            # otherwise deactivate and the default time zone will be used anyway
            else:
                timezone.deactivate()
    
            response = self.get_response(request)
            return response

There are a few new things going on. To learn more about middleware and what it does check out [that part of the documentation][2]. In `__call__` we are handling the setting of the timezone data. At first we make sure the user is authenticated, to make sure that we have timezone data for this user. Once we know we do, we active the timezone for the users session using `timezone.activate()`. In order to convert the time zone string we have to something usable by datetime, we use `pytz.timezone(str)`.

Now, when datetime objects are accessed in templates they will automatically be converted from the 'UTC' format of the database to whatever time zone the user is in. Just access the datetime object and its timezone will be set assuming the previous middleware is set up properly.

    {{ my_datetime_value }}

If you desire a fine grained control of whether the user's timezone is used take a look at the following:

    {% load tz %}
    {% localtime on %}
        {# this time will be respect the users time zone #}
        {{ your_date_time }}
    {% endlocaltime %}

    {% localtime off %}
        {# this will not respect the users time zone #}
        {{ your_date_time }}
    {% endlocaltime %}
    


 *Note, this method described only works in Django 1.10 and on. To support django from prior to 1.10 look into [MiddlewareMixin][3]*


  [1]: https://docs.djangoproject.com/en/1.11/ref/utils/#django.utils.timezone.is_aware
  [2]: https://www.wikiod.com/django/middleware
  [3]: https://docs.djangoproject.com/en/1.11/topics/http/middleware/#upgrading-pre-django-1-10-style-middleware
  [4]: https://docs.djangoproject.com/en/1.11/ref/utils/#django.utils.timezone.is_naive

