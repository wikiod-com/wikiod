---
title: "JSONField - a PostgreSQL specific field"
slug: "jsonfield---a-postgresql-specific-field"
draft: false
images: []
weight: 9928
type: docs
toc: true
---

## Syntax
- JSONField(**options)

- Django's `JSONField` actually stores the data in a Postgres `JSONB` column, which is only available in Postgres 9.4 and later.
- `JSONField` is great when you want a more flexible schema. For example if you want to change the keys without having to do any data migrations, or if not all your objects have the same structure.

- If you're storing data with static keys, consider using multiple normal fields instead of `JSONField`s instead, as querying `JSONField` can get quite tedious sometimes.

# Chaining queries
You can chain queries together. For example, if a dictionary exists inside a list, add two underscores and your dictionary query. 

> Don't forget to separate queries with double underscores.

<!--
 Hey future editors! Here's what still needs to be done:

 - contains
 - contained_by
 - has_key
 - has_any_keys
 - has_keys
        
        - CrazyPython ;)
--!>

## Creating a JSONField
## *Available in Django 1.9+*

    from django.contrib.postgres.fields import JSONField
    from django.db import models
    
    class IceCream(models.Model):
        metadata = JSONField()

You can add the normal `**options` if you wish.

> ***!*** Note that you must put `'django.contrib.postgres'` in `INSTALLED_APPS` in your `settings.py`

## Creating an object with data in a JSONField
Pass data in native Python form, for example `list`, `dict`, `str`, `None`, `bool`, etc.

    IceCream.objects.create(metadata={
        'date': '1/1/2016',
        'ordered by': 'Jon Skeet',
        'buyer': {
             'favorite flavor': 'vanilla',
             'known for': ['his rep on SO', 'writing a book']
         },
        'special requests': ['hot sauce'],
    })

> See the note in the "Remarks" section about using `JSONField` in practice.



## Querying data nested in dictionaries
Get all ice cream cones that were ordered by people liking chocolate:

    IceCream.objects.filter(metadata__buyer__favorite_flavor='chocolate')

> See the note in the "Remarks" section about chaining queries.

## Querying data present in arrays
An integer will be interpreted as an index lookup.

    IceCream.objects.filter(metadata__buyer__known_for__0='creating stack overflow')

> See the note in the "Remarks" section about chaining queries.

## Querying top-level data

    IceCream.objects.filter(metadata__ordered_by='Guido Van Rossum')

## Ordering by JSONField values
Ordering directly on `JSONField` is not yet supported in Django. But it's possible via RawSQL using PostgreSQL functions for jsonb:

    from django.db.models.expressions import RawSQL
    RatebookDataEntry.objects.all().order_by(RawSQL("data->>%s", ("json_objects_key",)))

This example orders by `data['json_objects_key']` inside `JSONField` named `data`:

    data = JSONField()



