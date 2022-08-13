---
title: "Mapping strings to strings with HStoreField - a PostgreSQL specific field"
slug: "mapping-strings-to-strings-with-hstorefield---a-postgresql-specific-field"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Syntax
- FooModel.objects.filter(field_name__key_name='value to query')

## Setting up HStoreField
First, we'll need to do some setup to get `HStoreField` working.

1. make sure `django.contrib.postgres` is in your `INSTALLED_APPS
2. Add `HStoreExtension` to your migrations. Remember to put `HStoreExtension` before any `CreateModel` or `AddField` migrations.


    from django.contrib.postgres.operations import HStoreExtension
    from django.db import migrations
    
    class FooMigration(migrations.Migration):
        # put your other migration stuff here
        operations = [
            HStoreExtension(),
            ...
        ]

## Adding HStoreField to your model
> `-> ` Note: make sure you set up `HStoreField` first before going on with this example. (above)

No parameters are required for initializing a `HStoreField`.

    from django.contrib.postgres.fields import HStoreField
    from django.db import models
        
    class Catalog(models.model):
        name = models.CharField(max_length=200)
        titles_to_authors = HStoreField()

## Creating a new model instance
Pass a native python dictionary mapping strings to strings to `create()`.

    Catalog.objects.create(name='Library of Congress', titles_to_authors={
        'Using HStoreField with Django': 'CrazyPython and la communidad',
        'Flabbergeists and thingamajigs': 'La Artista Fooista',
        'Pro Git': 'Scott Chacon and Ben Straub',
    })

<!-- 
la communidad = the community
--!>



## Performing key lookups
    Catalog.objects.filter(titles__Pro_Git='Scott Chacon and Ben Straub')



## Using contains
Pass a `dict` object to `field_name__contains` as a keyword argument.

    Catalog.objects.filter(titles__contains={
            'Pro Git': 'Scott Chacon and Ben Straub'})
<!--
I have no idea how to ident this example. Any help would be appreciated. :)
           - CrazyPython
--!>

Equivalent to the SQL operator `@>`.

<!--
A bug in the renderer!
          - CrazyPython
--!>

