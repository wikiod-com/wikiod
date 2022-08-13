---
title: "ArrayField - a PostgreSQL-specific field"
slug: "arrayfield---a-postgresql-specific-field"
draft: false
images: []
weight: 9841
type: docs
toc: true
---

## Syntax
- from django.contrib.postgres.fields import ArrayField
- class ArrayField(base_field, size=None, **options)
- FooModel.objects.filter(array_field_name__contains=[objects, to, check])
- FooModel.objects.filter(array_field_name__contained_by=[objects, to, check])

Note that although the `size` parameter is passed to PostgreSQL, PostgreSQL will not enforce it.

When using `ArrayField`s one should keep in mind this word of warning from the [Postgresql arrays documentation][1].

> Tip: Arrays are not sets; searching for specific array elements can be
> a sign of database misdesign. Consider using a separate table with a
> row for each item that would be an array element. This will be easier
> to search, and is likely to scale better for a large number of
> elements.


  [1]: https://www.postgresql.org/docs/9.5/static/arrays.html

## Querying for membership of ArrayField with contains
This query returns all cones with a chocolate scoop and a vanilla scoop.

    VANILLA, CHOCOLATE, MINT, STRAWBERRY = 1, 2, 3, 4  # constants for flavors
    choco_vanilla_cones = IceCream.objects.filter(scoops__contains=[CHOCOLATE, VANILLA])


Don't forget to import the `IceCream` model from your `models.py` file.


Also bear in mind that django will not create an index for `ArrayField`s. If you are going to search them, you are going to need an index and it will need to be manually created with a call to RunSQL in your migrations file.

  [1]: https://www.postgresql.org/docs/9.1/static/arrays.html

## A basic ArrayField
To create a PostgreSQL ArrayField, we should give ArrayField the type of data we want it to store as a field as its first argument. Since we'll be storing book ratings, we will use `FloatField`.

     from django.db import models, FloatField
     from django.contrib.postgres.fields import ArrayField
     
     class Book(models.Model):
         ratings = ArrayField(FloatField())

## Specifying the maximum size of an ArrayField


     from django.db import models, IntegerField
     from django.contrib.postgres.fields import ArrayField
     
     class IceCream(models.Model):
         scoops = ArrayField(IntegerField()  # we'll use numbers to ID the scoops
                       , size=6)  # our parlor only lets you have 6 scoops


When you use the size parameter, it's passed through to postgresql, which accepts it and then ignores it! Thus it's quite possible to add 7 integers to the `scoops` field above using the postgresql console.

## Nesting ArrayFields
You can nest `ArrayField`s by passing another `ArrayField` as it's `base_field`.

    from django.db import models, IntegerField
    from django.contrib.postgres.fields import ArrayField

    class SudokuBoard(models.Model):
        numbers = ArrayField(
            ArrayField(
                models.IntegerField(),
                size=9,
            ),
            size=9,
        )

## Querying for all models who contain any item in a list with contained_by
This query returns all cones with either a mint scoop or a vanilla scoop.

    minty_vanilla_cones = IceCream.objects.filter(scoops__contained_by=[MINT, VANILLA])

