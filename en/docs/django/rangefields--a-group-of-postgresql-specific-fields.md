---
title: "RangeFields - a group of PostgreSQL specific fields"
slug: "rangefields---a-group-of-postgresql-specific-fields"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Syntax
- from django.contrib.postgres.fields import *RangeField
- IntegerRangeField(**options)
- BigIntegerRangeField(**options)
- FloatRangeField(**options)
- DateTimeRangeField(**options)
- DateRangeField(**options)

## Including numeric range fields in your model
There are three kinds of numeric `RangeField`s in Python. `IntegerField`, `BigIntegerField`, and `FloatField`. They convert to `psycopg2` [`NumericRange`][1]s, but accept input as native Python tuples. **The lower bound is included and the upper bound is excluded.**

    class Book(models.Model):
        name = CharField(max_length=200)
        ratings_range = IntegerRange()

  [1]: http://initd.org/psycopg/docs/extras.html#psycopg2.extras.NumericRange

## Setting up for RangeField
 1. add `'django.contrib.postgres'` to your `INSTALLED_APPS`
 2. install `psycopg2` 

## Creating models with numeric range fields
It's simpler and easier to input values as a Python tuple instead of a `NumericRange`.

    Book.objects.create(name='Pro Git', ratings_range=(5, 5))

Alternative method with `NumericRange`:

    Book.objects.create(name='Pro Git', ratings_range=NumericRange(5, 5))

## Using contains
This query selects all books with any rating less than three.

    bad_books = Books.objects.filter(ratings_range__contains=(1, 3))




## Using contained_by
This query gets all books with ratings greater than or equal to zero and less than six.

    all_books = Book.objects.filter(ratings_range_contained_by=(0, 6))



## Using overlap
This query gets all overlapping appointments from six to ten.

    Appointment.objects.filter(time_span__overlap=(6, 10))

<!--
Dear future editors:

Use DateTimeTZRange() for this one. 
          - CrazyPython
--!>

## Using None to signify no upper bound
This query selects all books with any rating greater than or equal to four.

    maybe_good_books = Books.objects.filter(ratings_range__contains=(4, None))

## Ranges operations
    from datetime import timedelta

    from django.utils import timezone
    from psycopg2.extras import DateTimeTZRange

    # To create a "period" object we will use psycopg2's DateTimeTZRange
    # which takes the two datetime bounds as arguments
    period_start = timezone.now()
    period_end = period_start + timedelta(days=1, hours=3)
    period = DateTimeTZRange(start, end)

    # Say Event.timeslot is a DateTimeRangeField

    # Events which cover at least the whole selected period,
    Event.objects.filter(timeslot__contains=period)

    # Events which start and end within selected period,
    Event.objects.filter(timeslot__contained_by=period)

    # Events which, at least partially, take place during the selected period.
    Event.objects.filter(timeslot__overlap=period)

