---
title: "F() expressions"
slug: "f-expressions"
draft: false
images: []
weight: 9906
type: docs
toc: true
---

An F() expression is a way for Django to use a Python object to refer to the value of model field or annotated column in the database without having to pull the value into Python memory. This allows developers to avoid certain race conditions and also filtering results based on model field values.

## Syntax
- from django.db.models import F

## Avoiding race conditions
*See [this Q&A question][1] if you don't know what race conditions are.*

[1]: http://stackoverflow.com/q/34510/1529346

The following code may be subject to race conditions :

<!-- language: python -->
    article = Article.objects.get(pk=69)
    article.views_count += 1
    article.save()

If `views_count` is equal to `1337`, this will result in such query:

<!-- language: sql -->
    UPDATE app_article SET views_count = 1338 WHERE id=69

If two clients access this article at the same time, what *may* happen is that the second HTTP request executes `Article.objects.get(pk=69)` before the first executes `article.save()`. Thus, both requests will have `views_count = 1337`, increment it, and save `views_count = 1338` to the database, while it should actually be `1339`.

To fix this, use an `F()` expression:

<!-- language: python -->
    article = Article.objects.get(pk=69)
    article.views_count = F('views_count') + 1
    article.save()

This, on the other hand, will result in such query:

<!-- language: sql -->
    UPDATE app_article SET views_count = views_count + 1 WHERE id=69

## Updating queryset in bulk
Let's assume that we want to remove 2 upvotes from all the articles of the author with id `51`. <br>
Doing this only with Python would execute `N` queries (`N` being the number of articles in the queryset):

    for article in Article.objects.filter(author_id=51):
        article.upvotes -= 2
        article.save()
        # Note that there is a race condition here but this is not the focus
        # of this example.

What if instead of pulling all the articles into Python, looping over them, decreasing the upvotes, and saving each updated one back to the database, there was another way?<br>
Using an `F()` expression, can do it in one query:

    Article.objects.filter(author_id=51).update(upvotes=F('upvotes') - 2)

Which can be translated in the following SQL query:

<!-- language: sql -->
    UPDATE app_article SET upvotes = upvotes - 2 WHERE author_id = 51

Why is this better?

 - Instead of Python doing the work, we pass the load into the database which is fine tuned to make such queries.
 - Effectively cuts down on the number of database queries needed to achieve the wanted result.



## Execute Arithmetic operations between fields
`F()` expressions can be used to execute arithmetic operations (`+`, `-`, `*` etc.) among model fields, in order to define an algebraic lookup/connection between them.

 - Let model be:
   
       class MyModel(models.Model):
           int_1 = models.IntegerField()
           int_2 = models.IntegerField()
   
 - Now lets assume that we want to retrieve all the objects of `MyModel` table who's `int_1` and `int_2` fields satisfy this equation: `int_1 + int_2 >= 5`. Utilizing [`annotate()`][1] and `filter()` we get:

       result = MyModel.objects.annotate(
                    diff=F(int_1) + F(int_2)
                ).filter(diff__gte=5)
    `result` now contains all of the aforementioned objects.

Although the example utilizes `Integer` fields, this method will work on every field on which an arithmetic operation can be applied.


  [1]: https://www.wikiod.com/django/model-aggregations

