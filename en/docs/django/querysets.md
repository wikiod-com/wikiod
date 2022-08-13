---
title: "Querysets"
slug: "querysets"
draft: false
images: []
weight: 9851
type: docs
toc: true
---

A `Queryset` is fundamentally a list of objects derived from a `Model`, by a compilation of database queries.

## Reduce number of queries on ForeignKey field (n+1 issue)
# Problem
Django querysets are evaluated in a lazy fashion. For example:

<!-- language: lang-python -->

    # models.py:
    class Author(models.Model):
        name = models.CharField(max_length=100)

    class Book(models.Model):
        author = models.ForeignKey(Author, related_name='books')
        title = models.CharField(max_length=100)

   <!-- language: lang-python -->

    # views.py
    def myview(request):
        # Query the database
        books = Book.objects.all()

        for book in books:
            # Query the database on each iteration to get author (len(books) times)
            # if there is 100 books, there will have 100 queries plus the initial query
            book.author
            # ...

        # total : 101 queries

The code above causes django to query the database for the author of each book. This is inefficient, and it is better to only have a single query.

# Solution 

Use `select_related` on `ForeignKey` if you know that you will need to later access a `ForeignKey` field.

   <!-- language: lang-python -->

    # views.py
    def myview(request):
        # Query the database.
        books = Books.objects.select_related('author').all()
        
        for book in books:
            # Does not query the database again, since `author` is pre-populated
            book.author
            # ...

        # total : 1 query

`select_related` can also be used on lookup fields:
   
   <!-- language: lang-python -->
    # models.py:
    class AuthorProfile(models.Model):
        city = models.CharField(max_length=100)

    class Author(models.Model):
        name = models.CharField(max_length=100)
        profile = models.OneToOneField(AuthorProfile)

    class Book(models.Model):
        author = models.ForeignKey(Author, related_name='books')
        title = models.CharField(max_length=100)    

<!-- language: lang-python -->
    # views.py
    def myview(request):
        books = Book.objects.select_related('author')\
                            .select_related('author__profile').all()
       
        for book in books:
            # Does not query database
            book.author.name
            # or
            book.author.profile.city
            # ...

        # total : 1 query

## Get SQL for Django queryset
The `query` attribute on queryset gives you SQL equivalent syntax for your query.

    >>> queryset = MyModel.objects.all()
    >>> print(queryset.query)
    SELECT "myapp_mymodel"."id", ... FROM "myapp_mymodel"

> **Warning:** 
> 
> This output should only be used for debugging purposes. The generated
> query is not backend-specific. As such, the parameters aren't quoted
> properly, leaving it vulnerable to SQL injection, and the query may
> not even be executable on your database backend.

## Advanced queries with Q objects
Given the model:

    class MyModel(models.Model):
        name = models.CharField(max_length=10)
        model_num = models.IntegerField()
        flag = models.NullBooleanField(default=False)


We can use `Q` objects to create  `AND` , `OR` conditions in your lookup query. For example, say we want all objects that have `flag=True` **OR** `model_num>15`.

    from django.db.models import Q
    MyModel.objects.filter(Q(flag=True) | Q(model_num__gt=15))

The above translates to `WHERE flag=True OR model_num > 15` similarly for an **AND** you would do.

    MyModel.objects.filter(Q(flag=True) & Q(model_num__gt=15))

`Q` objects also allow us to make **NOT** queries with the use of `~`. Let's say we wanted to get all objects that have `flag=False` **AND** `model_num!=15`, we would do:

    MyModel.objects.filter(Q(flag=True) & ~Q(model_num=15)) 

If using Q objects and "normal" parameters in `filter()`, then the `Q` objects must come *first*. The following query searches for models with (`flag` set to `True` or a model number greater than `15`) and a name that starts with "H".

    from django.db.models import Q
    MyModel.objects.filter(Q(flag=True) | Q(model_num__gt=15), name__startswith="H")

**Note:** `Q` objects can be used with any lookup function that takes keyword arguments such as `filter`, `exclude`, `get`. Make sure that when you use with `get` that you will only return one object or the `MultipleObjectsReturned` exception will be raised.



## Reduce number of queries on ManyToManyField (n+1 issue)
# Problem

<!-- language: lang-python -->

    # models.py:
    class Library(models.Model):
        name = models.CharField(max_length=100)
        books = models.ManyToManyField(Book)

    class Book(models.Model):
        title = models.CharField(max_length=100)

<!-- language: lang-python -->

    # views.py
    def myview(request):
        # Query the database.
        libraries = Library.objects.all()

        # Query the database on each iteration (len(author) times)
        # if there is 100 librairies, there will have 100 queries plus the initial query
        for library in libraries:
            books = library.books.all()
            books[0].title
            # ...

        # total : 101 queries

# Solution 

Use `prefetch_related` on `ManyToManyField` if you know that you will need to access later a field which is a `ManyToManyField` field.

<!-- language: lang-python -->

    # views.py
    def myview(request):
        # Query the database.
        libraries = Library.objects.prefetch_related('books').all()
        
        # Does not query the database again, since `books` is pre-populated
        for library in libraries:
            books = library.books.all()
            books[0].title
            # ...

        # total : 2 queries - 1 for libraries, 1 for books

`prefetch_related` can also be used on lookup fields :
   
<!-- language: lang-python -->

    # models.py:
    class User(models.Model):
        name = models.CharField(max_length=100)

    class Library(models.Model):
        name = models.CharField(max_length=100)
        books = models.ManyToManyField(Book)

    class Book(models.Model):
        title = models.CharField(max_length=100)
        readers = models.ManyToManyField(User)
 
<!-- language: lang-python -->

     # views.py
    def myview(request):
        # Query the database.
        libraries = Library.objects.prefetch_related('books', 'books__readers').all()
        
        # Does not query the database again, since `books` and `readers` is pre-populated
        for library in libraries:
            for book in library.books.all():
                for user in book.readers.all():
                    user.name
                    # ...
    
        # total : 3 queries - 1 for libraries, 1 for books, 1 for readers

However, once the queryset has been executed, the data fetched can't be altered without hitting again the database. The following would execute extra queries for example:

<!-- language: lang-python -->

     # views.py
    def myview(request):
        # Query the database.
        libraries = Library.objects.prefetch_related('books').all()
        for library in libraries:
            for book in library.books.filter(title__contains="Django"):
                print(book.name)

The following can be optimized using a `Prefetch` object, introduced in Django 1.7:

<!-- language: lang-python -->
    from django.db.models import Prefetch
    # views.py
    def myview(request):
        # Query the database.
        libraries = Library.objects.prefetch_related(
            Prefetch('books', queryset=Book.objects.filter(title__contains="Django")
        ).all()
        for library in libraries:
            for book in library.books.all():
                print(book.name)  # Will print only books containing Django for each library


## Simple queries on a standalone model
Here is a simple model that we will use to run a few test queries:

    class MyModel(models.Model):
        name = models.CharField(max_length=10)
        model_num = models.IntegerField()
        flag = models.NullBooleanField(default=False)

Get a single model object where the id/pk is 4: \
(If there are no items with the id of 4 or there are more than one, this will throw an exception.)

    MyModel.objects.get(pk=4)

All model objects:

    MyModel.objects.all()

Model objects that have `flag` set to `True`:

    MyModel.objects.filter(flag=True)

Model objects with a `model_num` greater than 25:

    MyModel.objects.filter(model_num__gt=25)

Model objects with the `name` of "Cheap Item" and `flag` set to `False`:

    MyModel.objects.filter(name="Cheap Item", flag=False)

Models simple search `name` for specific string(Case-sensitive):

    MyModel.objects.filter(name__contains="ch")

Models simple search `name` for specific string(Case-insensitive):

    MyModel.objects.filter(name__icontains="ch")


## Get first and last record from QuerySet
To get First object:

    MyModel.objects.first()
To get last objects:

    MyModel.objects.last()
Using Filter First object:

    MyModel.objects.filter(name='simple').first()

Using Filter Last object:

    MyModel.objects.filter(name='simple').last()

## Advanced queries with F objects
>An F() object represents the value of a model field or annotated column. It makes it possible to refer to model field values and perform database operations using them without actually having to pull them out of the database into Python memory. - [F() expressions](https://docs.djangoproject.com/en/1.10/ref/models/expressions/#f-expressions)

It is appropriate to use `F()` objects whenever you need to reference another field's value in your query. By itself, `F()` objects do not mean anything, and they cannot and should not be called outside of a queryset. They are used to reference a field's value on the same queryset. 

For example, given a model ...

    SomeModel(models.Model):
        ...
        some_field = models.IntegerField()

... a user can query objects where the `some_field` value is twice its `id` by [referencing the `id` field's value](https://docs.djangoproject.com/en/1.10/topics/db/queries/#filters-can-reference-fields-on-the-model) while filtering using `F()` like this:

    SomeModel.objects.filter(some_field=F('id') * 2)

`F('id')` simply references the `id` value for that same instance. Django uses it to create corresponding SQL statement. In this case something closely resembling this:

    SELECT * FROM some_app_some_model 
    WHERE some_field = ((id * 2))


Without `F()` expressions this would be accomplished with either raw SQL or  filtering in Python (which reduces the performance especially when there are lots of objects). 

<hr>

References:

- [Filters can reference fields on model](https://docs.djangoproject.com/en/1.10/topics/db/queries/#filters-can-reference-fields-on-the-model)
- [F expressions](https://docs.djangoproject.com/en/1.10/ref/models/expressions/#django.db.models.F)
- [Answer](http://stackoverflow.com/a/39403426/1036843) from [TinyInstance](http://stackoverflow.com/users/3659874/tiny-instance)

From `F()` class definition:
> An object capable of resolving references to existing query objects. - [F source](https://docs.djangoproject.com/en/1.10/_modules/django/db/models/expressions/#F)

*Note: This example posted came from the answer listed above with consent from TinyInstance.*

