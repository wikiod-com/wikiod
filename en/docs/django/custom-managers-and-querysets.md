---
title: "Custom Managers and Querysets"
slug: "custom-managers-and-querysets"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Defining a basic manager using Querysets and `as_manager` method
Django manger is an interface through which the django model queries the database. The `objects` field used in most django queries is actually the default manager created for us by django (this is only created if we don't define custom managers).

**Why would we define a custom manager/queryset?**

To avoid writing common queries all over our codebase and instead referring them using an easier to remember abstraction.
Example: Decide for yourself which version is more readable : 

+ Only get all the active users : `User.objects.filter(is_active=True)` vs `User.manager.active()`
+ Get all active dermatologists on our plaform : 
`User.objects.filter(is_active=True).filter(is_doctor=True).filter(specialization='Dermatology')` vs
`User.manager.doctors.with_specialization('Dermatology')`

Another benefit is that if tomorrow we decide all `psychologists` are also `dermatologists`, we can easily modify the query in our Manager and be done with it.

Below is an example of creating a custom `Manager` defined by creating a `QuerySet` and using the `as_manager` method.
    
    
    from django.db.models.query import QuerySet
    
    class ProfileQuerySet(QuerySet):
        def doctors(self):
            return self.filter(user_type="Doctor", user__is_active=True)
    
        def with_specializations(self, specialization):
            return self.filter(specializations=specialization)
    
        def users(self):
            return self.filter(user_type="Customer", user__is_active=True)
    
    ProfileManager = ProfileQuerySet.as_manager

We will add it to our model as below:

    class Profile(models.Model):
        ...
        manager = ProfileManager()

**NOTE** : Once we've defined a `manager` on our model, `objects` won't be defined for the model anymore. 

## select_related for all queries
**Model with ForeignKey**

We will work with these models :

    from django.db import models

    class Book(models.Model):
     name= models.CharField(max_length=50)
     author = models.ForeignKey(Author)
    
    class Author(models.Model):
     name = models.CharField(max_length=50)

Suppose we often (always) access `book.author.name`

**In view**

We could use the following, each time,

    books = Book.objects.select_related('author').all()

But this is not DRY.

**Custom Manager**

    class BookManager(models.Manager):

        def get_queryset(self):
            qs = super().get_queryset()
            return qs.select_related('author')

    class Book(models.Model):
        ...
        objects = BookManager()

**Note** : the call to `super` must be changed for python 2.x

Now all we have to use in views is

    books = Book.objects.all()

and no additional queries will be made in template/view.

## Define custom managers
Very often it happens to deal with models which have something like a `published` field. Such kind of fields are almost always used when retrieving objects, so that you will find yourself to write something like:

    my_news = News.objects.filter(published=True)

too many times. You can use custom managers to deal with these situations, so that you can then write something like:

    my_news = News.objects.published()

which is nicer and more easy to read by other developers too. 

Create a file `managers.py` in your app directory, and define a new `models.Manager` class:

    from django.db import models


    class NewsManager(models.Manager):

        def published(self, **kwargs):
            # the method accepts **kwargs, so that it is possible to filter
            # published news
            # i.e: News.objects.published(insertion_date__gte=datetime.now)
            return self.filter(published=True, **kwargs)

use this class by redefining the `objects` property in the model class:

    from django.db import models

    # import the created manager
    from .managers import NewsManager

    class News(models.Model):
        """ News model
        """
        insertion_date = models.DateTimeField('insertion date', auto_now_add=True)
        title = models.CharField('title', max_length=255)
        # some other fields here
        published = models.BooleanField('published')

        # assign the manager class to the objects property
        objects = NewsManager()
        
Now you can get your published news simply this way:

    my_news = News.objects.published()
and you can also perform more filtering:

    my_news = News.objects.published(title__icontains='meow')

