---
title: "Unit Testing"
slug: "unit-testing"
draft: false
images: []
weight: 9908
type: docs
toc: true
---

## The Database and Testing
Django uses special database settings when testing so that tests can use the database normally but by default run on an empty database.  Database changes in one test will not be seen by another.  For example, both of the following tests will pass:

    from django.test import TestCase
    from myapp.models import Thing
    
    class MyTest(TestCase):

        def test_1(self):
            self.assertEqual(Thing.objects.count(), 0)
            Thing.objects.create()
            self.assertEqual(Thing.objects.count(), 1)

        def test_2(self):
            self.assertEqual(Thing.objects.count(), 0)
            Thing.objects.create(attr1="value")
            self.assertEqual(Thing.objects.count(), 1)

**Fixtures**

If you want to have database objects used by multiple tests, either create them in the `setUp` method of the test case.  Additionally, if you have defined fixtures in your django project, they can be included like so:

    class MyTest(TestCase):
        fixtures = ["fixture1.json", "fixture2.json"]

By default, django is looking for fixtures in the `fixtures` directory in each app. Further directories can be set using the `FIXTURE_DIRS` setting:

    # myapp/settings.py
    FIXTURE_DIRS = [
        os.path.join(BASE_DIR, 'path', 'to', 'directory'),
    ]

Let's assume you have created a model as follows:

    # models.py
    from django.db import models
    

    class Person(models.Model):
        """A person defined by his/her first- and lastname."""
        firstname = models.CharField(max_length=255)
        lastname = models.CharField(max_length=255)

Then your .json fixtures could look like that:
    
    # fixture1.json
    [
        { "model": "myapp.person",
            "pk": 1,
            "fields": {
                "firstname": "Peter",
                "lastname": "Griffin"
            }
        },
        { "model": "myapp.person",
            "pk": 2,
            "fields": {
                "firstname": "Louis",
                "lastname": "Griffin"
            }
        },
    ]

**Reuse the test-database**

To speed up your test-runs you can tell the management-command to reuse the test-database (and to prevent it from being created before and deleted after every test-run).
This can be done using the keepdb (or shorthand `-k`) flag like so:

    # Reuse the test-database (since django version 1.8)
    $ python manage.py test --keepdb

## Testing - a complete example
This assumes that you have read the documentation about starting a new Django project. Let us assume that the main app in your project is named td (short for test driven). To create your first test, create a file named test_view.py and copy paste the following content into it.

    from django.test import Client, TestCase
    
    class ViewTest(TestCase):
    
        def test_hello(self):
            c = Client()
            resp = c.get('/hello/')
            self.assertEqual(resp.status_code, 200)

You can run this test by

     ./manage.py test

and it will most naturally fail! You will see an error similar to the following.

    Traceback (most recent call last):
      File "/home/me/workspace/td/tests_view.py", line 9, in test_hello
        self.assertEqual(resp.status_code, 200)
    AssertionError: 200 != 404

Why does that happen? Because we haven't defined a view for that! So let's do it. Create a file called views.py and place in it the following code


    from django.http import HttpResponse
    def hello(request):
        return HttpResponse('hello')

Next map it to the /hello/ by editing urls py as follows:

    from td import views
    
    urlpatterns = [
        url(r'^admin/', include(admin.site.urls)),
        url(r'^hello/', views.hello),
        ....
    ]


Now run the test again `./manage.py test` again and viola!!

    Creating test database for alias 'default'...
    .
    ----------------------------------------------------------------------
    Ran 1 test in 0.004s
    
    OK




## Limit the number of tests executed
It is possible to limit the tests executed by `manage.py test` by specifying which modules should be discovered by the test runner:

    # Run only tests for the app names "app1"
    $ python manage.py test app1

    # If you split the tests file into a module with several tests files for an app
    $ python manage.py test app1.tests.test_models
    
    # it's possible to dig down to individual test methods.
    $ python manage.py test app1.tests.test_models.MyTestCase.test_something


If you want to run a bunch of tests you can pass a pattern of filenames. For example, you may want to run only tests that involving of your models:

    $ python manage.py test -p test_models*
    Creating test database for alias 'default'...
    .................................................
    ----------------------------------------------------------------------
    Ran 115 tests in 3.869s
    
    OK


Finally, it is possible to stop the test suite at the first fail, using `--failfast`. This argument allows to get quickly the potential error encountered in the suite:

    $ python manage.py test app1
    ...F..
    ----------------------------------------------------------------------
    Ran 6 tests in 0.977s
    
    FAILED (failures=1)


    $ python manage.py test app1 --failfast
    ...F
    ======================================================================
    [Traceback of the failing test]
    ----------------------------------------------------------------------
    Ran 4 tests in 0.372s
    
    FAILED (failures=1)


## Testing Access Control in Django Views
**tl;dr** : Create a base class that defines two user objects (say `user` and `another_user`). Create your other models and define three `Client` instances.
+ `self.client` : Representing `user` logged in browser
+ `self.another_client` : Representing `another_user` 's client
+ `self.unlogged_client` : Representing unlogged person

Now access all your public and private urls from these three client objects and dictact the response you expect. Below I've showcased the strategy for a `Book` object that can either be `private` (owned by a few privileged users) or `public` (visible to everyone).

    from django.test import TestCase, RequestFactory, Client
    from django.core.urlresolvers import reverse
    
    class BaseViewTestCase(TestCase):

        @classmethod
        def setUpClass(cls):
            super(BaseViewTestCase, cls).setUpClass()
            cls.client = Client()
            cls.another_client = Client()
            cls.unlogged_client = Client()
            cls.user = User.objects.create_user(
                    'dummy',password='dummy'
                    )
            cls.user.save()
            cls.another_user = User.objects.create_user(
                    'dummy2', password='dummy2'
                    )
            cls.another_user.save()
            cls.first_book = Book.objects.create(
                    name='first',
                    private = true
            )
            cls.first_book.readers.add(cls.user)
            cls.first_book.save()
            cls.public_book = Template.objects.create(
                    name='public',
                    private=False
            )
            cls.public_book.save()

   
        def setUp(self):
            self.client.login(username=self.user.username, password=self.user.username)
            self.another_client.login(username=self.another_user.username, password=self.another_user.username)


    """
       Only cls.user owns the first_book and thus only he should be able to see it.
       Others get 403(Forbidden) error
    """
    class PrivateBookAccessTestCase(BaseViewTestCase):
        
        def setUp(self):
            super(PrivateBookAccessTestCase, self).setUp()
            self.url = reverse('view_book',kwargs={'book_id':str(self.first_book.id)})

        def test_user_sees_own_book(self):
            response = self.client.get(self.url)
            self.assertEqual(200, response.status_code)
            self.assertEqual(self.first_book.name,response.context['book'].name)
            self.assertTemplateUsed('myapp/book/view_template.html')

        def test_user_cant_see_others_books(self):
            response = self.another_client.get(self.url)
            self.assertEqual(403, response.status_code)
            
        def test_unlogged_user_cant_see_private_books(self):
            response = self.unlogged_client.get(self.url)
            self.assertEqual(403, response.status_code)

    """
        Since book is public all three clients should be able to see the book
    """
     class PublicBookAccessTestCase(BaseViewTestCase):
        
        def setUp(self):
            super(PublicBookAccessTestCase, self).setUp()
            self.url = reverse('view_book',kwargs={'book_id':str(self.public_book.id)})

        def test_user_sees_book(self):
            response = self.client.get(self.url)
            self.assertEqual(200, response.status_code)
            self.assertEqual(self.public_book.name,response.context['book'].name)
            self.assertTemplateUsed('myapp/book/view_template.html')

        def test_another_user_sees_public_books(self):
            response = self.another_client.get(self.url)
            self.assertEqual(200, response.status_code)
            
        def test_unlogged_user_sees_public_books(self):
            response = self.unlogged_client.get(self.url)
            self.assertEqual(200, response.status_code)


## Testing Django Models Effectively
Assuming a class

    from django.db import models

    class Author(models.Model):
       name = models.CharField(max_length=50)
       
        def __str__(self):
            return self.name
        
        def get_absolute_url(self):
            return reverse('view_author', args=[str(self.id)])
    

    class Book(models.Model):
        author = models.ForeignKey(Manufacturer, on_delete=models.CASCADE)
        private = models.BooleanField(default=false)
        publish_date = models.DateField()

        def get_absolute_url(self):
            return reverse('view_book', args=[str(self.id)])
        
        def __str__(self):
            return self.name


Testing examples

    from django.test import TestCase
    from .models import Book, Author
    
    class BaseModelTestCase(TestCase):
    
        @classmethod
        def setUpClass(cls):
            super(BaseModelTestCase, cls).setUpClass()
            cls.author = Author(name='hawking')
            cls.author.save()
            cls.first_book = Book(author=cls.author, name="short_history_of_time")
            cls.first_book.save()
            cls.second_book = Book(author=cls.author, name="long_history_of_time")
            cls.second_book.save()


    class AuthorModelTestCase(BaseModelTestCase):
        def test_created_properly(self):
             self.assertEqual(self.author.name, 'hawking')
             self.assertEqual(True, self.first_book in self.author.book_set.all())
        
        def test_absolute_url(self):
            self.assertEqual(self.author.get_absolute_url(), reverse('view_author', args=[str(self.author.id)]))

    class BookModelTestCase(BaseModelTestCase):
        
        def test_created_properly(self:
            ...
            self.assertEqual(1, len(Book.objects.filter(name__startswith='long'))
        
        def test_absolute_url(self):
            ...
        

Some points
    
+ `created_properly` tests are used to verify the state properties of django models. They help catch sitautions where we've changed default values, file_upload_paths etc.
+ `absolute_url` might seem trivial but I've found that it's helped me prevent some bugs when changing url paths
+ I similarly write test cases for all the methods implemented inside a model (using `mock` objects etc)
+ By defining a common `BaseModelTestCase` we can setup the necessary relationships between models to ensure proper testing.

Finally, when in doubt, write a test. Trivial behavior changes are caught by paying attention to detail and long forgotten pieces of code don't end up causing unnecessary trouble.


        
    

        

