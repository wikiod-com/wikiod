---
title: "Models"
slug: "models"
draft: false
images: []
weight: 9658
type: docs
toc: true
---

In the basic case, a model is Python class that maps to a single database table. The attributes of the class map to columns in the table and an instance of the class represents a row in database table. The models inherit from `django.db.models.Model` which provides a rich API for adding and filtering results from the database.

[Create Your First Model](https://www.wikiod.com/django/models#Creating your first model) 

## Adding a string representation of a model
To create a human-readable presentation of a model object you need to implement `Model.__str__()` method (or `Model.__unicode__()` on python2). This method will be called whenever you call `str()` on a instance of your model (including, for instance, when the model is used in a template). Here's an example:

1. Create a book model.

       # your_app/models.py

       from django.db import models
    
       class Book(models.Model):
           name = models.CharField(max_length=50)
           author = models.CharField(max_length=50)

2. Create an instance of the model, and save it in the database:

       >>> himu_book = Book(name='Himu Mama', author='Humayun Ahmed')
       >>> himu_book.save()

3. Execute `print()` on the instance:

       >>> print(himu_book)
       <Book: Book object>

**<Book: Book object>**, the default output, is of no help to us. To fix this, let's add a `__str__` method.

    from django.utils.encoding import python_2_unicode_compatible

    @python_2_unicode_compatible
    class Book(models.Model):
        name = models.CharField(max_length=50)
        author = models.CharField(max_length=50)

        def __str__(self):
            return '{} by {}'.format(self.name, self.author)

Note the `python_2_unicode_compatible` decorator is needed only if you want your code to be compatible with python 2. This decorator copies the `__str__` method to create a `__unicode__` method. Import it from `django.utils.encoding`.

Now if we call the print function the book instance again:

    >>> print(himu_book)
    Himu Mama by Humayun Ahmed

Much better!

The string representation is also used when the model is used in a `ModelForm` for `ForeignKeyField` and `ManyToManyField` fields.

## Advanced models
A model can provide a lot more information than just the data about an object. Let's see an example and break it down into what it is useful for: 

    from django.db import models
    from django.urls import reverse
    from django.utils.encoding import python_2_unicode_compatible

    @python_2_unicode_compatible
    class Book(models.Model):
        slug = models.SlugField()
        title = models.CharField(max_length=128)
        publish_date = models.DateField()

        def get_absolute_url(self):
            return reverse('library:book', kwargs={'pk':self.pk})

        def __str__(self):
            return self.title

        class Meta:
            ordering = ['publish_date', 'title']

## Automatic primary key

You might notice the use of `self.pk` in the `get_absolute_url` method. The `pk`field is an alias to the primary key of a model. Also, django will automatically add a primary key if it's missing. That's one less thing to worry and let you set foreign key to any models and get them easily.

## Absolute url

The first function that is defined is `get_absolute_url`. This way, if you have an book, you can get a link to it without fiddling with the url tag, resolve, attribute and the like. Simply call `book.get_absolute_url` and you get the right link. As a bonus, your object in the django admin will gain a button "view on site".

## String representation

Have a `__str__` method let you use the object when you need to display it. For example, with the previous method, adding a link to the book in a template is as simple as `<a href="{{ book.get_absolute_url }}">{{ book }}</a>`. Straight to the point. This method also control what is displayed in the admin drop-down, for foreign key for example.

The class decorator let you define the method once for both `__str__` and `__unicode__` on python 2 while causing no issue on python 3. If you expect your app to run on both version, that's the way to go.

## Slug field
The slug field is similar to a char field but accept less symbols. By default, only letters, numbers, underscores or hyphens. It is useful if you want to identify an object using a nice representation, in url for example.

##  The Meta class
The `Meta` class let us define a lot more of information on the whole collection of item. Here, only the default ordering is set. It is useful with the ListView object for example. It take an ideally short list of field to use for sorting. Here, book will be sorted first by publication date then by title if the date is the same.

Other frequents attributes are `verbose_name` and `verbose_name_plural`. By default, they are generated from the name of the model and should be fine. But the plural form is naive, simply appending an 's' to the singular so you might want to set it explicitly in some case.





## Applying the changes to the database (Migrations)
After creating a new model or modifying existing models, you will need to generate migrations for your changes and then apply the migrations to the specified database. This can be done by using the Django's built-in migrations system. Using the `manage.py` utility when in the project root directory:

    python manage.py makemigrations <appname>

The above command will create the migration scripts that are necessary under `migrations` subdirectory of your application. If you omit the `<appname>` parameter, all the applications defined in the `INSTALLED_APPS` argument of `settings.py` will be processed. If you find it necessary, you can edit the migrations.

You can check what migrations are required without actually creating the migration use the --dry-run option, eg:

    python manage.py makemigrations --dry-run



To apply the migrations:

    python manage.py migrate <appname>

The above command will execute the migration scripts generated in the first step and physically update the database.

If the model of existing database is changed then following command is needed for making necessary changes.

    python manage.py migrate --run-syncdb

Django will create the table with name `<appname>_<classname>` by default. Sometime you don't want to use it. If you want to change the default name, you can announce the table name by setting the `db_table` in the class `Meta`:

    from django.db import models

    class YourModel(models.Model):
        parms = models.CharField()
        class Meta:
            db_table = "custom_table_name"

If you want to see what SQL code will be executed by a certain migration just run this command:

    python manage.py sqlmigrate <app_label> <migration_number>

*Django >1.10*  
The new `makemigrations --check` option makes the command exit with a non-zero status when model changes without migrations are detected.


See [Migrations][1] for more details on migrations.

[1]: https://www.wikiod.com/django/migrations

## Creating your first model
Models are typically defined in the `models.py` file under your application subdirectory. The `Model` class of `django.db.models` module is a good starting class to extend your models from. For example:

    from django.db import models

    class Book(models.Model):
        title = models.CharField(max_length=100)
        author = models.ForeignKey('Author', on_delete=models.CASCADE, related_name='authored_books')
        publish_date = models.DateField(null=True, blank=True)

        def __str__(self): # __unicode__ in python 2.*
            return self.title

Each attribute in a model represents a column in the database.

 - `title` is a text with a maximum length of 100 characters
 - `author` is a `ForeignKey` which represents a relationship to another model/table, in this case `Author` (used only for example purposes). `on_delete` tells the database what to do with the object should the related object (an `Author`) be deleted. (It should be noted that since django 1.9 `on_delete` can be used as the second positional argument. In [django 2 it is a **required** argument][1] and it is advisable to treat it as such immediately. In older versions it will default to `CASCADE`.)
 - `publish_date` stores a date. Both `null` and `blank` are set to `True` to indicate that it is not a required field (i.e. you may add it at a later date or leave it empty.)

Along with the attributes we define a method `__str__` this returns the title of the book which will be used as its `string` representation where necessary, rather than the default.


  [1]: https://docs.djangoproject.com/en/1.10/ref/models/fields/#django.db.models.ForeignKey.on_delete "django documentation 'on_delete'"

## UUID Primary key
A model by default will use an auto incrementing (integer) primary key. This will give you a sequence of keys 1, 2, 3.

Different primary key types can be set on a model with a small alterations to the model.

A [UUID][1] is a universally unique identifier, this is 32 character random identifier which can be used as an ID. This is a good option to use when you do not want sequential ID's assigned to records in your database.When used on PostgreSQL, this stores in a uuid datatype, otherwise in a char(32).

    import uuid
    from django.db import models

    class ModelUsingUUID(models.Model):
        id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)


The generated key will be in the format `7778c552-73fc-4bc4-8bf9-5a2f6f7b7f47`

  [1]: https://docs.djangoproject.com/en/1.9/ref/models/fields/#uuidfield

## Creating a model with relationships
**Many-to-One Relationship**

    from django.db import models
    
    class Author(models.Model):
       name = models.CharField(max_length=50)

    #Book has a foreignkey (many to one) relationship with author
    class Book(models.Model):
        author = models.ForeignKey(Author, on_delete=models.CASCADE)
        publish_date = models.DateField()

Most generic option. Can be used anywhere you would like to represent a relationship

**Many-to-Many Relationship**

    class Topping(models.Model):
        name = models.CharField(max_length=50)

    # One pizza can have many toppings and same topping can be on many pizzas
    class Pizza(models.Model):
        name = models.CharField(max_length=50)
        toppings = models.ManyToManyField(Topping)

Internally this is represented via another table. And `ManyToManyField` should be put on models that will be edited on a form. Eg: `Appointment` will have a `ManyToManyField` called `Customer`, `Pizza` has `Toppings` and so on.

**Many-to-Many Relationship using Through classes**

    class Service(models.Model):
         name = models.CharField(max_length=35)
    
    class Client(models.Model):
        name = models.CharField(max_length=35)
        age = models.IntegerField()
        services = models.ManyToManyField(Service, through='Subscription')

    class Subscription(models.Model):
         client = models.ForeignKey(Client)
         service = models.ForeignKey(Service)
         subscription_type = models.CharField(max_length=1, choices=SUBSCRIPTION_TYPES)
         created_at = models.DateTimeField(default=timezone.now)

This way, we can actually keep more metadata about a relationship between two entities. As can be seen, a client can be subscribed to several services via several subscription types. The only difference in this case is that to add new instances to the M2M relation, one cannot use the shortcut method `pizza.toppings.add(topping)`, instead, a new object of the *through* class should be created, `Subscription.objects.create(client=client, service=service, subscription_type='p')`

> In other languages `through tables` are also known as a `JoinColumn` ,
> `Intersection table` or `mapping table`

**One-to-One Relationship** 

    class Employee(models.Model):
       name = models.CharField(max_length=50)
       age = models.IntegerField()
       spouse = models.OneToOneField(Spouse)

    class Spouse(models.Model):
       name = models.CharField(max_length=50)

Use these fields when you will only ever have a composition relationship between the two models.

   


## Basic Django DB queries
Django ORM is a powerful abstraction that lets you store and retrieve data from the database without writing sql queries yourself.

Let's assume the following models:

    class Author(models.Model):
       name = models.CharField(max_length=50)

    class Book(models.Model): 
       name = models.CharField(max_length=50)
       author = models.ForeignKey(Author)

Assuming you've added the above code to a django application and run the `migrate` command (so that your database is created). 
Start the Django shell by

    python manage.py shell

This starts the standard python shell but with relevant Django libraries imported, so that you can directly focus on the important parts.

Start by importing the models we just defined (I am assuming this is done in a file `models.py`)

    from .models import Book, Author

Run your first select query:
    
    >>> Author.objects.all() 
    []
    >>> Book.objects.all()
    []

Lets create an author and book object: 

    >>> hawking = Author(name="Stephen hawking")
    >>> hawking.save()
    >>> history_of_time = Book(name="history of time", author=hawking)
    >>> history_of_time.save()

or use [create][1] function to create model objects and save in one line code

    >>> wings_of_fire = Book.objects.create(name="Wings of Fire", author="APJ Abdul Kalam")

Now lets run the query

    >>> Book.objects.all()
    [<Book: Book object>]
    >>> book = Book.objects.first() #getting the first book object
    >>> book.name
    u'history of time'

Let's add a where clause to our select query

    >>> Book.objects.filter(name='nothing')
    []
    >>> Author.objects.filter(name__startswith='Ste')
    [<Author: Author object>]

To get the details about the author of a given book

    >>> book = Book.objects.first() #getting the first book object
    >>> book.author.name # lookup on related model
    u'Stephen hawking'

To get all the books published by Stephen Hawking (Lookup book by its author)

    >>> hawking.book_set.all()
    [<Book: Book object>]

`_set` is the notation used for "Reverse lookups" i.e. while the lookup field is on the Book model, we can use `book_set` on an author object to get all his/her books.


  [1]: https://docs.djangoproject.com/en/1.10/ref/models/querysets/#django.db.models.query.QuerySet.create

## A basic unmanaged table.
At some point in your use of Django, you may find yourself wanting to interact with tables which have already been created, or with database views. In these cases, you would not want Django to manage the tables through its migrations. To set this up, you need to add only one variable to your model's `Meta` class: `managed = False`.

Here is an example of how you might create an unmanaged model to interact with a database view: 

    class Dummy(models.Model):
        something = models.IntegerField()
    
        class Meta:
           managed = False

This may be mapped to a view defined in SQL as follows.

    CREATE VIEW myapp_dummy AS 
    SELECT id, something FROM complicated_table 
    WHERE some_complicated_condition = True

Once you have this model created, you can use it as you would any other model:

    >>> Dummy.objects.all()
    [<Dummy: Dummy object>, <Dummy: Dummy object>, <Dummy: Dummy object>]
    >>> Dummy.objects.filter(something=42)
    [<Dummy: Dummy object>]

## Computed Values
Once a model object has been fetched, it becomes a fully realized instance of the class.  As such, any additional methods can be accessed in forms and serializers (like Django Rest Framework).

Using python properties is an elegant way to represent additional values that are not stored in the database due to varying circumstances.

```
def expire():
    return timezone.now() + timezone.timedelta(days=7)

class Coupon(models.Model):
    expiration_date = models.DateField(default=expire)

    @property
    def is_expired(self):
        return timezone.now() > self.expiration_date
```

While most cases you can supplement data with annotations on your querysets, computed values as model properties are ideal for computations that can not be evaluated simply within the scope of a query.

Additionally, properties, since they are declared on the python class and not as part of the schema, are not available for querying against.

## Model mixins
In same cases different models could have same fields and same procedures in the product life cycle.
To handle these similarities without having code repetition inheritance could be used.
Instead of inheriting a whole class, **mixin** design pattern offers us to inherit (*or some says include*) some methods and attributes.
Let's see an example:

    class PostableMixin(models.Model):
        class Meta:
            abstract=True
        
        sender_name = models.CharField(max_length=128)
        sender_address = models.CharField(max_length=255)
        receiver_name = models.CharField(max_length=128)
        receiver_address = models.CharField(max_length=255)
        post_datetime = models.DateTimeField(auto_now_add=True)
        delivery_datetime = models.DateTimeField(null=True)
        notes = models.TextField(max_length=500)

    class Envelope(PostableMixin):
        ENVELOPE_COMMERCIAL = 1
        ENVELOPE_BOOKLET = 2
        ENVELOPE_CATALOG = 3

        ENVELOPE_TYPES = (
            (ENVELOPE_COMMERCIAL, 'Commercial'),
            (ENVELOPE_BOOKLET, 'Booklet'),
            (ENVELOPE_CATALOG, 'Catalog'),
        )

        envelope_type = models.PositiveSmallIntegerField(choices=ENVELOPE_TYPES)
    
    class Package(PostableMixin):
        weight = models.DecimalField(max_digits=6, decimal_places=2)
        width = models.DecimalField(max_digits=5, decimal_places=2)
        height = models.DecimalField(max_digits=5, decimal_places=2)
        depth = models.DecimalField(max_digits=5, decimal_places=2)

To turn a model into an abstract class, you will need to mention `abstract=True` in its inner `Meta` class.
Django does not create any tables for abstract models in the database.
However for the models `Envelope` and `Package`, corresponding tables would be created in the database.

Furthermore the fields some model methods will be needed at more than one models. 
Thus these methods could be added to mixins to prevent code repetition.
For example if we create a method to set delivery date to `PostableMixin` it will be accesible from both of its children:

    class PostableMixin(models.Model):
        class Meta:
            abstract=True

        ...
        ...

        def set_delivery_datetime(self, dt=None):
            if dt is None:
                from django.utils.timezone import now
                dt = now()

            self.delivery_datetime = dt
            self.save()

This method could be used as following on the children:

    >> envelope = Envelope.objects.get(pk=1)
    >> envelope.set_delivery_datetime()

    >> pack = Package.objects.get(pk=1)
    >> pack.set_delivery_datetime()

## Inheritance
Inheritance among models can be done in two ways:
 - a common abstract class (see the "Model mixins" example)
 - a common model with multiple tables

The multi tables inheritance will create one table for the common fields and one per child model example:

    from django.db import models
    
    class Place(models.Model):
        name = models.CharField(max_length=50)
        address = models.CharField(max_length=80)
    
    class Restaurant(Place):
        serves_hot_dogs = models.BooleanField(default=False)
        serves_pizza = models.BooleanField(default=False)

will create 2 tables, one for `Place` and one for `Restaurant` with a hidden `OneToOne` field to `Place` for the common fields.

note that this will need an extra query to the places tables every time you fetch an Restaurant Object.



