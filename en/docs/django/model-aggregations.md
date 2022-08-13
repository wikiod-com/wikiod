---
title: "Model Aggregations"
slug: "model-aggregations"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

Aggregations are methods allowing the execution of operations on (individual and/or groups of) rows of objects derived from a Model.

## Average, Minimum, Maximum, Sum from Queryset
    class Product(models.Model):
        name = models.CharField(max_length=20)
        price = models.FloatField()

To Get average price of all products:

    >>> from django.db.models import Avg, Max, Min, Sum
    >>> Product.objects.all().aggregate(Avg('price'))
    # {'price__avg': 124.0}
To Get Minimum price of all products:

    >>> Product.objects.all().aggregate(Min('price'))
    # {'price__min': 9}

To Get Maximum price of all products:

    >>> Product.objects.all().aggregate(Max('price'))
    # {'price__max':599 }

To Get SUM of prices of all products:

    >>> Product.objects.all().aggregate(Sum('price'))
    # {'price__sum':92456 }



## Count the number of foreign relations
    class Category(models.Model):
        name = models.CharField(max_length=20)
    
    
    class Product(models.Model):
        name = models.CharField(max_length=64)
        category = models.ForeignKey(Category, on_delete=models.PROTECT)

To get the number products for each category:

    >>> categories = Category.objects.annotate(Count('product'))

This adds the `<field_name>__count` attribute to each instance returned:
 
    >>> categories.values_list('name', 'product__count')
    [('Clothing', 42), ('Footwear', 12), ...]

You can provide a custom name for your attribute by using a keyword argument:

    >>> categories = Category.objects.annotate(num_products=Count('product'))

You can use the annotated field in querysets:

    >>> categories.order_by('num_products')
    [<Category: Footwear>, <Category: Clothing>]
    
    >>> categories.filter(num_products__gt=20)
    [<Category: Clothing>]



## GROUB BY ... COUNT/SUM Django ORM equivalent
We can perform a `GROUP BY ... COUNT` or a `GROUP BY ... SUM` SQL equivalent queries on Django ORM, with the use of `annotate()`, `values()`, `order_by()` and the `django.db.models`'s `Count` and `Sum` methods respectfully:

Let our model be:

       class Books(models.Model):
           title  = models.CharField()
           author = models.CharField()
           price = models.FloatField()


**`GROUP BY ... COUNT`**:

 - Lets assume that we want to count how many book objects per distinct author exist in our `Books` table:

       result = Books.objects.values('author')
                             .order_by('author')
                             .annotate(count=Count('author'))
      

 - Now `result` contains a queryset with two columns: `author` and `count`:

         author    | count
       ------------|-------
        OneAuthor  |   5
       OtherAuthor |   2
          ...      |  ...

<hr>

**`GROUB BY ... SUM`**:

 - Lets assume that we want to sum the price of all the books per distinct author that exist in our `Books` table:

        result = Books.objects.values('author')
                              .order_by('author')
                              .annotate(total_price=Sum('price'))

 - Now `result` contains a queryset with two columns: `author` and `total_price`:

         author    | total_price
       ------------|-------------
        OneAuthor  |    100.35
       OtherAuthor |     50.00
           ...     |      ...
    
                           

       

