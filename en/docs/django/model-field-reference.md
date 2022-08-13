---
title: "Model Field Reference"
slug: "model-field-reference"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

## Parameters

| Parameter | Details |
| --------- | ------- |
| null | If true, empty values may be stored as `null` in the database |
| blank | If true, then the field will not be required in forms. If fields are left blank, Django will use the default field value. |
| choices | An iterable of 2-element iterables to be used as choices for this field. If set, field is rendered as a drop-down in the admin. `[('m', 'Male'),('f','Female'),('z','Prefer Not to Disclose')]`. To group options, simply nest the values: `[('Video Source',((1,'YouTube'),(2,'Facebook')),('Audio Source',((3, 'Soundcloud'),(4, 'Spotify'))]`
| db_column | By default, django uses the field name for the database column. Use this to provide a custom name |
| db_index | If `True`, an index will be created on this field in the database |
| _db_tablespace_ | The tablespace to use for this field's index. _This field is only used if the database engine supports it, otherwise its ignored_. |
| default | The default value for this field. Can be a value, or a callable object. For mutable defaults (a list, a set, a dictionary) you **must** use a callable. Due to compatibility with migrations, you cannot use lambdas. |
| editable | If `False`, the field is not shown in the model admin or any `ModelForm`. Default is `True`. |
| error_messages | Used to customize the default error messages shown for this field. The value is a dictionary, with the keys representing the error and the value being the message. Default keys (for error messages) are `null`, `blank`, `invalid`, `invalid_choice`, `unique` and `unique_for_date`; additional error messages may be defined by custom fields. |
| help_text | Text to be displayed with the field, to assist users. HTML is allowed. |
| on_delete | When an object referenced by a ForeignKey is deleted, Django will emulate the behavior of the SQL constraint specified by the on_delete argument. This is the second positional argument for both `ForeignKey` and `OneToOneField` fields. Other fields do not have this argument. |
| primary_key | If `True`, this field will be the primary key. Django automatically adds a primary key; so this is only required if you wish to create a custom primary key. You can only have one primary key per model.|
| unique | If `True`, errors are raised if duplicate values are entered for this field. This is a database-level restriction, and not simply a user-interface block.|
| unique_for_date | Set the value to a `DateField` or `DateTimeField`, and errors will be raised if there are duplicate values _for the same date or date time_.|
| unique_for_month | Similar to `unique_for_date`, except checks are limited for the month. |
| unique_for_year | Similar to `unique_for_date`, except checks are limited to the year. |
| verbose_name | A friendly name for the field, used by django in various places (such as creating labels in the admin and model forms). |
| validators | A list of [validators](https://docs.djangoproject.com/en/1.9/ref/validators/) for this field.|






 


 - You can write your own fields if you find it necessary
 - You can override functions of the base model class, most commonly the `save()` function

## Number Fields
Examples of numeric fields are given:

**AutoField**

An auto-incrementing integer generally used for primary keys.

    from django.db import models

    class MyModel(models.Model):
        pk = models.AutoField()

> Each model gets a primary key field (called `id`) by default. Therefore, it is not necessary to duplicate an id field in the model for the purposes of a primary key.

----------

**BigIntegerField**

An integer fitting numbers from `-9223372036854775808` to `9223372036854775807`(`8 Bytes`).

    from django.db import models

    class MyModel(models.Model):
        number_of_seconds = models.BigIntegerField()

----------

**IntegerField**

The IntegerField is used to store integer values from -2147483648 to 2147483647 (`4 Bytes`).

    from django.db import models
    
    class Food(models.Model):
        name = models.CharField(max_length=255)
        calorie = models.IntegerField(default=0)

> `default` parameter is not mandatory. 
> But it's useful to set a default value. 

----------

**PositiveIntegerField**

Like an IntegerField, but must be either positive or zero (0). The PositiveIntegerField is used to store integer values from 0 to 2147483647 (`4 Bytes`). This can be useful at field which should be semantically positive. For example if you are recording foods with its calories, it should not be negative. This field will prevent negative values via its validations.

    from django.db import models
    
    class Food(models.Model):
        name = models.CharField(max_length=255)
        calorie = models.PositiveIntegerField(default=0)

> `default` parameter is not mandatory. 
> But it's useful to set a default value.

----------

**SmallIntegerField**

The SmallIntegerField is used to store integer values from -32768 to 32767 (`2 Bytes`).
This field is useful for values not are not extremes. 

    from django.db import models
    
    class Place(models.Model):
        name = models.CharField(max_length=255)
        temperature = models.SmallIntegerField(null=True)

----------

**PositiveSmallIntegerField**

The SmallIntegerField is used to store integer values from 0to 32767 (`2 Bytes`).
Just like SmallIntegerField this field is useful for values not going so high and  should be semantically positive. For example it can store age which cannot be negative.

    from django.db import models
    
    class Staff(models.Model):
        first_name = models.CharField(max_length=255)
        last_name = models.CharField(max_length=255)
        age = models.PositiveSmallIntegerField(null=True)

Besides PositiveSmallIntegerField is useful for choices, this is the Djangoic way of implementing Enum:

    from django.db import models
    from django.utils.translation import gettext as _

    APPLICATION_NEW = 1
    APPLICATION_RECEIVED = 2
    APPLICATION_APPROVED = 3
    APPLICATION_REJECTED = 4
    
    APLICATION_CHOICES = (
        (APPLICATION_NEW, _('New')),
        (APPLICATION_RECEIVED, _('Received')),
        (APPLICATION_APPROVED, _('Approved')),
        (APPLICATION_REJECTED, _('Rejected')),
    )
    
    class JobApplication(models.Model):
        first_name = models.CharField(max_length=255)
        last_name = models.CharField(max_length=255)
        status = models.PositiveSmallIntegerField(
            choices=APLICATION_CHOICES, 
            default=APPLICATION_NEW
        )
        ...

> Definition of the choices as class variables or module variables according to the situation is a good way to use them. If choices are passed to field without friendly names than it will create confusion. 


----------
**DecimalField**

A fixed-precision decimal number, represented in Python by a Decimal instance.
Unlike IntegerField and its derivatives this field has 2 required arguments:

 1. *DecimalField.max_digits*: The maximum number of digits allowed in the number. Note that this number must be greater than or equal to decimal_places.
 2. *DecimalField.decimal_places*: The number of decimal places to store with the number.

If you want to store numbers up to 99 with 3 decimal places you need use `max_digits=5` and `decimal_places=3`:


    class Place(models.Model):
        name = models.CharField(max_length=255)
        atmospheric_pressure = models.DecimalField(max_digits=5, decimal_places=3)



## ForeignKey
ForeignKey field is used to create a `many-to-one` relationship between models. 
Not like the most of other fields requires positional arguments.
The following example demonstrates the car and owner relation:

    from django.db import models
    
    class Person(models.Model):
        GENDER_FEMALE = 'F'
        GENDER_MALE = 'M'
    
        GENDER_CHOICES = (
            (GENDER_FEMALE, 'Female'),
            (GENDER_MALE, 'Male'),
        )
    
        first_name = models.CharField(max_length=100)
        last_name = models.CharField(max_length=100)
        gender = models.CharField(max_length=1, choices=GENDER_CHOICES)
        age = models.SmallIntegerField()
    

    class Car(model.Model)
        owner = models.ForeignKey('Person')
        plate = models.CharField(max_length=15)
        brand = models.CharField(max_length=50)
        model = models.CharField(max_length=50)
        color = models.CharField(max_length=50)

The first argument of the field is the class to which the model is related.
The second positional argument is `on_delete` argument. 
In the current versions this argument is not required, but it will be required in Django 2.0. 
The default functionality of the argument is shown as following:

    class Car(model.Model)
        owner = models.ForeignKey('Person', on_delete=models.CASCADE)
        ...
    
> This will cause Car objects to be deleted from the model when its owner deleted from Person model.
> This is the default functionality.


    class Car(model.Model)
        owner = models.ForeignKey('Person', on_delete=models.PROTECT)
        ...
    
> This will prevents Person objects to be deleted if they are related to at least one Car object.
> All of the Car objects which reference a Person object should be deleted first.
> And then the Person Object can be deleted.


## CharField
The CharField is used for storing defined lengths of text. In the example below up to 128 characters of text can be stored in the field. Entering a string longer than this will result in a validation error being raised.

    from django.db import models

    class MyModel(models.Model):
        name = models.CharField(max_length=128, blank=True)

## DateTimeField
DateTimeField is used to store date time values.

    class MyModel(models.Model):
        start_time = models.DateFimeField(null=True, blank=True)
        created_on = models.DateTimeField(auto_now_add=True)
        updated_on = models.DateTimeField(auto_now=True)

A `DateTimeField` has two optional parameters:

- `auto_now_add` sets the value of the field to current datetime when the object is created.

- `auto_now` sets the value of the field to current datetime every time the field is saved.

These options and the `default` parameter are mutually exclusive. 


## BinaryField
This is a specialized field, used to store binary data. It only accepts **bytes**. Data is base64 serialized upon storage.

As this is storing binary data, this field cannot be used in a filter.

    from django.db import models

    class MyModel(models.Model):
        my_binary_data = models.BinaryField()



