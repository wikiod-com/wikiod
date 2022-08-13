---
title: "Database transactions"
slug: "database-transactions"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Atomic transactions
Problem
=======
By default, Django immediately commits changes to the database. When exceptions occur during a series of commits, this can leave your database in an unwanted state:

    def create_category(name, products):
        category = Category.objects.create(name=name)
        product_api.add_products_to_category(category, products)
        activate_category(category)

In the following scenario:

    >>> create_category('clothing', ['shirt', 'trousers', 'tie'])
    ---------------------------------------------------------------------------
    ValueError: Product 'trousers' already exists

An exception occurs whilst trying to add the trousers product to the clothing category. By this point, the category itself has already been added, and the shirt product has been added to it.

The incomplete category and containing product would have to be manually removed before fixing the code and calling the `create_category()` method once more, as otherwise a duplicate category would be created.

----

Solution
========
The `django.db.transaction` module allows you to combine multiple database changes into an [atomic transaction](https://en.wikipedia.org/wiki/Atomicity_(database_systems)):

> [a] series of database operations such that either all occur, or nothing occurs.

Applied to the above scenario, this can be applied as a [decorator](https://wiki.python.org/moin/PythonDecorators):

    from django.db import transaction

    @transaction.atomic
    def create_category(name, products):
        category = Category.objects.create(name=name)
        product_api.add_products_to_category(category, products)
        activate_category(category)

Or by using a [context manager](https://docs.python.org/2/reference/compound_stmts.html#with):

    def create_category(name, products):
        with transaction.atomic():
            category = Category.objects.create(name=name)
            product_api.add_products_to_category(category, products)
            activate_category(category)

Now, if an exception occurs at any stage within the transaction, no database changes will be committed.


