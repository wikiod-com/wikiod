---
title: "Getting started with activerecord"
slug: "getting-started-with-activerecord"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Java
In Java, activerecord pattern isn't very popular. Though there are some implementations:

* http://arjava.sourceforge.net/
* http://javalite.io/
* http://www.javalobby.org/articles/activeobjects/
* http://www.jooq.org/

## Ruby on Rails
ActiveRecord pattern was popularized by Rails. It's the default ORM there.

**Conventions**

Rails ActiveRecord is driven by conventions: class names are mapped to table names, field names are mapped to field names, foreign and primary keys should be named accordingly. These conventions can be overridden.

**Query**

Having the following schema:

    CREATE TABLE products (
       id int(11) NOT NULL auto_increment,
       name varchar(255),
       PRIMARY KEY  (id)
    );

And the following code:

    class Product < ApplicationRecord
    end
    p = Product.new
    p.name = "Some Book"
    p.save!

Will produce the following statement:

    INSERT INTO products (name) VALUES ("Some Book");

## Pseudocode
The pattern can be illustrated by the following pseudocode:

    product = new Product()
    product.name = "Some Book"
    product.price = 123.45
    product.save()

The following SQL would be a result:

    INSERT INTO products (name, price) VALUES ('Some Book', 123.45);



