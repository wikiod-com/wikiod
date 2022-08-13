---
title: "Complex Types"
slug: "complex-types"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Code First Complex Types
A complex type allows you to map selected fields of a database table into a single type that is a child of the main type.

    [ComplexType]
    public class Address
    {
        public string Street { get; set; }
        public string Street_2 { get; set; }
        public string City { get; set; }
        public string State { get; set; }
        public string ZipCode { get; set; }
    }

This complex type can then be used in multiple entity types. It can even be used more than once in the same entity type.

    public class Customer
    {
        public int Id { get; set; }
        public string Name { get; set; }
        ...
        public Address ShippingAddress { get; set; }
        public Address BillingAddress { get; set; }
    }

This entity type would then be stored in a table in the database that would look something like this.

[![Customers Table][1]][1]


  [1]: http://i.stack.imgur.com/DgIwf.png

Of course, in this case, a 1:n association (Customer-Address) would be the preferred model, but the example shows how complex types can be used.

