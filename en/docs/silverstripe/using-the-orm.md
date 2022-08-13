---
title: "Using the ORM"
slug: "using-the-orm"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Reading and writing DataObjects
DataObjects in SilverStripe represent a database table row. The fields in the model have magic methods that handle getting and setting data via their property names.

Given we have a simple DataObject as an example:

    class Fruit extends DataObject
    {
        private static $db = ['Name' => 'Varchar'];
    }

You can create, set data and write a `Fruit` as follows:

    $apple = Fruit::create();
    $apple->Name = 'Apple';
    $apple->write();

You can similarly retrieve the `Fruit` object as follows:

    $apple = Fruit::get()->filter('Name', 'Apple')->first();
    var_dump($apple->Name); // string(5) "Apple"

