---
title : rails-activerecord Tutorial
slug : rails-activerecord-tutorial
weight : 9993
draft : false
images : []
type : docs
---

## Active Record ##
It's the M in MVC - the model - which is the layer of the system responsible for representing business data and logic. Active Record facilitates the creation and use of business objects whose data requires persistent storage to a database. It is an implementation of the Active Record pattern which itself is a description of an Object Relational Mapping system.

When writing applications using other programming languages or frameworks, it may be necessary to write a lot of configuration code. This is particularly true for ORM frameworks in general. However, if you follow the conventions adopted by Rails, you'll need to write very little configuration (in some cases no configuration at all) when creating Active Record models. The idea is that if you configure your applications in the very same way most of the time then this should be the default way. Thus, explicit configuration would be needed only in those cases where you can't follow the standard convention.

### Active Record automatically creates methods to allow an application to read and manipulate data stored within its tables. ###

Rails provides a domain-specific language for **managing a database schema** called migrations. Migrations are stored in files which are executed against any database that Active Record supports using rake. 

**Active Record allows you to validate** the state of a model before it gets written into the database. There are several methods that you can use to check your models and validate that an attribute value is not empty, is unique and not already in the database, follows a specific format and many more.

**Active Record callbacks** allow you to attach code to certain events in the life-cycle of your models. This enables you to add behavior to your models by transparently executing code when those events occur, like when you create a new record, update it, destroy it and so on.



----------


Text extracted from [Rails Guides][1], with some editting and
   modifications

  [1]: http://guides.rubyonrails.org/active_record_basics.html

