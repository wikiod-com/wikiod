---
title: "Eloquent"
slug: "eloquent"
draft: false
images: []
weight: 9415
type: docs
toc: true
---

The Eloquent is an ORM (Object Relational Model) included with the Laravel. It implements the active record pattern and is used to interact with relational databases.

**Table naming**

The convention is to use pluralised “snake_case” for table names and singular “StudlyCase” for model names. For example:

* A `cats` table would have a `Cat` model
* A `jungle_cats` table would have a `JungleCat` model
* A `users` table would have a `User` model
* A `people` table would have a `Person` model

Eloquent will automatically try to bind your model with a table that has the plural of the name of the model, as stated above.

You can, however, specify a table name to override the default convention.

<!-- language: php -->
    class User extends Model
    { 
        protected $table = 'customers';
    }

## Introduction
Eloquent is the [ORM][1] built into the Laravel framework. It allows you to interact with your database tables in an object-oriented manner, by use of the [ActiveRecord][2] pattern.

A single model class usually maps to a single database table, and also relationships of different types ([one-to-one][3], [one-to-many][4], [many-to-many][5], polymorphic) can be defined between different model classes.

Section [Making a Model][6] describes the creation and definition of model classes.

Before you can start using Eloquent models, make sure at least one database connection has been configured in your `config/database.php` configuration file.

To understand usage of eloquent query builder during development you may use `php artisan ide-helper:generate` command. Here is the [link][7].

---

## Sub-topic Navigation
[Eloquent Relationship][8]


  [1]: https://en.wikipedia.org/wiki/Object-relational_mapping
  [2]: https://en.wikipedia.org/wiki/Active_record_pattern
  [3]: https://www.wikiod.com/laravel/eloquent--relationship
  [4]: https://www.wikiod.com/laravel/eloquent--relationship#Relationship Types
  [5]: https://www.wikiod.com/laravel/eloquent--relationship#Many To Many
  [6]: https://www.wikiod.com/laravel/eloquent#Making a Model
  [7]: https://github.com/barryvdh/laravel-ide-helper
  [8]: https://www.wikiod.com/laravel/eloquent--relationship

## Deleting
<!-- language-all: lang-php -->

You can delete data after writing it to the database. You can either delete a model instance if you have retrieved one, or specify conditions for which records to delete.

To delete a model instance, retrieve it and call the `delete()` method:

    $user = User::find(1);
    $user->delete();

Alternatively, you can specify a primary key (or an array of primary keys) of the records you wish to delete via the `destroy()` method:

    User::destroy(1);
    User::destroy([1, 2, 3]);

You can also combine querying with deleting:

    User::where('age', '<', 21)->delete();

This will delete all users who match the condition.

> Note: When executing a mass delete statement via Eloquent, the `deleting` and `deleted` model events will not be fired for the deleted models. This is because the models are never actually retrieved when executing the delete statement.

## Soft Deleting

Some times you don’t want to permanently delete a record, but keep it around for auditing or reporting purposes. For this, Eloquent provides _soft deleting_ functionality.

To add soft deletes functionality to your model, you need to import the `SoftDeletes` trait and add it to your Eloquent model class:

    namespace Illuminate\Database\Eloquent\Model;
    namespace Illuminate\Database\Eloquent\SoftDeletes;

    class User extends Model
    {
         use SoftDeletes;
    }

When deleting a model, it will set a timestamp on a `deleted_at` timestamp column in the table for your model, so be sure to create the `deleted_at` column in your table first. Or in migration you should call `softDeletes()` method on your blueprint to add the `deleted_at` timestamp. Example:

    Schema::table('users', function ($table) {
        $table->softDeletes();
    });

Any queries will omit soft-deleted records. You can force-show them if you wish by using the `withTrashed()` scope:

    User::withTrashed()->get();

If you wish to allow users to _restore_ a record after soft-deleting (i.e. in a trash can-type area) then you can use the `restore()` method:

    $user = User::find(1);
    $user->delete();
    $user->restore();

To forcefully delete a record use the `forceDelete()` method which will truly remove the record from the database:

    $user = User::find(1);
    $user->forceDelete();

## Change primary key and timestamps
<!-- Language-All: php -->

By default, Eloquent models expect for the primary key to be named `'id'`. If that is not your case, you can change the name of your primary key by specifying the `$primaryKey` property.

    class Citizen extends Model
    {
        protected $primaryKey = 'socialSecurityNo';

        // ...
    }

Now, any Eloquent methods that use your primary key (e.g. `find` or `findOrFail`) will use this new name.

Additionally, Eloquent expects the primary key to be an auto-incrementing integer. If your primary key is not an auto-incrementing integer (e.g. a GUID), you need to tell Eloquent by updating the `$incrementing` property to `false`:

    class Citizen extends Model
    {
        protected $primaryKey = 'socialSecurityNo';

        public $incrementing = false;

        // ...
    }

By default, Eloquent expects `created_at` and `updated_at` columns to exist on your tables. If you do not wish to have these columns automatically managed by Eloquent, set the `$timestamps` property on your model to false:

    class Citizen extends Model
    {
        public $timestamps = false;

        // ...
    }

If you need to customize the names of the columns used to store the timestamps, you may set the  `CREATED_AT` and `UPDATED_AT` constants in your model:

    class Citizen extends Model
    {
        const CREATED_AT = 'date_of_creation';
        const UPDATED_AT = 'date_of_last_update';

        // ...
    }

## Persisting
<!-- Language-all: php -->

In addition to reading data with Eloquent, you can also use it to insert or update data with the `save()` method. If you have created a new model instance then the record will be _inserted_; otherwise, if you have retrieved a model from the database and set new values, it will be _updated_.

In this example we create a new `User` record:

    $user = new User();
    $user->first_name = 'John';
    $user->last_name = 'Doe';
    $user->email = 'john.doe@example.com';
    $user->password = bcrypt('my_password');
    $user->save();

You can also use the `create` method to populate fields using an array of data:

    User::create([
        'first_name'=> 'John',
        'last_name' => 'Doe',
        'email'     => 'john.doe@example.com', 
        'password'  => bcrypt('changeme'),
    ]);

When using the create method your attributes should be declared in the `fillable` array within your model:

    class User extends Model
    {
        protected $fillable = [
             'first_name',
             'last_name',
             'email',
             'password',
        ];
    }

Alternatively, if you would like to make all attributes mass assignable, you may define the $guarded property as an empty array:

    class User extends Model
    {
        /**
        * The attributes that aren't mass assignable.
        *
        * @var array
        */
        protected $guarded = [];
    }

But you can also create a record without even changing `fillable` attribute in your model by using `forceCreate` method rather than `create` method

    User::forceCreate([
        'first_name'=> 'John',
        'last_name' => 'Doe',
        'email'     => 'john.doe@example.com', 
        'password'  => bcrypt('changeme'),
    ]);

The following is an example of updating an existing `User` model by first loading it (by using `find`), modifying it, and then saving it:

    $user = User::find(1);
    $user->password = bcrypt('my_new_password');
    $user->save();

To accomplish the same feat with a single function call, you may use the `update` method:

    $user->update([
        'password' => bcrypt('my_new_password'),
    ]);

The `create` and `update` methods make working with large sets of data much simpler than having to set each key/value pair individually, as shown in the following examples:
  
> Note the use of `only` and `except` when gathering request data. It's important you specify the exact keys you want to allow/disallow to be updated, otherwise it's possible for an attacker to send additional fields with their request and cause unintended updates.

    // Updating a user from specific request data
    $data = Request::only(['first_name', 'email']);
    $user->find(1);
    $user->update($data);

    // Create a user from specific request data
    $data = Request::except(['_token', 'profile_picture', 'profile_name']);
    $user->create($data);


## Throw 404 if entity not found
If you want to automatically throw an exception when searching for a record that isn't found on a modal, you can use either

<!-- language-all: php -->

```
Vehicle::findOrFail(1);
```

or

```
Vehicle::where('make', 'ford')->firstOrFail();
```

If a record with the primary key of `1` is not found, a [`ModelNotFoundException`](https://laravel.com/api/5.2/Illuminate/Database/Eloquent/ModelNotFoundException.html) is thrown. Which is essentially the same as writing ([view source](https://github.com/laravel/framework/blob/a5e56a5cd11983b0b67740a6af2f25e23cd9b8e8/src/Illuminate/Database/Eloquent/Builder.php#L297-L304)):

<!-- language: lang-php -->

```
$vehicle = Vehicle::find($id);

if (!$vehicle) {
    abort(404);
}
```

## Cloning Models
You may find yourself needing to clone a row, maybe change a few attributes but you need an efficient way to keep things DRY. Laravel provides a sort of 'hidden' method to allow you to do this functionality. Though it is completely undocumented, you need to search through the API to find it.

Using `$model->replicate()` you can easily clone a record

<!-- language: php -->

    $robot = Robot::find(1);
    $cloneRobot = $robot->replicate();
    // You can add custom attributes here, for example he may want to evolve with an extra arm!
    $cloneRobot->arms += 1;
    $cloneRobot->save();

The above would find a robot that has an ID of 1, then clones it.

