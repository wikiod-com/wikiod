---
title: "Eloquent  Relationship"
slug: "eloquent--relationship"
draft: false
images: []
weight: 9913
type: docs
toc: true
---

## Inserting Related Models
Suppose you have a `Post` model with a `hasMany` relationship with `Comment`. You may insert a `Comment` object related to a post by doing the following:

<!-- language: php -->
    $post = Post::find(1);

    $commentToAdd = new Comment(['message' => 'This is a comment.']);

    $post->comments()->save($commentToAdd);

You can save multiple models at once using the `saveMany` function:

<!-- language: php -->
    $post = Post::find(1);

    $post->comments()->saveMany([
        new Comment(['message' => 'This a new comment']),
        new Comment(['message' => 'Me too!']),
        new Comment(['message' => 'Eloquent is awesome!'])
    ]);

Alternatively, there's also a `create` method which accepts a plain PHP array instead of an Eloquent model instance.

<!-- language: php -->
    $post = Post::find(1);

    $post->comments()->create([
        'message' => 'This is a new comment message'
    ]);

## Relationship Types
**One to Many**
----------


<!-- language-all: lang-php -->

Lets say that each Post may have one or many comments and each comment belongs to just a single Post. 

so the comments table will be having `post_id`. In this case the relationships will be as follows.

**Post Model**

    public function comments()
    {
       return $this->belongsTo(Post::class);
    }

If the foreign key is other than `post_id`, for example the foreign key is `example_post_id`. 

    public function comments()
    {
       return $this->belongsTo(Post::class, 'example_post_id');
    }

and plus, if the local key is other than `id`, for example the local key is `other_id`

    public function comments()
    {
       return $this->belongsTo(Post::class, 'example_post_id', 'other_id');
    }


**Comment Model**

defining inverse of one to many

    public function post()
    {
       return $this->hasMany(Comment::class);
    }


**One to One**
----------

## How to associate between two models (example: `User` and `Phone` model)

`App\User`

<!-- language: php -->
    <?php

    namespace App;

    use Illuminate\Database\Eloquent\Model;

    class User extends Model
    {
        /**
         * Get the phone record associated with the user.
         */
        public function phone()
        {
            return $this->hasOne('Phone::class', 'foreign_key', 'local_key');
        }
    }

`App\Phone`

<!-- language: php -->
    <?php

    namespace App;

    use Illuminate\Database\Eloquent\Model;

    class Phone extends Model
    {
        /**
         * Get the user that owns the phone.
         */
        public function user()
        {
            return $this->belongsTo('User::class', 'foreign_key', 'local_key');
        }
    }

`foreign_key` : By default Eloquent will assume this value to be `other_model_name_id` (in this case `user_id` and `phone_id`), change it if it isn't the case.

`local_key` : By default Eloquent will assume this value to be `id` (current model primary key), change it if it isn't the case.

> If your database filed name as per laravel standard, you don't need to provide foreign key and local key in relationship declaration 
### Explanation


**Many to Many**
----------
Lets say there is roles and permissions. Each role may belongs to many permissions and each permission may belongs to many role. so there will be 3 tables. two models and one pivot table. a `roles`, `users` and `permission_role` table.

**Role Model**

<!-- language: php -->
    public function permissions()
    {
       return $this->belongsToMany(Permission::class);
    }

**Permission Model**

<!-- language: php -->
    public function roles()
    {
       return $this->belongsToMany(Roles::class);
    }

***Note: 1***

*consider following while using different table name for pivot table.*

Suppose if you want to use `role_permission` instead of `permission_role`, as eloquent uses alphabetic order for building the pivot key names. you will need to pass pivot table name as second parameter as follows.

**Role Model**

<!-- language: php -->
    public function permissions()
    {
       return $this->belongsToMany(Permission::class, 'role_permission');
    }

**Permission Model**

<!-- language: php -->
    public function roles()
    {
       return $this->belongsToMany(Roles::class, 'role_permission');
    }

***Note: 2***

*consider following while using different key names in pivot table.*

Eloquent assumes that if no keys are passed as third and fourth parameters that it will be the singular table names with `_id`. so it assumes that the pivot will be having `role_id` and `permission_id` fields. If keys other than these are to be used it should be passed as third and fourth parameters. 

Lets say if `other_role_id` instead of `role_id` and `other_permission_id` instead of `permission_id` is to be used. So it would be as follows.

**Role Model**

<!-- language: php -->
    public function permissions()
    {
       return $this->belongsToMany(Permission::class, 'role_permission', 'other_role_id', 'other_permission_id');
    }

**Permission Model**

<!-- language: php -->
    public function roles()
    {
       return $this->belongsToMany(Roles::class, 'role_permission', 'other_permission_id', 'other_role_id');
    }

**Polymorphic**
----------

Polymorphic relations allow a model to belong to more than one other model on a single association. A good example would be images, both a user and a product can have an image. The table structure might look as follows:

    user
        id - integer
        name - string
        email - string
    
    product
        id - integer
        title - string
        SKU - string
    
    image
        id - integer
        url - string
        imageable_id - integer
        imageable_type - string

The important columns to look at are in the images table. The `imageable_id` column will contain the ID value of the user or product, while the `imageable_type` column will contain the class name of the owning model. In your models, you setup the relations as follows:

    <?php
    
    namespace App;
    
    use Illuminate\Database\Eloquent\Model;
    
    class Image extends Model
    {
        /**
         * Get all of the owning imageable models.
         */
        public function imageable()
        {
            return $this->morphTo();
        }
    }
    
    class User extends Model
    {
        /**
         * Get all of the user's images.
         */
        public function images()
        {
            return $this->morphMany('Image::class', 'imageable');
        }
    }
    
    class Product extends Model
    {
        /**
         * Get all of the product's images.
         */
        public function images()
        {
            return $this->morphMany('Image::class', 'imageable');
        }
    }

You may also retrieve the owner of a polymorphic relation from the polymorphic model by accessing the name of the method that performs the call to `morphTo`. In our case, that is the `imageable` method on the Image model. So, we will access that method as a dynamic property

    $image = App\Image::find(1);
    
    $imageable = $image->imageable;

This `imageable` will return either a User or a Product.

## Querying on relationships
Eloquent also lets you query on defined relationships, as show below:

<!-- language: lang-php -->


    User::whereHas('articles', function (Builder $query) {
        $query->where('published', '!=', true);
    })->get();


This requires that your relationship method name is `articles` in this case. The argument passed into the closure is the Query Builder for the related model, so you can use any queries here that you can elsewhere.

**Eager Loading**

Suppose User model has a relationship with Article model and you want to eager load the related articles. This means the articles of the user will be loaded while retrieving user.
 
`articles` is the relationship name (method) in User model.
 
<!-- language: php -->
    User::with('articles')->get();

if you have multiple relationship. for example articles and posts.

<!-- language: php -->
    User::with('articles','posts')->get();

and to select nested relationships 

<!-- language: php -->
    User::with('posts.comments')->get();

Call more than one nested relationship
<!-- language: php -->
    User::with('posts.comments.likes')->get()


## Many To Many
Lets say there is roles and permissions. Each role may belongs to many permissions and each permission may belongs to many role. so there will be 3 tables. two models and one pivot table. a `roles`, `users` and `permission_role` table.

**Role Model**

<!-- language: php -->
    public function permissions()
    {
       return $this->belongsToMany(Permission::class);
    }

**Permission Model**

<!-- language: php -->
    public function roles()
    {
       return $this->belongsToMany(Roles::class);
    }

***Note: 1***

*consider following while using different table name for pivot table.*

Suppose if you want to use `role_permission` instead of `permission_role`, as eloquent uses alphabetic order for building the pivot key names. you will need to pass pivot table name as second parameter as follows.

**Role Model**

<!-- language: php -->
    public function permissions()
    {
       return $this->belongsToMany(Permission::class, 'role_permission');
    }

**Permission Model**

<!-- language: php -->
    public function roles()
    {
       return $this->belongsToMany(Roles::class, 'role_permission');
    }

***Note: 2***

*consider following while using different key names in pivot table.*

Eloquent assumes that if no keys are passed as third and fourth parameters that it will be the singular table names with `_id`. so it assumes that the pivot will be having `role_id` and `permission_id` fields. If keys other than these are to be used it should be passed as third and fourth parameters. 

Lets say if `other_role_id` instead of `role_id` and `other_permission_id` instead of `permission_id` is to be used. So it would be as follows.

**Role Model**

<!-- language: php -->
    public function permissions()
    {
       return $this->belongsToMany(Permission::class, 'role_permission', 'other_role_id', 'other_permission_id');
    }

**Permission Model**

<!-- language: php -->
    public function roles()
    {
       return $this->belongsToMany(Roles::class, 'role_permission', 'other_permission_id', 'other_role_id');
    }

**Accessing Intermediate table using withPivot()**

Suppose you have a third column '**permission_assigned_date**' in the pivot table . By default, only the model keys will be present on the pivot object. Now to get this column in query result you need to add the name in withPivot() function.


       public function permissions()
            {
               return $this->belongsToMany(Permission::class, 'role_permission', 'other_role_id', 'other_permission_id')->withPivot('permission_assigned_date');
            }

**Attaching / Detaching**

Eloquent also provides a few additional helper methods to make working with related models more convenient. For example, let's imagine a user can have many roles and a role can have many permissions. To attach a role to a permission by inserting a record in the intermediate table that joins the models, use the attach method:

    $role= App\Role::find(1);    
    $role->permissions()->attach($permissionId);

When attaching a relationship to a model, you may also pass an array of additional data to be inserted into the intermediate table:

    $rol->roles()->attach($permissionId, ['permission_assigned_date' => $date]);

Similarly, To remove a specific permission  against a role use detach function


    $role= App\Role::find(1);
    //will remove permission 1,2,3 against role 1
    $role->permissions()->detach([1, 2, 3]);
    
**Syncing Associations**

You may also use the sync method to construct many-to-many associations. The sync method accepts an array of IDs to place on the intermediate table. Any IDs that are not in the given array will be removed from the intermediate table. So, after this operation is complete, only the IDs in the given array will exist in the intermediate table:

    //will keep permission id's 1,2,3 against Role id 1

    $role= App\Role::find(1)
    $role->permissions()->sync([1, 2, 3]);

    
    

## Introduction
Eloquent relationships are defined as functions on your Eloquent model classes. Since, like Eloquent models themselves, relationships also serve as powerful query builders, defining relationships as functions provides powerful method chaining and querying capabilities. For example, we may chain additional constraints on this posts relationship:

<!-- language: php -->
    $user->posts()->where('active', 1)->get();
    

[Navigate to parent topic][1]

   [1]:https://www.wikiod.com/laravel/eloquent

