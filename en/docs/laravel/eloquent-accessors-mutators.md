---
title: "Eloquent Accessors & Mutators"
slug: "eloquent-accessors--mutators"
draft: false
images: []
weight: 9935
type: docs
toc: true
---

Accessors and mutators allow you to format Eloquent attribute values when you retrieve or set them on model instances. For example, you may want to use the Laravel encrypter to encrypt a value while it is stored in the database, and then automatically decrypt the attribute when you access it on an Eloquent model.

In addition to custom accessors and mutators, Eloquent can also automatically cast date fields to Carbon instances or even cast text fields to JSON.

## Syntax
- set{ATTRIBUTE}Attribute($attribute) // in camel case 

## Defining An Accessors
    <?php
    
    namespace App;
    
    use Illuminate\Database\Eloquent\Model;
    
    class User extends Model
    {
        /**
         * Get the user's first name.
         *
         * @param  string  $value
         * @return string
         */
        public function getFirstNameAttribute($value)
        {
            return ucfirst($value);
        }
    }

# Getting Accessor:

As you can see, the original value of the column is passed to the accessor, allowing you to manipulate and return the value. To access the value of the accessor, you may simply access the `first_name` attribute on a model instance:

    $user = App\User::find(1);
    $firstName = $user->first_name;



## Defining a Mutator
    class User extends Model  
    {
        public function setPasswordAttribute($password)
        {
            $this->attributes['password'] = bcrypt($password);
        } 
        ... 
    }

Above code does "bcrypting" each time password property is set.

    $user = $users->first();
    $user->password = 'white rabbit'; //laravel calls mutator on background
    $user->save(); // password is bcrypted and one does not need to call bcrypt('white rabbit')





