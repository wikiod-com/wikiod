---
title: "Collections"
slug: "collections"
draft: false
images: []
weight: 9822
type: docs
toc: true
---

## Syntax
 - $collection = collect(['Value1', 'Value2', 'Value3']); // Keys default to 0, 1, 2, ...,



`Illuminate\Support\Collection` provides a fluent and convenient interface to deal with arrays of data. You may well have used these without knowing, for instance Model queries that fetch multiple records return an instance of `Illuminate\Support\Collection`.

For up to date documentation on Collections you can find the official documentation [here][1]


  [1]: https://laravel.com/docs/master/collections

## Creating Collections
Using the `collect()` helper, you can easily create new collection instances by passing in an array such as:

<!-- language: php -->
    $fruits = collect(['oranges', 'peaches', 'pears']);

If you don't want to use helper functions, you can create a new Collection using the class directly:

<!-- language: php -->
    $fruits = new Illuminate\Support\Collection(['oranges', 'peaches', 'pears']);

As mentioned in the remarks, Models by default return a `Collection` instance, however you are free to create your own collections as needed. If no array is specified on creation, an empty Collection will be created.

## where()
You can select certain items out of a collection by using the `where()` method.

<!-- language: php -->
    $data = [
        ['name' => 'Taylor',  'coffee_drinker' => true],
        ['name' => 'Matt', 'coffee_drinker' => true]
    ];

    $matt = collect($data)->where('name', 'Matt');

This bit of code will select all items from the collection where the name is 'Matt'. In this case, only the second item is returned.

# Nesting

Just like most array methods in Laravel, `where()` supports searching for nested elements as well. Let's extend the example above by adding a second array:

<!-- language: php -->
    $data = [
        ['name' => 'Taylor',  'coffee_drinker' => ['at_work' => true, 'at_home' => true]],
        ['name' => 'Matt', 'coffee_drinker' => ['at_work' => true, 'at_home' => false]]
    ];

    $coffeeDrinkerAtHome = collect($data)->where('coffee_drinker.at_home', true);

This will only return Taylor, as he drinks coffee at home. As you can see, nesting is supported using the dot-notation.

# Additions

When creating a Collection of objects instead of arrays, those can be filtered using `where()` as well. The Collection will then try to receive all desired properties.

<!-- if version [gte 5.3] -->
Please note, that since Laravel 5.3 the `where()` method will try to loosely compare the values by default. That means when searching for `(int)1`, all entries containing `'1'` will be returned as well. If you don't like that behaviour, you may use the `whereStrict()` method.
<!-- end version if -->

## Using Pluck to extract certain values from a collection
You will often find yourself with a collection of data where you are only interested in parts of the data. 

In the example below we got a list of participants at an event and we want to provide a the tour guide with a simple list of names.

    // First we collect the participants
    $participants = collect([
        ['name' => 'John', 'age' => 55],
        ['name' => 'Melissa', 'age' => 18],
        ['name' => 'Bob', 'age' => 43],
        ['name' => 'Sara', 'age' => 18],
    ]);
    
    // Then we ask the collection to fetch all the names
    $namesList = $partcipants->pluck('name')
    // ['John', 'Melissa', 'Bob', 'Sara'];

You can also use `pluck` for collections of objects or nested arrays/objects with dot notation.

    $users = User::all(); // Returns Eloquent Collection of all users
    $usernames = $users->pluck('username'); // Collection contains only user names

    $users->load('profile'); // Load a relationship for all models in collection

    // Using dot notation, we can traverse nested properties
    $names = $users->pluck('profile.first_name'); // Get all first names from all user profiles

## Using macro() to extend collections
<!-- Language-All: php -->
The `macro()` function allows you to add new functionality to `Illuminate\Support\Collection` objects

Usage:

    Collection::macro("macro_name", function ($parameters) {
        // Your macro
    });
  
For example:
      
    Collection::macro('uppercase', function () {
        return $this->map(function ($item) {
            return strtoupper($item);
        });
    });

    collect(["hello", "world"])->uppercase();

Result: `["HELLO", "WORLD"]`








## Using Array Syntax
<!-- Language-all: php -->

The `Collection` object implements the `ArrayAccess` and `IteratorAggregate` interface, allowing it to be used like an array. 

**Access collection element:**

     $collection = collect([1, 2, 3]);
     $result = $collection[1];

Result: `2`

**Assign new element:**

    $collection = collect([1, 2, 3]);
    $collection[] = 4;

Result: `$collection` is `[1, 2, 3, 4]`
  
**Loop collection:**

    $collection = collect(["a" => "one", "b" => "two"]);
    $result = "";
    foreach($collection as $key => $value){
        $result .= "(".$key.": ".$value.") ";        
    }

Result: `$result` is `(a: one) (b: two)`

**Array to Collection conversion:**

To convert a collection to a native PHP array, use:

    $array = $collection->all();
    //or
    $array = $collection->toArray()

To convert an array into a collection, use:

    $collection = collect($array);

**Using Collections with Array Functions**

Please be aware that collections are normal objects which won't be converted properly when used by functions explicitly requiring arrays, like `array_map($callback)`.

Be sure to convert the collection first, or, if available, use the method provided by the `Collection` class instead: `$collection->map($callback)`


## Using Contains to check if a collection satisfies certain condition
<!-- language-all: lang-php -->

A common problem is having a collection of items that all need to meet a certain criteria. In the example below we have collected two items for a diet plan and we want to check that the diet doesn't contain any unhealthy food.
    
    // First we create a collection
    $diet = collect([
        ['name' => 'Banana', 'calories' => '89'],
        ['name' => 'Chocolate', 'calories' => '546']
    ]);
    
    // Then we check the collection for items with more than 100 calories
    $isUnhealthy = $diet->contains(function ($i, $snack) {
        return $snack["calories"] >= 100;
    });

In the above case the ```$isUnhealthy``` variable will be set to ```true``` as Chocolate meets the condition, and the diet is thus unhealthy.

## Using sum, avg, min or max on a collection for statistical calculations
Collections also provide you with an easy way to do simple statistical calculations.

    $books = [
        ['title' => 'The Pragmatic Programmer', 'price' => 20],
        ['title' => 'Continuous Delivery', 'price' => 30],
        ['title' => 'The Clean Coder', 'price' => 10],
    ]
    
    $min = collect($books)->min('price'); // 10
    $max = collect($books)->max('price'); // 30
    $avg = collect($books)->avg('price'); // 20
    $sum = collect($books)->sum('price'); // 60

## Using reduce()
<!-- Language-All: php -->
The ```reduce``` method reduces the collection to a single value, passing the result of each iteration into the subsequent iteration. Please see [reduce method][1].

The ```reduce``` method loops through each item with a collection and produces new result to the next iteration. Each result from the last iteration is passed through the first parameter (in the following examples, as ```$carry```).

This method can do a lot of processing on large data sets. For example the following examples, we will use the following example student data:

     $student = [
        ['class' => 'Math', 'score' => 60],
        ['class' => 'English', 'score' => 61],
        ['class' => 'Chemistry', 'score' => 50],
        ['class' => 'Physics', 'score' => 49],
    ];

**Sum student's total score**

    $sum = collect($student)
        ->reduce(function($carry, $item){
            return $carry + $item["score"];
        }, 0);

Result: ```220```

Explanation:

 - ```$carry``` is the result from the last iteration. 
 - The second parameter is the default value for the $carry in the first round of iteration. This case, the default value is 0

**Pass a student if all their scores are >= 50**

    $isPass = collect($student)
        ->reduce(function($carry, $item){
            return $carry && $item["score"] >= 50;
        }, true);

Result: ```false```

Explanation:

 - Default value of $carry is true
 - If all score is greater than 50, the result will return true; if any less than 50, return false.

**Fail a student if any score is < 50**

    $isFail = collect($student)
        ->reduce(function($carry, $item){
            return $carry || $item["score"] < 50;
        }, false);

Result: ```true```

Explain:

 - the default value of $carry is false
 - if any score is less than 50, return true; if all scores are greater than 50, return false.

**Return subject with the highest score**

    $highestSubject = collect($student)
        ->reduce(function($carry, $item){
            return $carry === null || $item["score"] > $carry["score"] ? $item : $carry;
        });

result: ```[ "subject" => "English", "score" => 61 ]```

Explain:

- The second parameter is not provided in this case.
- The default value of $carry is null, thus we check for that in our conditional.

  [1]: https://laravel.com/docs/5.2/collections#method-reduce

## Using Get to lookup value or return default
You often find yourself in a situation where you need to find a variables corresponding value, and collections got you covered.

In the example below we got three different locales in an array with a corresponding calling code assigned. We want to be able to provide a locale and in return get the associated calling code. The second parameter in `get` is a default parameter if the first parameter is not found.

    function lookupCallingCode($locale)
    {
        return collect([
            'de_DE' => 49,
            'en_GB' => 44,
            'en_US' => 1,
        ])->get($locale, 44);
    }

In the above example we can do the following

    lookupCallingCode('de_DE'); // Will return 49
    lookupCallingCode('sv_SE'); // Will return 44

You may even pass a callback as the default value. The result of the callback will be returned if the specified key does not exist:

        return collect([
            'de_DE' => 49,
            'en_GB' => 44,
            'en_US' => 1,
        ])->get($locale, function() {
            return 44;
        });

## Using Map to manipulate each element in a collection
Often you need to change the way a set of data is structured and manipulate certain values.

In the example below we got a collection of books with an attached discount amount. But we much rather have a list of books with a price that's already discounted.

    $books = [
        ['title' => 'The Pragmatic Programmer', 'price' => 20, 'discount' => 0.5],
        ['title' => 'Continuous Delivery', 'price' => 25, 'discount' => 0.1],
        ['title' => 'The Clean Coder', 'price' => 10, 'discount' => 0.75],
    ];
    
    $discountedItems =  collect($books)->map(function ($book) {
       return ['title' => $book["title"], 'price' => $book["price"] * $book["discount"]];
    });
    
    //[
    //    ['title' => 'The Pragmatic Programmer', 'price' => 10],
    //    ['title' => 'Continuous Delivery', 'price' => 12.5],
    //    ['title' => 'The Clean Coder', 'price' => 5],
    //]

This could also be used to change the keys, let's say we wanted to change the key `title` to `name` this would be a suitable solution.

## Sorting a collection
There are a several different ways of sorting a collection.

# Sort()
The `sort` method sorts the collection:

    $collection = collect([5, 3, 1, 2, 4]);

    $sorted = $collection->sort();
    
    echo $sorted->values()->all();
    
    returns : [1, 2, 3, 4, 5]

The `sort` method also allows for passing in a custom callback with your own algorithm. Under the hood sort uses php's [`usort`][1].

    $collection = $collection->sort(function ($a, $b) {
        if ($a == $b) {
            return 0;
        }
        return ($a < $b) ? -1 : 1;
    });

# SortBy()
The `sortBy` method sorts the collection by the given key:

    $collection = collect([
    ['name' => 'Desk', 'price' => 200],
    ['name' => 'Chair', 'price' => 100],
    ['name' => 'Bookcase', 'price' => 150],
    ]);
    
    $sorted = $collection->sortBy('price');
    
    echo $sorted->values()->all();
    
    returns:  [
            ['name' => 'Chair', 'price' => 100],
            ['name' => 'Bookcase', 'price' => 150],
            ['name' => 'Desk', 'price' => 200],
        ]

The `sortBy` method allows using dot notation format to access deeper key in order to sort a multi-dimensional array.

    $collection = collect([
        ["id"=>1,"product"=>['name' => 'Desk', 'price' => 200]],
        ["id"=>2, "product"=>['name' => 'Chair', 'price' => 100]],
        ["id"=>3, "product"=>['name' => 'Bookcase', 'price' => 150]],
        ]);
    
    $sorted = $collection->sortBy("product.price")->toArray();

    return: [
      ["id"=>2, "product"=>['name' => 'Chair', 'price' => 100]],
      ["id"=>3, "product"=>['name' => 'Bookcase', 'price' => 150]],
      ["id"=>1,"product"=>['name' => 'Desk', 'price' => 200]],
    ]

# SortByDesc()
This method has the same signature as the `sortBy` method, but will sort the collection in the opposite order.


  [1]: http://php.net/manual/en/function.usort.php#refsect1-function.usort-parameters

