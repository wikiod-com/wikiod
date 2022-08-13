---
title: "Arrays"
slug: "arrays"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Mapping values
You want to convert all elements in an array to some other form.

For example, you have

    theUsers = [
      {id: 1, username: 'john'}
      {id: 2, username: 'lexy'}
      {id: 3, username: 'pete'}
    ]

and you want to have an array of usernames only, i.e.

    ['john', 'lexy', 'pete']

### Method 1 - using `.map`

    theUsernames = theUsers.map (user) -> user.username

### Method 2 - using comprehension

    theUsernames = (user.username for user in theUsers)

## Filtering values
    theUsers = [
      {id: 1, username: 'john'}
      {id: 2, username: 'lexy'}
      {id: 3, username: 'pete'}
    ]

To retain only users whose id is greather than 2, use the following:

    [{id: 3, username: 'pete'}]

### Method 1 - using `.filter`

    filteredUsers = theUsers.filter (user) -> user.id >= 2

### Method 2 - using comprehension

    filteredUsers = (user for user in theUsers when user.id >= 2)

## Concatenation
You want to combine arrays into one.

For example, you have

    fruits = ['Broccoli', 'Carrots']
    spices = ['Thyme', 'Cinnamon']

and you want to combine them into

    ingredients = ['Broccoli', 'Carrots', 'Thyme', 'Cinnamon']

### Method 1 - using `.concat`

    ingredients = fruits.concat spices

### Method 2 - using splats

    ingredients = [fruits..., spices...] 

### Method 3 - using `.concat` with indeterminate number of arrays

If the number of arrays can vary, e.g. you have array of arrays:

    arrayOfArrays = [[1], [2,3], [4]]
    [].concat.apply([], arrayOfArrays) # [1, 2, 3, 4]

## Slicing
If you want to extract a subset of an array (i.e. `numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9]`) you can easily do this with one of the following examples:

- `numbers[0..2]` will return `[1, 2, 3]`
- `numbers[3...-2]` will return `[3, 4, 5, 6]`
- `numbers[-2..]` will return `[8, 9]`
- `numbers[..]` will return `[1, 2, 3, 4, 5, 6, 7, 8, 9]`

With two dots (3..6), the range is inclusive `[3, 4, 5, 6]`  
With three dots (3...6), the range excludes the end `[3, 4, 5]`  
Adding a `-` to the range will start the count at the end of the array  
An omitted first index defaults to zero  
An omitted second index defaults to the size of the array

The same syntax can be used with assignment to replace a segment of an array with new values

    numbers[3..6] = [-3, -4, -5, -6]

The above row will replace the numbers array with the following : `[1, 2, -3, -4, -5, -6, 7, 8, 9]`

## Comprehensions
You can do neat things via the results of Array "comprehensions"...

Like assign multiple variables... from the result of a looping `for` statement...

    [express,_] = (require x for x in ['express','underscore'])

Or a syntactically sweet version of a "mapped" function call, etc...

    console.log (x.nme for x in [{nme:'Chad',rnk:99}, {nme:'Raul', rnk:9}])

> `[ 'Chad', 'Raul' ]`

Notice the `( )` surrounding these statements.  These parenthesis are required to make the enclosed comprehension "work".

