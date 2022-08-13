---
title: "Creating Lists"
slug: "creating-lists"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Nested lists
    1. Lists can be nested
        * Four spaces
            - Eight spaces
                + Twelve spaces
    2. And back

> 1. Lists can be nested
>     * Four spaces
>         - Eight spaces
>            + Twelve spaces
> 2. And back
 

## Numbered lists
    1. Lists
    2. Can be
    3. Numbered

> 1. Lists
> 2. Can be
> 3. Numbered

Note that the numbers themselves are ignored:

    1. This is the first item
    5. This is the fifth item
    7. This is the seventh item

> 1. This is the first item
> 5. This is the fifth item
> 7. This is the seventh item

However, the first number is used to start the numbering:

    3. This list starts at #3
    2. However, this item is #4, despite being prefixed with `2.`

> 3. This list starts at #3
> 2. However, this item is #4, despite being prefixed with `2.`

This can be used to resume a list after it's been interrupted by other text/an image/a table/etc.

    My very favorite colors are:    
    1. Blue
    5. Red
    
    (I like red because that's the best flavor of Skittle. But I digress.)
    
    3. Orange  
    9. [etc]

> My very favorite colors are:    
> 1. Blue
> 5. Red
>
> (I like red because that's the best flavor of Skittle. But I digress.)
>
> 3. Orange  
> 9. [etc]

You can also annotate a list item in this way without interrupting the numbering:

    My very favorite colors are:    
    1. Blue
    5. Red  
    (I like red because that's the best flavor of Skittle. But I digress.)
    9. Orange  
    11. [etc]

Note that there is no blank line between 5. and the parenthetical statement, and there **are** two spaces at the end of "Red", so we get:

> My very favorite colors are:    
> 1. Blue
> 5. Red  
> (I like red because that's the best flavor of Skittle. But I digress.)
> 9. Orange  
> 11. [etc]

Without the two spaces, that section would be:

> 1. Blue
> 5. Red
> (I like red because that's the best flavor of Skittle. But I digress.)

...because of how Markdown treats line breaks.

## Bulleted lists
    Characters for bulleted lists:
    * Asterisks
    + Plus signs
    - Minus signs

Characters for bulleted lists:
* Asterisks
+ Plus signs
- Minus signs


**Please note:**  
For the best results you have to use the same character because as you can see in the example below different signs make the list break


    Characters for bulleted lists:
    * Asterisks 1
    * Asterisks 2
    * Asterisks 3
    + Plus signs 1
    + Plus signs 2
    + Plus signs 3
    - Minus signs 1
    - Minus signs 2
    - Minus signs 3

Characters for bulleted lists:
* Asterisks 1
* Asterisks 2
* Asterisks 3
+ Plus signs 1
+ Plus signs 2
+ Plus signs 3
- Minus signs 1
- Minus signs 2
- Minus signs 3


