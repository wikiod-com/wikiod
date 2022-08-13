---
title: "Constants"
slug: "constants"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Example
First you have to create a file constants.php and it is a good practice to create this file inside app/config/ folder. 
You can also add constants.php file in compose.json file.

Example File:

> app/config/constants.php

Array based constants inside the file:

    return [
        'CONSTANT' => 'This is my first constant.'
    ];

And you can get this constant by including the facade `Config` :

    use Illuminate\Support\Facades\Config;

Then get the value by constant name `CONSTANT` like below :

    echo Config::get('constants.CONSTANT');

And the result would be the value : 

>This is my first constant.



