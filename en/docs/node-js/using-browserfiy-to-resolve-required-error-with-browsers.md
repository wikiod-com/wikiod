---
title: "Using Browserfiy to resolve 'required' error with browsers"
slug: "using-browserfiy-to-resolve-required-error-with-browsers"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Example - file.js
In this example we have a file called **file.js**.

Let's assume that you have to parse an URL using JavaScript and NodeJS querystring module. 



To accomplish this all you have to do is to insert the following statement in your file:

    const querystring = require('querystring'); 
    var ref = querystring.parse("foo=bar&abc=xyz&abc=123");

What is this snippet doing?
==
Well, first, we create a querystring module which provides utilities for parsing and formatting URL query strings. It can be accessed using:

    const querystring = require('querystring'); 

Then, we parse a URL using the .parse() method. It parses a URL query string (str) into a collection of key and value pairs.

For example, the query string `'foo=bar&abc=xyz&abc=123'` is parsed into:

    {  foo: 'bar',  abc: ['xyz', '123']   }

Unfortunately, Browsers don't have the *require* method defined, but Node.js does.

Install Browserfy
==
With Browserify you can write code that uses *require* in the same way that you would use it in Node.
So, how do you solve this? It's simple.

 1. First install node, which ships with npm. Then do:   

> npm install -g **browserify**

 2. Change into the directory in which your file.js is and Install our *querystring* module with npm:

> npm install **querystring**

**Note:** If you don't change in the specific directory the command will fail because it can't find the file which contains the module.
 3. Now recursively bundle up all the required modules starting at file.js into a single file called bundle.js (or whatever you like to name it) with the **browserify command**:

> **browserify** file.js -o bundle.js

Browserify parses the Abstract Syntax Tree for *require()* calls to traverse the entire dependency graph of your 

 4. FinallyDrop a single <script> tag into your html and you're done!

> `<script src="bundle.js"></script>`

What happens is that you get a combination of your old .js file (**file.js** that is) and your newly created **bundle.js** file. Those two files are merged into one single file.

> Important
> ===
>Please keep in mind that if you want to make any changes to your file.js and will not affect the behaviour of your program. **Your changes will only take effect if you edit the newly created bundle.js**

What does that mean?
==
This means that if you want to edit **file.js** for any reasons, the changes will not have any effects. You really have to edit **bundle.js** since it is a merge of **bundle.js** and **file.js**. 

