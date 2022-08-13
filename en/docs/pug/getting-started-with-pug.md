---
title: "Getting started with pug"
slug: "getting-started-with-pug"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World Example
First, let's create a template to be rendered!

    p Hello World, #{name}!

Save this in a file ending with the `.pug` extension (you can call it anything you like, but we will use `view.pug` in the following code to compile it).

All that's left to do, now, is compile that template! Create a JS script file (we'll call ours `main.js`), and add the following content:

    // Import the pug module
    const pug = require('pug');

    // Compile the template (with the data not yet inserted)
    const templateCompiler = pug.compileFile('view.pug');

    // Insert your data into the template file
    console.log(templateCompiler({ name: 'John' });

When you run this file with `npm main.js`, you should get the following HTML code output in your console:

    <p>Hello World, John!</p>

Congratulations, you just created and compiled your first template! On to more advanced stuff, such as [Conditionals](https://pugjs.org/language/conditionals.html), [Iteration](https://pugjs.org/language/iteration.html), and much more!

## Installation
To install the Pug template rendering system, follow these steps:

1. Have the [Node.js environment](https://nodejs.org/en/) installed on your machine
2. Run `npm install pug --save` to install the `pug` module to your current project.

You can now use `pug` in your project through the standard `require` mechanism:

    const pug = require("pug");
    
If you are using Express in your application, you do not need to `require("pug")`. However, you must set the `view engine` property of your Express application to `pug`.

    app.set("view engine", "pug");

Further, you must set the view directory of your app so that Express knows where to look for your Pug files (for compilation).

    app.set("views", "path/to/views");

Within your Express route, you can then render your Pug files by calling the `res.render` function with the path of the file (starting from the directory set by the `app.set("views")` option).


    app.get("/", function (req, res, next) {
        // Your route code
        var locals = {
            title: "Home",
        };
        res.render("index", locals);
    });

In the above, `index` points to a file located at `views/index.pug`, and `locals` represents an object of variables that are exposed to your file. As will be explained in later sections, Pug can access variables passed to it and perform a variety of actions (conditionals, interpolation, iteration, and more).



