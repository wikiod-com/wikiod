---
title: "Twitter Bootstrap Style Customization"
slug: "twitter-bootstrap-style-customization"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

One thing to note is that one has to mention custom.css name after the main bootstrap.css , otherwise the values of custom.css won't get actually implemented.

## Overriding Default CSS
Everybody loves [twitter bootstrap][1], but some of us don't like it's default design. So here's a simple guide on how to start customizing boostrap's design. Twitter boostrap when cloned provides a set of default css files which we can override.

The mail css file that we need to override is the `boostrap.min.css` under the `boostrap/dist/css` directory.

To override boostrap's default design follow this 2 easy steps.

 1. Creat a `custom.css` (or you can name it whatever you want) and link it to your `index.html`

    <!DOCTYPE html>
        <html>
        <head>
            <title>Customize Bootstrap</title>
        
            <link rel="stylesheet" type="text/css" href="path/to/bootstrap.min.css">
            <!-- This mus be declared after the bootstrap.min.css -->
            <link rel="stylesheet" type="text/css" href="path/to/your/custom.css">
        </head>
        <body>
            <!-- Do something -->
        </body>
        </html>
 2. Start customizing. For example we want to change the color of the default button. If you want to use bootstrap's default button style you need to add the `btn` class on you `<button class="btn">Sample</button>` tag. Just write the following code on your `custom.css`.

        .btn{
            background-color:red;
        }
    
    The code above will produce something like this.

       **Default :**

       [![enter image description here][2]][2]

       **Custom :**

       [![enter image description here][3]][3]


This technique will save us from rewriting the whole button styles that were already written by boostrap contributors. This also saved us from writing our own css class which for me is less tedious.    


  [1]: http://getbootstrap.com/getting-started/
  [2]: http://i.stack.imgur.com/kGiYg.png
  [3]: http://i.stack.imgur.com/0dud5.png

