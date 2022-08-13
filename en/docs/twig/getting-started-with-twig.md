---
title: "Getting started with twig"
slug: "getting-started-with-twig"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Basic API Usage
It can also be installed by downloading the source code and placing it in a directory of your project. However there are many benefits to using composer.

    require '/path/to/lib/Twig/Autoloader.php';
    Twig_Autoloader::register();
    
    $loader = new Twig_Loader_Filesystem('/path/to/templates');

    $options = array(
        'strict_variables' => false,
        'debug' => false,
        'cache'=> false
    );

    $twig = new Twig_Environment($loader, $options);

When creating a new `Twig_Environment` instance, you can pass an array of options as the constructor's second argument. Here is a list of the available options:

 - debug (*boolean*, default `false`)

>    When set to true, the generated templates have a `__toString()` method that you can use to display the generated nodes.

- charset (*string*, default ``utf-8``)

> The charset used by the templates.

 - base_template_class (*string*, default ``Twig_Template``)

> The base template class to use for generated templates.

    

 - cache (*string or `false`*, default `false`)

> An absolute path where to store the compiled templates, or false to
> disable caching (which is the default).

 - auto_reload (*boolean*, default inherited from *debug*)
   
> When developing with Twig, it's useful to recompile the template
> whenever the source code changes. If you don't provide a value for the
> auto_reload option, it will be determined automatically based on the
> debug value.

    

 - strict_variables (*boolean*, default `false`)

> If set to false, Twig will silently ignore invalid variables (variables and or attributes/methods that do not exist) and replace
> them with a null value. When set to true, Twig throws an exception
> instead.

 - autoescape (*string or boolean*, default`true`)

> If set to true, HTML auto-escaping will be enabled by default for all templates.
> 
> As of Twig 1.8, you can set the escaping strategy to use (html, js, false to disable).
> 
> As of Twig 1.9, you can set the escaping strategy to use (css, url, html_attr, or a PHP callback that takes the template "filename"
> and must return the escaping strategy to use -- the callback cannot be
> a function name to avoid collision with built-in escaping strategies).
> 
> As of Twig 1.17, the filename escaping strategy determines the escaping strategy to use for a template based on the template filename
> extension (this strategy does not incur any overhead at runtime as
> auto-escaping is done at compilation time.)

    

 - optimizations (*integer*, default `-1`)

> A flag that indicates which optimizations to apply:<br >
> `set to -1 to enabled all optimalizations`<br >
> `set o 0 to disable all optimalitazations`<br >



[Official Twig Installation Guide][1]

A Twig PHP extension (written in C) can also be compiled and installed, and the PHP package will automatically take advantage of that for optimizing some common routines.


  [1]: http://twig.sensiolabs.org/doc/installation.html

## What is Twig?
Twig is a templating language that compiles to optimized PHP code. It is primarily used for outputting HTML, but can also be used to output any other text-based format. It is a standalone component that can be easily integrated into any PHP project.

It provides many excellent features:

 - Autoescaping for HTML (helps to prevent XSS)
 - Syntax designed with templating in mind (based on Django templates)
 - Template inheritance
 - Macros

[Official Twig Templating Documentation][1]

Example of Twig's syntax:

    {% extends "base.html" %}

    {% block sidebar %}
      {{ parent() }}
      <span>Sidebar content specific to this page</span>
    {% endblock sidebar %}

    {% block body %}
      <p>Select an item:</p>
      <ul>
        {% for item in list %}
          <li><a href="/items/{{ item.id }}">{{ item.name }}</a>
        {% else %}
          <li>No items yet.
        {% endfor %}
      </ul>
    {% endblock body %}

  [1]: http://twig.sensiolabs.org/doc/templates.html

## Introduction
If you have any exposure to other text-based template languages, such as [Smarty][1], [Django][2], or [Jinja][3], you should feel right at home with [Twig][4]. It's both ***designer and developer friendly*** by sticking to PHP's principles and adding functionality useful for templating environments. 

**The key-features are...**

 - Fast: [Twig][4] compiles templates down to plain optimized PHP code.
   The overhead compared to regular PHP code was reduced to the very
   minimum.

  

 - Secure: [Twig][4] has a **sandbox mode** to evaluate untrusted template
   code. This allows Twig to be used as a template language for
   applications where users may modify the template design.

 

 - Flexible: [Twig][4] is powered by a flexible **lexer** and **parser**. This
   allows the developer to define their own **custom tags** and **filters**, and
   to create their own DSL.

Twig is used by many Open-Source projects like [Symfony][5], [Drupal][6], [eZPublish][7] and many frameworks have support for it as well like [Slim][8], [Yii][9], [Laravel][10], [Codeigniter][11],[silex][12] and [Kohana][13] — just to name a few.

**Installation**

The recommended way to install Twig is via [Composer][14]:

For **php 5.x** users

    composer require "twig/twig:~1.0"

For **php 7.x** users

    composer require "twig/twig:~2.0"


  [1]: http://www.smarty.net/
  [2]: https://www.djangoproject.com/
  [3]: http://jinja.pocoo.org/
  [4]: http://twig.sensiolabs.org
  [5]: https://symfony.com/
  [6]: https://www.drupal.org
  [7]: https://ez.no/
  [8]: https://www.slimframework.com
  [9]: http://www.yiiframework.com
  [10]: https://laravel.com/
  [11]: http://codeigniter.com/
  [12]: http://silex.sensiolabs.org
  [13]: https://kohanaframework.org/
  [14]: https://getcomposer.org/

