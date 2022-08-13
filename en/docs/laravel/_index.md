---
title : Laravel Tutorial
slug : laravel-tutorial
weight : 9379
draft : false
images : []
type : docs
---

# Laravel StackOverflow Slack Community

_Coming soon_

# Featured Tutorial

[Getting Started With Laravel][1]


  [1]: https://www.wikiod.com/laravel/installation

# Contribution Guidelines

_Coming soon_

# Contribution Style Guide

_Coming soon_

# About Laravel

Created by [Taylor Otwell][1] as a free open-source [PHP][2] [web framework][3], [Laravel][4] is meant to ease and accelerate the development process of web applications with a great taste for simplicity.

It follows the model–view–controller ([MVC][5]) architectural pattern as well as the [PSR-2][6] coding standard, and the [PSR-4][7] autoloading standard.

Running a Test Driven Development ([TDD][8]) in Laravel is fun and easy to implement.

Hosted on [GitHub][9] and available at https://github.com/laravel/laravel, Laravel boasts of a [micro-services][10] architecture, making it tremendously extendable and this, with ease, with the use of custom-made and or existing third-party packages.

## Main Features

### MVC
Laravel uses the MVC model, therefore there are three core-parts of the framework which work together: models, views and controllers. Controllers are the main part where most of the work is done. They connect to models to get, create or update data and display the results on views, which contain the actual HTML structure of the application.

### Blade Templating Engine

Laravel is shipped with a templating engine known as Blade. Blade is quite easy to use, yet, powerful. One feature the Blade templating engine does not share with other popular ones is her permissiveness; allowing the use of plain PHP code in Blade templating engine files.

It is important to note that Blade templating engine files have `.blade` appended to file names right before the usual `.php` which is nothing other than the actual file extension. As such, `.blade.php` is the resulting file extension for Blade template files. Blade template engine files are stored in the resources/views directory.

### Routing & Middleware
You can define the URLs of your application with the help of routes. These routes can contain variable data, connect to controllers or can be wrapped into middlewares.
Middelware is a mechanism for filtering HTTP requests. They can be used to interact with requests before they reach the controllers and can thus modify or reject requests.

### Artisan
Artisan is the command line tool you can use to control parts of Laravel. There are a lot of commands available to create models, controllers and other resources needed for development. You can also write your own commands to extend the Artisan command line tool.

### Eloquent ORM
To connect your models to various types of databases, Laravel offers its own ORM with a large set of functions to work with. The framework also provides migration and seeding and also features rollbacks.

### Event Handling
The framework is capable of handling events across the application. You can create event listeners and event handlers that are similar to the ones from NodeJs.


  [1]: https://github.com/taylorotwell/ "Taylor Otwell"
  [2]: https://en.wikipedia.org/wiki/Category:PHP_frameworks "PHP framework"
  [3]: https://en.wikipedia.org/wiki/Web_framework/ "web framework &#40;WF&#41; or web application framework &#40;WAF&#41;"
  [4]: https://laravel.com/ "Laravel"
  [5]: https://en.wikipedia.org/wiki/Model%E2%80%93view%E2%80%93controller/ "Model–view–controller software architectural pattern"
  [6]: https://github.com/php-fig/fig-standards/blob/master/accepted/PSR-2-coding-style-guide.md/ "PSR-2"
  [7]: https://github.com/php-fig/fig-standards/blob/master/accepted/PSR-4-autoloader.md/ "PSR-4"
  [8]: https://en.wikipedia.org/wiki/Test-driven_development/ "Test-driven development &#40;TDD&#41;"
  [9]: https://github.com/ "GitHub"
  [10]: https://en.wikipedia.org/wiki/Microservices/ "A more concrete and modern interpretation of service-oriented architectures &#40;SOA&#41;"

