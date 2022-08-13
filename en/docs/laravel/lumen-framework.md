---
title: "lumen framework"
slug: "lumen-framework"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Getting started with Lumen
*The following example demonstrates using `Lumen` in `WAMP / MAMP / LAMP` environments.*

To work with [`Lumen`][4] you need to setup couple of things first.
* [Composer][1]
* [PHPUnit][2]
* [git][3] (not required but strongly recommended)

Assuming you have all these three components installed (at least you need composer), first go to your web servers document root using terminal. MacOSX and Linux comes with a great terminal. You can use `git bash` (which is actually `mingw32` or `mingw64`) in windows.

    $ cd path/to/your/document/root

Then you need to use compose to install and create `Lumen` project. Run the following command.

    $ composer create-project laravel/lumen=~5.2.0 --prefer-dist lumen-project
    $ cd lumen-project

`lumen-app` in the code above is the folder name. You can change it as you like. Now you need to setup your virtual host to point to the `path/to/document/root/lumen-project/public` folder. Say you mapped `http://lumen-project.local` to this folder. Now if you go to this url you should see a message like following (depending on your installed `Lumen` version, in my case it was 5.4.4)-

    Lumen (5.4.4) (Laravel Components 5.4.*)


If you open `lumen-project/routers/web.php` file there you should see the following-

    $app->get('/', function () use($app) {
        return $app->version();
    });

Congratulations! Now you have a working `Lumen` installation. No you can extend this app to listen to your custom endpoints.



  [1]: https://getcomposer.org/
  [2]: https://phpunit.de/
  [3]: https://git-scm.com/
  [4]: https://lumen.laravel.com

