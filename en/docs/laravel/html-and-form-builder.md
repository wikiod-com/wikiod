---
title: "HTML and Form Builder"
slug: "html-and-form-builder"
draft: false
images: []
weight: 9939
type: docs
toc: true
---

## Installation
HTML and Form Builder is not a core component since Laravel 5, so we need to install it separately:

    composer require laravelcollective/html "~5.0"

Finally in `config/app.php` we need to register the service provider, and the facades aliases like this:

    'providers' => [
        // ...
        Collective\Html\HtmlServiceProvider::class,        
        // ...
    ],

    'aliases' => [
       // ...
      'Form' => Collective\Html\FormFacade::class,
      'Html' => Collective\Html\HtmlFacade::class,
       // ...
    ],

Full docs are available on [Forms & HTML][1]


  [1]: https://laravelcollective.com

