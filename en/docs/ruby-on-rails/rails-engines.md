---
title: "Rails -Engines"
slug: "rails--engines"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

Engines can be considered miniature applications that provide functionality to their host applications. A Rails application is actually just a "supercharged" engine, with the Rails::Application class inheriting a lot of its behavior from Rails::Engine.

Engines are the reusable rails applications/plugins. It works like a Gem. Famous engines are Device, Spree gems which can be integrated with rails applications easily.


## Syntax
-  `rails plugin new [engine name] --mountable`



## Parameters

| Parameters | Purpose |
| ------ | ------ |
| **--mountable**   | option tells the generator that you want to create a "mountable" and namespace-isolated engine   |
| **--full**   | option tells the generator that you want to create an engine, including a skeleton structure   |

Engines are very good options for creating reusable plugin for rails application

## Famous examples are
Generating simple blog engine

    rails plugin new [engine name] --mountable

Famous engines examples are

[Device][1] (authentication gem for rails)


[Spree][2] (Ecommerce)


  [1]: https://github.com/plataformatec/devise
  [2]: https://github.com/spree/spree

