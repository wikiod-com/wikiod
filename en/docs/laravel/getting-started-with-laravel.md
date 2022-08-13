---
title: "Getting started with Laravel"
slug: "getting-started-with-laravel"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Welcome to Laravel tag documentation!
[**Laravel**][1] is a well-known PHP Framework. Here, you will learn all-about Laravel. Starting from _as-simple-as_ knowing what Object-Oriented Programming is, to the advanced Laravel package development topic.

This, like every other Stackoverflow documentation tag, is community-driven documentation, so if you already have experiences on Laravel, share your knowledge by add your own topics or examples! Just don't forget to consult our **Contribution style guide** on this topic remarks to know more about how to contribute and the style guide that we made to make sure we can give the best experience towards people that want to learn more about Laravel.

More than that, we are very glad that you come, hope we can see you often here!


  [1]: https://www.wikiod.com/laravel/installation

## Starter Guide
Starter guide is custom navigation that we ordered by ourselves to make topic browsing easier especially for beginner. This navigation is ordered by level of difficulty.

## Getting Started

[Installation][1]

## Laravel Views

[Blade : Introduction][2]

[Blade : Variables and Control Structures][3]

Or 

**Installation from here**

1. Get composer from [here][4] and install it

2. Get Wamp from [here][5], install it and set environment variable of PHP

3. Get path to `www` and type command:


    composer create-project --prefer-dist laravel/laravel projectname


To install a specific Laravel version, get path to `www` and type command:

    composer create-project --prefer-dist laravel/laravel=DESIRED_VERSION projectname

Or

**Via Laravel Installer**

First, download the Laravel installer using Composer:

    composer global require "laravel/installer"

Make sure to place the `$HOME/.composer/vendor/bin` directory (or the equivalent directory for your OS) in your $PATH so the `laravel` executable can be located by your system.

Once installed, the `laravel new` command will create a fresh Laravel installation in the directory you specify. For instance, `laravel new blog` will create a directory named `blog` containing a fresh Laravel installation with all of Laravel's dependencies already installed:

    laravel new blog

  [1]: https://www.wikiod.com/laravel/installation
  [2]: https://www.wikiod.com/laravel
  [3]: https://www.wikiod.com/laravel/blade-templates
  [4]: https://getcomposer.org/
  [5]: http://www.wampserver.com/en/

