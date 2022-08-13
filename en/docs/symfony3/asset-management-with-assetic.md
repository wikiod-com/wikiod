---
title: "Asset Management with Assetic"
slug: "asset-management-with-assetic"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

When using the Assetic Bundle, according to the Symfony documentation, please be aware of the following:

Starting from Symfony 2.8, Assetic is no longer included by default in the Symfony Standard Edition. Before using any of its features, install the AsseticBundle executing this console command in your project:

$ composer require symfony/assetic-bundle

There are other steps you have to take. For more information go to:
http://symfony.com/doc/current/assetic/asset_management.html

## Parameters
| Name| Example | 
| ------ | ------ |
| Path | 'static/images/logo/logo-default.png'|

The folder for the publicly accessible assets in a standard Symfony3 project is "/web".
Assetic uses this folder as root folder for the assets.

## Create relative path for asset
    <img src="{{ asset('static/images/logo-default.png') }}" alt="Logo"/>
    
    <!--Generates path for the file "/web/static/images/logo-default.png" -->

## Create absolute path for asset
    <img src="{{ app.request.getSchemeAndHttpHost() ~ asset('static/images/logo-default.png') }}" alt="Logo"/>
    
    <!--Generates path for the file "/web/static/images/logo-default.png" -->

