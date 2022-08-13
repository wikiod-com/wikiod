---
title: "Getting started with extjs6"
slug: "getting-started-with-extjs6"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## ExtJS 6 Application Setup
Getting started with Ext JS 6.

Prerequisite steps:

 1. Download [Sencha Cmd 6][1] and install it with `sencha` command set in
    environment path variables.
 2. Download trail version of [Sencha SDK][2] (ext-6.2.1.zip) and unzip it.(on location D:\ext-6.2.1)

System is ready to create Extjs 6 application.

**Create sample application (World) with Extjs 6 standard folder structure**

There are three type of Application:

 1. Universal (Runs on Mobile web`Sencha touch` and Desktop web`Sencha Extjs`)
    
 2. Classic (Runs on Desktop web`Sencha Extjs`)
    
 3. Modern (Runs on Mobile web`Sencha touch`)

Open CMD and run bellow command to generate `World` application:

> **Universal Application:** `sencha -sdk  D:\ext-6.2.1 generate app World D:\World` 
> 
> **classic Application:** `sencha -sdk  D:\ext-6.2.1 generate app --classic World D:\World`
> 
> **Modern Application:** `sencha -sdk  D:\ext-6.2.1 generate app --modern World D:\World`

This respective command will generate application's folder structure in respective location.

Go to `cd D:/World` and run command:

> sencha app watch

Application will be loaded on:

> http://localhost:1841/

`World` Application can be access from browser.

*Application's path can be vary as per requirements. 

In order to build extjs application for production code use,
> sencha app build

This will create extjs compressed, obfuscated code ready for production use in, [APPLICATION_HOME]/build/production. Build directory also contains other folders for development and testing code.


 

  [1]: https://www.sencha.com/products/extjs/cmd-download/
  [2]: https://www.sencha.com/products/extjs/evaluate/

## Extjs 6 Application setup
Getting started with Ext JS 6.

Prerequisite steps:

 1. Download [Sencha Cmd 6][1] and install it with `sencha` command set in
    environment path variables.
 2. Download trail version of [Sencha SDK][2] (ext-6.2.1.zip) and unzip it.(on location D:\ext-6.2.1)

System is ready to create Extjs 6 application.

**Create sample application (World) with Extjs 6 standard folder structure**

There are three type of Application:

 1. Universal (Runs on Mobile web`Sencha touch` and Desktop web`Sencha Extjs`)
    
 2. Classic (Runs on Desktop web`Sencha Extjs`)
    
 3. Modern (Runs on Mobile web`Sencha touch`)

Open CMD and run bellow command to generate `World` application:

> **Universal Application:** `sencha -sdk  D:\ext-6.2.1 generate app World D:\World` 
> 
> **classic Application:** `sencha -sdk  D:\ext-6.2.1 generate app --classic World D:\World`
> 
> **Modern Application:** `sencha -sdk  D:\ext-6.2.1 generate app --modern World D:\World`

This respective command will generate application's folder structure in respective location.

Go to `cd D:/World` and run command:

> sencha app watch

Application will be loaded on:

> http://localhost:1841/

`World` Application can be access from browser.

*Application's path can be vary as per requirements. 


 

  [1]: https://www.sencha.com/products/extjs/cmd-download/
  [2]: https://www.sencha.com/products/extjs/evaluate/

