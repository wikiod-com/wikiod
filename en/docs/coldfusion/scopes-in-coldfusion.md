---
title: "Scopes in Coldfusion"
slug: "scopes-in-coldfusion"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

A **scope** is "the range in which a variable can be referenced". ColdFusion knows — as well as most other programming and script languages — several scopes. The following text deals with these types and trys to bring clarity about them, their differences and their characteristics.

## Overview
* Components and functions
    * variables
    * this
    * local
    * arguments

* Custom tags
    * attributes
    * thisTag
    * caller

* Global Scopes
    * Server
    * Application
    * Session

* Request Scopes
    * request
    * variables
    * form
    * url
    * cgi

## Request Scopes
**request** 

**variables**

**form**

**url**

**cgi**

## Global Scopes
**Server**

**Application**

**Session**


## Components and functions
**variables**

**this**

**local**

**arguments**

## Custom tags
**attributes**

**thisTag**

**caller**

## Common scopes
Mostly you're probably working with these scopes:

 * **Variables scope**&emsp;is the scope where all variables are assigned to when nothing else is intentionally declared (like the `window` scope in JavaScript).
 * **Form scope**&emsp;When you send a form to your server, all the form fields which can be identified (by setting the name/id property) are accessible in this scope for further server-side processing.
 * **URL scope**&emsp;All url query params are stored in that scope
 * **this scope**&emsp;Inside a component the `this` refers to the component itself
 * **local scope**&emsp;Variables declared inside a function using the `local` statement are encapsulated and only accessible inside that specific function (this is made to avoid pollution of other sopes)
 * **Arguments scope**&emsp;Arguments passed to a function inside a component declared by the `cfargument` tag are accessible with that scope

