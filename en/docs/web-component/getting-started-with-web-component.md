---
title: "Getting started with Web Component"
slug: "getting-started-with-web-component"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Availability
**Native implementations**

The `<template>` element is implemented in every modern browsers: 
- Chrome, 
- Edge, 
- Firefox, 
- Opera,
- Safari,
- [...][1]

Custom Elements `customElements.define()`, Shadow DOM `attachShadow()` and HTML Imports `<link rel="import">` are implemented in the latest versions of Chrome and Opera.

**Polyfills**

For other browsers, you can use a polyfill library: 

 - for Custom Elements: from [WebReflection][2] or [Webcomponents.org][3],
 - for Shadow DOM: from [Webcomponents.org][5],
 - for Template : from [Neovov][4],
 - for HTML Imports: from [Webcomponents.org][6]


  [1]: http://caniuse.com/#search=web%20components
  [2]: https://github.com/WebReflection/document-register-element
  [3]: https://github.com/webcomponents/custom-elements
  [4]: https://github.com/neovov/template-element-polyfill
  [5]: https://github.com/webcomponents/shadydom
  [6]: https://github.com/webcomponents/html-imports

## HTML Template - Hello World
Use a `<template>` element to design a HTML template that you can then reuse in your code.

    <template id="Template1">
        Hello, World !
    <template>
    
    <div id="Target1"></div>
    
    <script>
       Target1.appendChild( Template1.content.cloneNode( true ) )
    </script>

This will insert the content of the template in the `#Target1` div.

## Custom Element - Hello World
Create a new HTML tag named `<hello-world>` that will display "Hello, World!":

    <script>
    //define a class extending HTMLElement
    class HelloWorld extends HTMLElement {
        connectedCallback () {
          this.innerHTML = 'Hello, World!'
        }
    }
    
    //register the new custom element
    customElements.define( 'hello-world', HelloWorld )
    </script>
    
    <!-- make use the custom element -->
    <hello-world></hello-world>



## Shadow DOM - Hello World
Add a Shadow DOM to a `div` that will display "Hello, World!" instead of its initial content.

    <div id="Div1">intial content</div>
    
    <script>
       var shadow = Div1.attachShadow( { mode: 'open' } )
       shadow.innerHTML = "Hello, World!" 
    </script>



## HTML Import - Hello World
Import an HTML file that will add a div with "Hello, World!" at the end of the main document's DOM tree.

Imported file *hello.html*:

    <script>
       var div = document.createElement( 'div' )
       div.innerHTML = 'Hello, World!'
       document.body.appendChild( div )
    </script>

Main file *index.html*:

    <html>
      <link rel="import" href="hello.html">






## Hello World example
This example combines Custom Element, Template, Shadow DOM and HTML Import to display a the "Hello, World!" string in HTML.

In file `hello-world.html`:

    <!-- 1. Define the template -->
    <template>
       Hello, World!
    </template>
    
    <script>
      var template = document.currentScript.ownerDocument.querySelector( 'template' )   
    
      //2. Define the custom element
      
      customElements.define( 'hello-world', class extends HTMLElement 
      {
          constructor() 
          {
              //3. Create a Shadow DOM
              var sh = this.attachShadow( { mode: 'open' } )
              sh.appendChild( document.importNode( template.content, true ) )
         }
      } )
    </script>

In main file `index.html`:

    <html>
    <head>
        <!-- 4. Import the HTML component --> 
        <link rel="import" href="hello-world.html">
    </head>
    <body>
        <hello-world></hello-world>      
    </body>
    </html>
  

