---
title: "Using external Javascript Libraries with Polymer"
slug: "using-external-javascript-libraries-with-polymer"
draft: false
images: []
weight: 9726
type: docs
toc: true
---

Since all Web Components must be self-contained, including all their dependencies, duplicated dependency import would quickly become an issue with script includes. Thus, Web Components (and, by extension, Polymer) use W3C HTML imports for managing component dependencies. These imported HTML files can be cached by the browser and will only be loaded once.

Most external libraries are not yet prepared for HTML imports. Fortunately creating the necessary HTML wrapper is quick and straight forward.

## Parameters
| Parameter | Description |
| ------ | ------ |
| this.resolveUrl('../libraries/turf.js')   | Resolves the location of your library   |
| function() {this.doSomething(argument, anotherArgument);}.bind(this)); | Callback. This example uses a closure to recurse this.doSomething() |


In this example, the library used in the component is installed with the package manager bower.  This allows for easy distribution of a library-dependent component.  If the library you wish to use is not distributed via a package manager, it can still be loaded the same way but your component will be require more effort to be used by others.

The lazy loading example uses a simple string for the library path. If one wished to avoid magic string constants, paths could be loaded using iron-ajax from a JSON file into an object and passed between components if needed.

## Import a static HTML file

1. Create an HTML file (in this example `libraries/turf.html`) with the library you want to load:

    ```
    <script src="../../bower_components/turf/turf.min.js"></script>
    ```


2. Import the HTML file (`libraries/turf.html`) in your component with the rest of your imports:

    ```
    <link rel="import" href="../../bower_components/polymer/polymer.html">
    <link rel="import" href="../libraries/turf.html">
    ```
3. Invoke your library (example usage): 
    ```
        var point = turf.point([42.123123, -23.83839]);
    ```




## Lazy Loading
1. Create an HTML file (in this example `libraries/turf.html`) with the library you want to load:  

    ```
    <script src="../../bower_components/turf/turf.min.js"></script>
    ```
2. Import and use your library when needed:
    ```
    doSomething: function(argument, anotherArgument): {

        // If library has not been loaded, load it
        if(typeof turf == 'undefined') {
            this.importHref(this.resolveUrl('../libraries/turf.js'), function() {
                // Once the library is loaded, recursively call the function
                this.doSomething(argument, anotherArgument);
            }.bind(this));

            return;
        }
        
        // Example usage of a library method
        var point = turf.point([42.123123, -23.83839]);
    }

