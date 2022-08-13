---
title: "Configuring Dojo with dojoConfig"
slug: "configuring-dojo-with-dojoconfig"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

The **dojoConfig** object (formerly **djConfig**) allows you to set options and default behavior for various aspects of the dojo toolkit. Examples will explain what's possible and how you can put dojoConfig to use in your code.

Note that dojoConfig is defined in a script block before dojo.js is loaded. This is of paramount importance—if reversed, the configuration properties will be ignored.

## Loader Configuration
Dojo received a new loader in Dojo 1.7 to accommodate for the toolkit's new AMD module format. This new loader added a few new configuration options that are crucial to defining packages, maps, and more. For details on the loader, see the Advanced AMD Usage tutorial. Important loader configuration parameters include:

>    baseUrl: The base URL prepended to a module identifier when
> converting it to a path or URL.

   

    baseUrl: "/js"

> packages: An array of objects which provide the package name and
> location:

    packages: [{
        name: "myapp",
        location: "/js/myapp"
    }]

> map: Allows you to map paths in module identifiers to different paths:

    map: {
        dijit16: {
            dojo: "dojo16"
        }
    }

> paths: a map of module id fragments to file paths:

    var dojoConfig = {
        packages: [
            "package1",
            "package2"
        ],
        paths: {
            package1: "../lib/package1",
            package2: "/js/package2"
        }
    };

        // ...is equivalent to:
    var dojoConfig = {
        packages: [
            { name: "package1", location: "../lib/package1" },
            { name: "package2", location: "/js/package2" }
        ]
    };

> async: Defines if Dojo core should be loaded asynchronously. Values
> can be true, false or legacyAsync, which puts the loader permanently
> in legacy cross-domain mode.

    async: true

> parseOnLoad: If true, parses the page with dojo/parser when the DOM
> and all initial dependencies (including those in the dojoConfig.deps
> array) have loaded.

    parseOnLoad: true

    It is recommended that parseOnLoad be left at false (it defaults to false, so you can simply omit this property), and that developers explicitly require dojo/parser and call parser.parse().

> deps: An array of resource paths which should load immediately once
> Dojo has loaded:

    deps: ["dojo/parser"]

> callback: The callback to execute once deps have been retrieved:

    callback: function(parser) {
        // Use the resources provided here
    }

> waitSeconds: Amount of time to wait before signaling load timeout for
> a module; defaults to 0 (wait forever):

    waitSeconds: 5

> cacheBust: If true, appends the time as a querystring to each module
> URL to avoid module caching:

    cacheBust: true
Now let's create a simple demo that puts the basic parameters to use. One very common scenario is using Dojo Toolkit from CDN with local modules. 

Let's say we use Google CDN with modules in the `/documentation/tutorials/1.10/dojo_config/demo` space:

    <!-- Configure Dojo first -->
    <script>
        dojoConfig = {
            has: {
                "dojo-firebug": true,
                "dojo-debug-messages": true
            },
            // Don't attempt to parse the page for widgets
            parseOnLoad: false,
            packages: [
                // Any references to a "demo" resource should load modules locally, *not* from CDN
                {
                    name: "demo",
                    location: "/documentation/tutorials/1.10/dojo_config/demo"
                }
            ],
            // Timeout after 10 seconds
            waitSeconds: 10,
            map: {
                // Instead of having to type "dojo/domReady!", we just want "ready!" instead
                "*": {
                    ready: "dojo/domReady"
                }
            },
            // Get "fresh" resources
            cacheBust: true
        };
    </script>
    
    <!-- Load Dojo, Dijit, and DojoX resources from Google CDN -->
    <script src="//ajax.googleapis.com/ajax/libs/dojo/1.10.4/dojo/dojo.js"></script>
    
    <!-- Load a "demo" module -->
    
    <script>
        require(["demo/AuthoredDialog", "dojo/parser", "ready!"], function(AuthoredDialog, parser) {
            // Parse the page
            parser.parse();
    
            // Do something with demo/AuthoredDialog...
        });
    </script>

By using the packages configuration, we've made all references to `demo/*` point to our local `/documentation/tutorials/1.10/dojo_config/demo/` directory, while allowing any references to `dojo`, `dijit`, and `dojox` to come from Google CDN. Had the demo package not been defined, the request for `demo/AuthoredDialog` would have gone to `//ajax.googleapis.com/ajax/libs/dojo/1.10.4/dojo/demo/AuthoredDialog.js`. We also used alias, by associating ready with `dojo/domReady`.


## Load DojoConfig
In Below sample we are creating one global javascript object `dojoConfig`  which will contain all the configuration values.

**Note:** dojoConfig is defined in a script block before dojo.js is loaded. This is of paramount importance—if reversed, the configuration properties will be ignored.
   
 <!-- set Dojo configuration, load Dojo -->
    <script>
        dojoConfig= {
            has: {
                "dojo-firebug": true
            },
            parseOnLoad: false,
            foo: "bar",
            async: true
        };
    </script>
    <script src="//ajax.googleapis.com/ajax/libs/dojo/1.10.4/dojo/dojo.js"></script>
    
    <script>
    // Require the registry, parser, Dialog, and wait for domReady
    require(["dijit/registry", "dojo/parser", "dojo/json", "dojo/_base/config", "dijit/Dialog", "dojo/domReady!"]
    , function(registry, parser, JSON, config) {
        // Explicitly parse the page
        parser.parse();
        // Find the dialog
        var dialog = registry.byId("dialog");
        // Set the content equal to what dojo.config is
        dialog.set("content", "<pre>" + JSON.stringify(config, null, "\t") + "```");
        // Show the dialog
        dialog.show();
    });
    </script>
    
    <!-- and later in the page -->
    <div id="dialog" data-dojo-type="dijit/Dialog" data-dojo-props="title: 'dojoConfig / dojo/_base/config'"></div>

