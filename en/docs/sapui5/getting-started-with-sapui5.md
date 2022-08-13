---
title: "Getting started with sapui5"
slug: "getting-started-with-sapui5"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World!
We start with creating an HTML page for the app. There we define the meta tags, a script tag to load the SAPUI5 libraries, and a placeholder for the content of the app.

<!-- language: lang-html -->

    <!DOCTYPE html>
    <html>
    <head>
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta charset="utf-8">
        <title>Hello World App</title>
        <script src="http://<<server>>:<<port>>/resources/sap-ui-core.js"
            id="sap-ui-bootstrap"
            data-sap-ui-theme="sap_bluecrystal"
            data-sap-ui-libs="sap.m">
        </script>
    </head>
    <body class="sapUiBody" id="content">
    </body>
    </html>

> Adapt the path where the resources are located (<<server>>:<<port>>)
> according to your installation. For OpenUI5 you can use
> src="https://openui5.hana.ondemand.com/resources/sap-ui-core.js". For
> accessing SAPUI5 on the SAP HANA Cloud Platform, for example, use
> src="https://sapui5.hana.ondemand.com/resources/sap-ui-core.js".

## Hello World 
    <!DOCTYPE html>
    <html>
    <head>
        <meta http-equiv="X-UA-Compatible" content="IE=edge" />
        <meta http-equiv="Content-Type" content="text/html;charset=UTF-8"/>
        <title>SAPUI5 Hello World</title>
        <!-- Load SAPUI5 , theme and control library -->
        <script id="sap-ui-bootstrap"
            src="https://sapui5.hana.ondemand.com/resources/sap-ui-core.js"
            data-sap-ui-theme="sap_bluecrystal"
            data-sap-ui-libs="sap.m"></script>
    
        <!-- Create a UI5 button and place it onto the page -->
        <script>
                new sap.m.Button({
                    text:"Hello world",
                    press: function(){
                        alert("hello SapUI5!");
                    }
                }).placeAt("content");
         </script>
    </head>
    <body class="sapUiBody" id="content">
    </body>
    </html>

## Hello World
    <!DOCTYPE HTML>
    <html>
    <head>
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta http-equiv='Content-Type' content='text/html;charset=UTF-8'/>
        <script type="text/javascript" charset="utf-8" src="cordova.js"></script>
        <script src="resources/sap-ui-core.js"
                id="sap-ui-bootstrap"
                data-sap-ui-libs="sap.m"
                data-sap-ui-theme="sap_bluecrystal"
                data-sap-ui-xx-bindingSyntax="complex"
                data-sap-ui-compatVersion="1.24"
                data-sap-ui-resourceroots='{"<projectname>": "./"}'>
        </script>
        <!-- only load the mobile lib "sap.m" and the "sap_bluecrystal" theme -->

        <script>
        sap.ui.getCore().attachInit( function () {
            new sap.ui.core.ComponentContainer ("<ComponentId(can be anyname you wish)>",{
                height : "100%",
                name : "<name of component>"
            }).placeAt('content');
        });
        </script>

    </head>
    <body class="sapUiBody" role="application">
        <div id="content"></div>
    </body>
</html>

**Place the bootstrapping code in attachInit because it will be triggered after core library loaded**

