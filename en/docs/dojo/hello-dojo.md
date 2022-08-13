---
title: "Hello Dojo"
slug: "hello-dojo"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

A simple Hello Dojo like Hello World in other programming languages. Dojo is simple to install/configure and use. You can download the latest version of Dojo from http://dojotoolkit.org/download/ or you can use what is called CDN's in your project. 
I prefer to download and use it. The latest version at the time of this topic is 1.12.1 and work on Dojo 2.0 is in progress.



## Hello World
Create an HTML file like below. In the script tag, either you can use CDN link or from your local like

    <script type="text/javascript" src="dojo/dojo/dojo.js"
            data-dojo-config="async:true">

    <!DOCTYPE html>
    <html>
    <head>
        <meta charset="utf-8">
        <title>Tutorial: Hello Dojo!</title>
    </head>
    <body>
        <h1 id="greeting">Hello</h1>
        <!-- load Dojo -->
        <script src="//ajax.googleapis.com/ajax/libs/dojo/1.10.4/dojo/dojo.js"
                data-dojo-config="async: true"></script>
    </body>
    </html>

That's it. When you run this HTML, you should see Hello. Yes, we are yet to use any Dojo specific code in this example. But this shows how you can create a simple page using Dojo. Next, we will see how we can apply Dojo to this simple page.


## Dojo AMD
In the next example, let's use Dojo features and understand what AMD ( Asynchronous Module Definition) means.

    <!DOCTYPE html>
    <html>
    <head>
        <meta charset="utf-8">
        <title>Tutorial: Hello Dojo!</title>
    </head>
    <body>
        <h1 id="greeting">Hello</h1>
        <!-- load Dojo -->
        <script src="//ajax.googleapis.com/ajax/libs/dojo/1.10.4/dojo/dojo.js"
                data-dojo-config="async: true"></script>
    
        <script>
            require([
                'dojo/dom',
                'dojo/dom-construct'
            ], function (dom, domConstruct) {
                var greetingNode = dom.byId('greeting');
                domConstruct.place('<em> Dojo!</em>', greetingNode);
            });
        </script>
    </body>
    </html>
The difference from previous example is that, there is an extra script tag and within that, there are few dojo featues being used. Let's see each one of them

**require**
In layman term, require is similar to import or using statements in other languages where you import some OOTB libraries (in dojo you call them modules). Existing modules are loaded using the keyword 'require' and new modules are created using the keyword 'define'. We will learn more about modules in the later section.
For this example, we have used two OOTB modules 'dojo/dom' and 'dojo/dom-construct'.
dojo/dom (dom) is the core DOM (document object model) which can be used to get a node from html. For javascript developers, it is similar to document.getElementById('') in-fact, internally dojo uses the same method. dojo/dom-construct(domConstruct) is used to create nodes like div, li, ul etc. It's DOM construction API and it can also be used to insert a node into DOM at any position. Let's say, you have a div 'abc' and want to create another div 'xyz' and place it after 'abc'. You can accomplish that like

    domConstruct.create("div", { id:"xyz",innerHTML: "<p>new DIV</p>" });
    domConstruct.place(dojo.byId("xyz"), dojo.byId("abc"), "after");
Coming back to our example, we have

    require([
                'dojo/dom',
                'dojo/dom-construct'
            ], function (dom, domConstruct) {
                var greetingNode = dom.byId('greeting');
            domConstruct.place('<em> Dojo!</em>', greetingNode);
            });

 within function, you see dom and domConstruct. This is how we reffer to dojo/dom and dojo/dom-construct. You can use whatever the naming convention you want like

    require([
                    'dojo/dom',
                    'dojo/dom-construct'
                ], function (hi, bye) {
                    var greetingNode = hi.byId('greeting');
                    bye.place('<em> Dojo!</em>', greetingNode);
                });
But it's a good practice to you have meaningful names like for dojo/dom use dom and for dojo/dom-construct, user domConstruct.

Now within the function, we have

    var greetingNode = hi.byId('greeting');
What this does is that it searches the a dom (div in this case) with id='greeting'. variable greetingNode, will have the actual dom node. Then we have,

    domConstruct.place('<em> Dojo!</em>', greetingNode);
So here, we are appending Dojo! to the node greetingNode. This is like 
Hello+Dojo! and the output will be Hello Dojo!

So with this, we learned 

 1. How to use dojo features
 2. How to use OOTB modules
 3. How to manipulate dom





