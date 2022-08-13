---
title: "Events"
slug: "events"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Page, DOM and Browser loading
This is an example to explain the variations of load events.

   1. **onload event**
        
    <body onload="someFunction()">
    <img src="image1" />
    <img src="image2" />
    </body>

    <script>
        function someFunction() {
        console.log("Hi! I am loaded");
    }
    </script>
In this case, the message is logged once *all the contents of the page including the images and stylesheets(if any)* are completely loaded.

2. **DOMContentLoaded event**
    
       document.addEventListener("DOMContentLoaded", function(event) {
           console.log("Hello! I am loaded");
       });
In the above code, the message is logged only after the DOM/document is loaded (*ie:once the DOM is constructed*).
3. **Self-invoking anonymous function**

       (function(){
           console.log("Hi I am an anonymous function! I am loaded");
       })();
Here, the message gets logged as soon as the browser interprets the anonymous function. It means, this function can get executed even before the DOM is loaded.

