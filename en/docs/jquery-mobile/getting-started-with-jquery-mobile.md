---
title: "Getting started with jquery-mobile"
slug: "getting-started-with-jquery-mobile"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Setup
    

The current version of JQuery Mobile is 1.4.5 and can be downloaded here:
https://jquerymobile.com/

- Download the zip file and extract all files. 
- Create a library (or lib) folder inside of your application (inside the www folder).
- Create a jquery-mobile folder inside the lib folder (I've named mine jqm).
- Place all of the jquery-mobile files that you have extracted inside of the jquery-mobile folder. You can now link to these files in your index.html file.

In the code below the javascript and css files are included by URL. You can alternatively follow the bullet points above and include it by the path it's located at on your local machine, i.e.:

    <link href="lib/jqm/jquery.mobile-1.4.5.min.css" rel="stylesheet" />
    ...
    <script type="text/javascript" src="lib/jqm/jquery.mobile-1.4.5.min.js"></script>

These are the minified versions of the css and js files. You can alternatively include the non minified version during development so that it's easier to read if you need to dig into it. If you do so I would recommend switching them back to the minified versions upon deployment to production.

<!-- jquery mobile is used to enhance html documents and -->
    <!-- it is imported inside head tags. -->

    <!-- no additional files needed, if css and js referred with full path. -->

     <!DOCTYPE html>
     <html>
     <head>
     <!-- Viewport setup. -->
     <meta name="viewport" content="width=device-width, initial-scale=1">
     <!-- jQuery Mobile styles come first before js. -->
     <link rel="stylesheet" href="http://code.jquery.com/mobile/1.4.5/jquery.mobile-1.4.5.min.css">
     <!-- Basic jQuery library -->
     <script src="http://code.jquery.com/jquery-1.11.3.min.js"></script>
     <!-- jQuery Mobile library kind of extends upon jQuery -->
     <script src="http://code.jquery.com/mobile/1.4.5/jquery.mobile-1.4.5.min.js"></script>
     </head>
     <body>

     <!-- example page element using jQuery mobile tags: -->      
     <div data-role="page" id="pageone">

         <div data-role="header" id="pageone-header">
            <h3>Insert Page header Here</h3>
         </div><!-- /header -->

         <div role="main" class="ui-content" id="pageone-content">
            <center><h3>Page title</h3></center>
            <!-- All HTML for the page content goes here -->
         </div><!-- /content -->
            
        <div data-role="footer" data-position="fixed">
            <h4>Insert Page footer Here</h4>
        </div><!-- /footer -->

     </div>
      
     </body>
     </html>
</body>
</html>


