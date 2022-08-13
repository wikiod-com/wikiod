---
title: "Getting started with materializecss"
slug: "getting-started-with-materializecss"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Setup
Once you've downloaded the files, extract your them into your designated directory.

You'll notice that there are two sets of the files. The `min` means that the file is "compressed" to reduce load times. These minified files are usually used in production while it is better to use the unminified files during development.

----------


After extracting, your directory's file structure should look like:

      MyWebsite/
      |--css/
      |  |--materialize.css
      |
      |--fonts/
      |  |--roboto/
      |
      |--js/
      |  |--materialize.js
      |
      |--index.html


----------

Example MaterializeCSS HTML page:

    <!DOCTYPE html>
    <html>
     <head>
      <!--Import Google Icon Font-->
      <link href="http://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">
      <!--Import materialize.css-->
      <link type="text/css" rel="stylesheet" href="css/materialize.min.css"  media="screen,projection"/>

      <!--Let browser know website is optimized for mobile-->
      <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
     </head>

     <body>
      <!--Import jQuery before materialize.js-->
      <script type="text/javascript" src="https://code.jquery.com/jquery-2.1.1.min.js"></script>
      <script type="text/javascript" src="js/materialize.min.js"></script>
     </body>
    </html>

## Download
There are two ways to use MaterializeCSS, either you can download the files on your system or use the files from CDN (Content Delivery Network).

**Download files**
 - Download the [Materialize Package][1]. 
 - Download the [Materialize SASS Package][2].
 - Install via NPM: `npm install materialize-css`
 - Install via Bower: `bower install materialize`

**Include From CDN**
 - Include minifed CSS in the **head** section:

       <!-- Compiled and minified CSS -->
       <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.7/css/materialize.min.css">
    

 - Include  Javascript file in the **Body** section, just before the closing body tag.

       <!-- Compiled and minified JavaScript -->
       <script src="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.7/js/materialize.min.js"></script>

  [1]: http://materializecss.com/bin/materialize-v0.97.7.zip
  [2]: http://materializecss.com/bin/materialize-src-v0.97.7.zip

## Templates
These are the simplest starter pages with a Header, Call-to-Action, and Icon Features.

| Starter Template | Parallax Template |
| ------ | ------ |
| [![Starter Template][5]][5] | [![Parallax Template][6]][6] |
| [Demo][1] or [Download][2] | [Demo][3] or [Download][4] |


  [1]: http://materializecss.com/templates/starter-template/preview.html
  [2]: http://materializecss.com/templates/starter-template.zip
  [3]: http://materializecss.com/templates/parallax-template/preview.html
  [4]: http://materializecss.com/templates/parallax-template.zip
  [5]: http://i.stack.imgur.com/JnOsX.gif
  [6]: http://i.stack.imgur.com/UcrKc.jpg

## Web Page Using Materializecss
Here is an example of a basic page using the Materialize CSS framework which incorporates the grid system and `materialboxed`.

    <!DOCTYPE html>
    <html>
    <head>
    <title>Materializecss Example webpage</title>
      <!--Import Google Icon Font-->
      <link href="http://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">
      <!--Import materialize.css-->
      <link type="text/css" rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.7/css/materialize.min.css"  media="screen,projection"/>

      <!--Let browser know website is optimized for mobile-->
      <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
    </head>

    <body>
      <div class="container"> 
        <div class="row">
          <div class="col s12"><h3>MATERIALIZECSS EXAMPLE</h3><h4>A simple website just to show how an element is used.</h4></div>
          <div class="col s12 m6 l3">
            <img class="materialboxed" width="100%" height="220"  src="https://upload.wikimedia.org/wikipedia/commons/7/76/Katun_nature_reserve.jpg">
            <h4>Katun Nature Reserve</h4>
            <p><i>Lovely Place</i></p>
          </div>
          <div class="col s12 m6 l3">
            <img class="materialboxed" width="100%" height="220" src="https://upload.wikimedia.org/wikipedia/commons/4/4b/Computer_art_4.png">
            <h4>Art</h4>
            <p><i>Simple & Attractive</i></p>
          </div>
          <div class="col s12 m6 l3">
            <img class="materialboxed" width="100%" height="220" src="https://c1.staticflickr.com/9/8715/16947318656_4c6cbc3091_b.jpg">
            <h4>Food</h4>
            <p><i>For any occasion</i></p>
           </div>
          <div class="col s12 m6 l3">
            <img class="materialboxed" width="100%" height="220" src="https://c1.staticflickr.com/7/6179/6217102314_350be5e843.jpg">
            <h4>Steve Jobs</h4>
            <p><i>Inspiration</i></p>
          </div>
        <div class="col s12"><h5>Comment your suggestion</h5></div>
        </div>
      </div>
      <!--Import jQuery before materialize.js-->
      <script type="text/javascript" src="https://code.jquery.com/jquery-2.1.1.min.js"></script>
      <script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.7/js/materialize.min.js"></script>
    <script>
    $(document).ready(function(){
      $('.materialboxed').materialbox();
    });
    </script>
    </body>
  </html>

## How can I compile my Sass
This section is only relevant if you work with the Sass version of Materialize.

First, you need to install Sass in your working directory:

 

    gem install sass

When you have Sass installed on your project and you want to update your output .css file, you need to use the following command:

 

    sass sass/materialize.scss public/style.css

**NOTE**: the second parameter sass/materialize.scss is the path to your .scss file, and the last parameter sass/style.css is the path to your output folder when the file .css file is located.

If you want to avoid this command every time when you do a change, you can run a watch command:

     sass --watch sass/sass:public/stylesheets

**NOTE**: this command watches all Sass files in the scss directory for changes and then update the style file into our public directory.



## Using MaterializeCSS with Angular
There are many ways to use MaterializeCSS framework. 

Few things to keep in mind before going to installation 

- It is not a CSS only framwork, though it has CSS  name in it. We can use its SCSS too
- It is not built for Angular
- It is a component framework too built on jquery. Though we are not supposed to use jquery ( not suggested ) in angular, still we import .

You can use **_any_** of the following methods: 
  

 - CDN
 - Assets
 - Include in Angular (NPM)
 - Include in Angular (source with SCSS)

Each has its own advantages and disadvantages. 

CDN 
--
  Just add this to `index.html` and you are good to go . 

     <!-- Compiled and minified CSS -->
      <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.98.2/css/materialize.min.css">
     
     <!-- We need jquery first -->  
      <script src="https://code.jquery.com/jquery-3.2.1.min.js"></script>

     <!-- Compiled and minified JavaScript -->
       <script src="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.98.2/js/materialize.min.js"></script>

    
Assets
---
Add it as an asset in your project. This helps in not depending on internet when building and running locally. 

[Download jQuery][1]

[Download CSS version][2]

 - Extract them
 - Copy `materialize.min.css`, `jquery-3.2.1.min.js` and `materialize.min.js` in your assets folder 
 - add them to index.html

        <link rel="stylesheet" href="./assets/materialize.min.css" >
        <script src="./assets/jquery-3.2.1.min.js"></script>
        <script src="./assets/materialize.min.js"></script>

Include in angular ( NPM)
---
In this method we directly include the files to our angular build. I am assuming the angular project is built with `@angular/cli` for simplicity. 

Do
 
    npm install materialize-css --save 
    npm install jquery --save

This is same as downloading CSS files, but we dont need to add them in our repository. 

Add the following to `.angular-cli.json`:
    
    
    "styles": [
        "../node_modules/materialize-css/dist/css/materialize.css",
        "styles.scss"
    ]
    
    ...

    "scripts":[
      "../node_modules/jquery/dist/jquery.js",
      "../node_modules/materialize-css/dist/js/materialize.js"
    ]


Include in angular (SCSS)
--

[Download source with SCSS version][3]

Do this if you want to take advantage of `SCSS` to change default behavior of the library. Otherwise you can still use previous method and use SCSS along side. 

Add them to a folder outside `src` . May be create a folder `materialize-src` as sibling to `src` and paste the contents there. We are doing this because npm install of materialize-css doesn't give us scss version. (_Correct me if I am wrong_ )

Install jquery

    npm install jquery --save 

Add them to your `.angular-cli.json`

    "styles": [
        "../materialize-src/sass/materialize.scss",
        "styles.scss"
     ],
     "scripts": [
        "../node_modules/jquery/dist/jquery.min.js",
        "../materialize-src/js/bin/materialize.min.js"
     ],


-------
Integration with Angular: 
---
The above all installation methods provide full functionality with materialize and **no need to further install anything to work in angular**. 
Take any example and just use the appropriate HTML structure inside the component part of angular and you are good to go. 

In some instances you might need to tinker with javascript and for that we need to use jQuery. Instead of that we can use the angular wrapper developer at [angular2-materialize][4]. I developed a full functional site using angular and materialize and never felt a need for that. 

If you still believe you need it . You can install as follows : 

 - Install materialize with any of the above mentioned ways 
 - Install angular2-materialize 

        npm install angular2-materilize --save 

    Add in angular `app.module.ts`

        import { MaterializeModule } from "angular2-materialize";
    
        @NgModule({
          imports: [
            //...
            MaterializeModule,
          ],
          //...
        })



  [1]: https://code.jquery.com/jquery-3.2.1.min.js
  [2]: http://materializecss.com/bin/materialize-v0.98.2.zip
  [3]: http://materializecss.com/bin/materialize-src-v0.98.2.zip
  [4]: https://github.com/InfomediaLtd/angular2-materialize/

