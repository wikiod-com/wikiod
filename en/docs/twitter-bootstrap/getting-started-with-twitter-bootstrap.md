---
title: "Getting started with twitter-bootstrap"
slug: "getting-started-with-twitter-bootstrap"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation/Setup
Downloading:
 - Download Bootstrap [directly][1] or clone, etc. from the [GitHub][2] repository
 - Download your customized version of Bootstrap from official [docs][3]
 - Install with bower: `bower install bootstrap`
 - Install with npm: `npm install bootstrap`
 - Install with composer: `composer require twbs/bootstrap`

**The File Structure**
[![File structure][4]][4]


Installing:

 Within your HTML page, include Bootstrap's CSS, JS, and the dependency of jQuery (pre version 3, at least as of the latest Bootstrap version). Please note that if you plan to utilize Bootstrap's JavaScript features, your jQuery reference must come *before* your bootstrap.js reference within your HTML.

You can utilize your installed Bootstrap files from the above section, or reference a CDN provided by the makers of Bootstrap (links taken from [Getting Started with Bootstrap][5]):

    <!-- Latest compiled and minified CSS -->
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u" crossorigin="anonymous">
    
    <!-- Optional theme -->
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css" integrity="sha384-rHyoN1iRsVXV4nD0JutlnGaslCJuC7uwjduW9SVrLvRYooPp2bWYgmgJQIXwl/Sp" crossorigin="anonymous">
    
    <!-- Latest compiled and minified JavaScript -->
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js" integrity="sha384-Tc5IQib027qvyjSMfHjOMaLkfuWVxZxUPnCJA7l2mCWNIpG9mGCD8wGNIcPD7Txa" crossorigin="anonymous"></script>

A very basic Bootstrap webpage:

    <!DOCTYPE html>
    <html lang="en">
      <head>
        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->
        <title>Bootstrap 101 Template</title>
    
        <!-- Bootstrap -->
        <link href="css/bootstrap.min.css" rel="stylesheet">
    
        <!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->
        <!-- WARNING: Respond.js doesn't work if you view the page via file:// -->
        <!--[if lt IE 9]>
          <script src="https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js"></script>
          <script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
        <![endif]-->
      </head>
      <body>
        <h1>Hello, world!</h1>
    
        <!-- jQuery (necessary for Bootstrap's JavaScript plugins) -->
        <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
        <!-- Include all compiled plugins (below), or include individual files as needed -->
        <script src="js/bootstrap.min.js"></script>
      </body>
    </html>


  [1]: https://github.com/twbs/bootstrap/releases/download/v3.3.7/bootstrap-3.3.7-dist.zip
  [2]: https://github.com/twbs/bootstrap
  [3]: http://getbootstrap.com/customize/
  [4]: https://i.stack.imgur.com/3hnWS.png
  [5]: http://getbootstrap.com/getting-started/

## Basic webpage using bootstrap components
    <!DOCTYPE html>
    <html lang="en">
      <head>
        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->
        <title>Bootstrap 101 Template</title>
    
        <!-- Bootstrap -->
        <link href="css/bootstrap.min.css" rel="stylesheet">
    
        <!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->
        <!-- WARNING: Respond.js doesn't work if you view the page via file:// -->
        <!--[if lt IE 9]>
          <script src="https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js"></script>
          <script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
        <![endif]-->
      </head>
      <body>
         <!-- Fixed navbar -->
            <nav class="navbar navbar-default navbar-fixed-top">
              <div class="container">
                <div class="navbar-header">
                  <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
                    <span class="sr-only">Toggle navigation</span>
                    <span class="icon-bar"></span>
                    <span class="icon-bar"></span>
                    <span class="icon-bar"></span>
                  </button>
                  <a class="navbar-brand" href="#">Project name</a>
                </div>
                <div id="navbar" class="navbar-collapse collapse">
                  <ul class="nav navbar-nav">
                    <li class="active"><a href="#">Home</a></li>
                    <li><a href="#about">About</a></li>
                    <li><a href="#contact">Contact</a></li>
                    <li class="dropdown">
                      <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">Dropdown <span class="caret"></span></a>
                      <ul class="dropdown-menu">
                        <li><a href="#">Action</a></li>
                        <li><a href="#">Another action</a></li>
                        <li><a href="#">Something else here</a></li>
                        <li role="separator" class="divider"></li>
                        <li class="dropdown-header">Nav header</li>
                        <li><a href="#">Separated link</a></li>
                        <li><a href="#">One more separated link</a></li>
                      </ul>
                    </li>
                  </ul>
                  <ul class="nav navbar-nav navbar-right">
                    <li><a href="../navbar/">Default</a></li>
                    <li><a href="../navbar-static-top/">Static top</a></li>
                    <li class="active"><a href="./">Fixed top <span class="sr-only">(current)</span></a></li>
                  </ul>
                </div><!--/.nav-collapse -->
              </div>
            </nav>
        
            <div class="container">

              <div class="jumbotron">
                <h1>Navbar example</h1>
                <p>This example is a quick exercise to illustrate how the default, static and fixed to top navbar work. It includes the responsive CSS and HTML, so it also adapts to your viewport and device.</p>
                <p>To see the difference between static and fixed top navbars, just scroll.</p>
                <p>
                  <a class="btn btn-lg btn-primary" href="../../components/#navbar" role="button">View navbar docs &raquo;</a>
                </p>
              </div>
        
            </div> <!-- /container -->

    
        <!-- jQuery (necessary for Bootstrap's JavaScript plugins) -->
        <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
        <!-- Include all compiled plugins (below), or include individual files as needed -->
        <script src="js/bootstrap.min.js"></script>
      </body>
    </html>

## Basic Template
    <!DOCTYPE html>
    <html lang="en">
      
        <head>
            
            <meta charset="utf-8">
            <meta http-equiv="X-UA-Compatible" content="IE=edge">
            <meta name="viewport" content="width=device-width, initial-scale=1">
            <!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->
        
            
            <title>Bootstrap 101 Template</title>  <!-- The title of the Website -->
    

            <!-- Reference to Bootstrap's CSS file -->
            <!-- This is the line to reference the bootstrap's Stylesheet -->
            <link href="css/bootstrap.min.css" rel="stylesheet"> 
    

            <!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->
            <!-- WARNING: Respond.js doesn't work if you view the page via file:// -->
            <!-- [if lt IE 9] -->
             <script src="https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js"></script>
             <script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
            <!--[endif]-->
      

        </head>
      
        <body>
        
            <h1>Hello, world!</h1>
    
            <!-- Referencing jQuery (necessary for Bootstrap JavaScript plugins(bootstrap.min.js) to work) -->
            <script src = "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
        

            <!-- Referencing Javascript Bootstrap Plugin to Facilitate Bootstrap Animations and functionalities. -->
            <!-- (Necessary to run Bootstrap) -->
            <script src="js/bootstrap.min.js"></script>
      

        </body>
    </html>

## When to use Bootstrap
Bootstrap is an opinionated framework for HTML, CSS and Javascript. It contains basic styling and functionality for what have become accepted [User Interface] elements, such as form elements, buttons, modal windows and navigation elements.

Bootstrap is a responsive web framework, meaning it is designed to adapt layout and design for screen sizes large and small, such as mobile devices, tablets and desktop computers, all in a single code base.

One of the fundamental concepts of Bootstrap is the grid framework. By applying classes to HTML elements, it is possible to create intricate layouts using a basic grid of twelve columns. For example, a four column layout might adapt to two columns on tablet devices and one column on mobile devices. The grid uses `media queries`, a CSS method for targeting specific screen sizes, to achieve this.

Bootstrap performs particularly well if:

* Custom design is not a top priority
* You are more comfortable editing HTML and adding classes than you are creating custom CSS
* You are comfortable using a framework that will have many visual similarities to many other websites

Bootstrap can be used by those who are new to HTML, CSS and Javascript, since the [documentation](http://getbootstrap.com/getting-started/) is excellent. However, there is a learning curve for those not entirely comfortable with the three basic technologies used by Bootstrap (HTML, CSS and Javascript).

It is possible to purchase or download Bootstrap themes in order to alter the style or functionality of Bootstrap. It is also possible to use Bootstrap as a starting point, with customization of CSS and Javascript.

