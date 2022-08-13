---
title: "Getting started with twitter-bootstrap-3"
slug: "getting-started-with-twitter-bootstrap-3"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation
- **Direct Download** - [download link](https://github.com/twbs/bootstrap/releases/download/v3.3.6/bootstrap-3.3.6-dist.zip)
- **CDN** - [get here](http://getbootstrap.com/getting-started/#download-cdn)
- **Bower** - `bower install bootstrap` [[read]](http://getbootstrap.com/getting-started/#download-bower)
- **NPM** - `npm install bootstrap` [[read]](http://getbootstrap.com/getting-started/#download-npm)
- **Composer** - `composer require twbs/bootstrap` [[read]](http://getbootstrap.com/getting-started/#download-composer)
- **Customize** - I you have your own config, you can customize [here](http://getbootstrap.com/customize/).
- **Sass** - For Sass related projects you may get it [here][1].

**Usage**

    <head>
      <link rel="stylesheet" href="path/to/bootstrap.min.css">
    </head>

Reference to the bootstrap js file is made using script tag just above body tag (see below). Also note bootstrap is using jQuery for most of it widgets - like accordion carousel  etc.. so reference bootstrap js file below the jQuery js file.

<body>
   <script src="path/to/jquery.min.js"></script>
   <script src="path/to/bootstrap.min.js"></script>
<body/>
**Sample**

    <!DOCTYPE html>
    <html lang="en">

    <head>

      <title>Form Bootstrap Example</title>
      <meta charset="utf-8">
      <meta name="viewport" content="width=device-width, initial-scale=1">
      <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
    </head>

    <body>

      <div class="container">
        <h2>Form Email</h2>
        <form role="form">
          <div class="form-group">
            <label for="email">Email:</label>
            <input type="email" class="form-control" id="email" placeholder="Enter email">
          </div>
          <div class="form-group">
            <label for="pwd">Password:</label>
            <input type="password" class="form-control" id="pwd" placeholder="Enter password">
          </div>
          <div class="checkbox">
            <label><input type="checkbox"> Remember me</label>
          </div>
          <button type="submit" class="btn btn-default">Submit</button>
        </form>
      </div>

      <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js"></script>
      <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>

    </body>

    </html>


  [1]: https://github.com/twbs/bootstrap-sass/archive/v3.3.7.tar.gz

## Hello World
The following `HTML` page illustrates a simple Hello World page using `Bootstrap 3`. 

The page contains a full-width navigation bar with example links and a drop-down control. The navigation bar takes advantage of Bootstrap's mobile first capabilities. It starts collapsed in mobile views and become horizontal as the available viewport width increases.

In addition, a `jumbotron` element has been used to display featured information.

    <!DOCTYPE html>
    <html lang="en">
    <head>
        <title>Bootstrap Hello World</title>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
    </head>

    <body>

        <nav class="navbar navbar-default">
            <div class="container-fluid">
                <div class="navbar-header">
                    <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#bs-example-navbar-collapse-1" aria-expanded="false">
                        <span class="sr-only">Toggle navigation</span>
                        <span class="icon-bar"></span>
                        <span class="icon-bar"></span>
                        <span class="icon-bar"></span>
                    </button>
                    <a class="navbar-brand" href="#">Hello, World!</a>
                </div>


                <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">

                    <ul class="nav navbar-nav navbar-right">
                        <li><a href="#">Link</a></li>
                        <li class="dropdown">
                            <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">Dropdown <span class="caret"></span></a>
                            <ul class="dropdown-menu">
                                <li><a href="#">Action</a></li>
                                <li><a href="#">Another action</a></li>
                                <li><a href="#">Something else here</a></li>
                                <li role="separator" class="divider"></li>
                                <li><a href="#">Separated link</a></li>
                            </ul>
                        </li>
                    </ul>
                </div>
            </div>
        </nav>

        <div class="container">
            <div class="jumbotron">
                <h1>Bootstrap</h1>
                <p>This is a simple hero unit, a simple jumbotron-style component for calling extra attention to featured content or information.</p>
                <p><a class="btn btn-primary btn-lg">Learn more</a></p>
            </div>
        </div>


        <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js"></script>
        <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>

    </body>
    </html>

## Bootstrap installation and getting started
**Introduction**<br>
So you want to start using bootstrap for your project? Great! then lets get started right now!.<br>

**What is bootstrap?**
Bootstrap is an open source library wich you can use to make amazing **responsive** projects with using responsive design and simple code. Responsive Design is a design philosophy where in the design of the system (the representation and the layout) responds/adapts depending upon the layout of the device. The primary reason to keep your design responsive is to increase the reach of your application to a larger user base using an array of devices.
<br>
**Installation** <br>
Bootstrap can be installed in many different ways and for many different kind of projects. In the list below i've placed some download and tutorial links on how to install bootstrap <br>
**Download links**<br>

- **Direct Download** - [download link](https://github.com/twbs/bootstrap/releases/download/v3.3.6/bootstrap-3.3.6-dist.zip)
- **CDN** - [get here](http://getbootstrap.com/getting-started/#download-cdn)
- **Bower** - `bower install bootstrap` [[read]](http://getbootstrap.com/getting-started/#download-bower)
- **NPM** - `npm install bootstrap` [[read]](http://getbootstrap.com/getting-started/#download-npm)
- **Composer** - `composer require twbs/bootstrap` [[read]](http://getbootstrap.com/getting-started/#download-composer)
- **Customize** - I you have your own config, you can customize [here](http://getbootstrap.com/customize/).
- **Sass** - For Sass related projects you may get it [here][1].

**Basic information** <br>
So you've now installed bootstrap in your project. And now it is time to start using the great advantages of bootstrap. First i'm going to tell you some basic usage of bootstrap, after that i'll show some small examples and at the end i'll give you a startup code example wich you can use as a starting template!


**The grid system**<br>
Bootstrap uses a grid system. This grid system normally consists of 12 columns. Each of these 12 columns has the same width but can have different heights.

So we have a grid system that consists of 12 columns. We can use these columns to build our basic website. lets say that we want to achieve the following layout:

**menu** - full width<br>
**sidebar** - 1/3 of the screen<br>
**Main content** 2/3 of the screen<br>
**Footer** - full width<br>

**The menu**<br>
First we are going to look at the menu. As we know the grid system works with 12 columns. Since we want the menu on full width we have to put the menu in all of the 12 columns.
It will look like the example below

    <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
        Menu
    </div>

By giving the menu a col-lg-12 class we indicate the following: <br>
**col** - The col in the classname stands for columns.<br>
**lg** - The lg in the classname stands for the width of the screen, in this case large.<br>
**12** - the 12 in the classname stands for the number of columns we want our menu to posses. since in this case we want the menu on a full width we get all the 12 columns (12/12) <br>

Since we've used 12 of the 12 columns for our menu everything after the menu will be set on a new row.

**The sidebar**<br>
The second item that we want to add to our template is the sidebar. Now as i've told we want the sidebar to be 1/3 of the screen. So what we are going to do is divide the 12 columns in 3. wich is 4. We now know how many columns we want to fill to reach 1/3 of the screen. Follow the code below.

    <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
        The menu
    </div>
    <div class="col-lg-4 col-md-4 col-sm-4 col-xs-4">
        The sidebar
    </div>

The same as with the menu only now our number of col's differ from the menu.<br>
**col** - The col in the classname stands for columns.<br>
**lg** - The lg in the classname stands for the width of the screen, in this case large.<br>
**4** - the 12 in the classname stands for the number of columns we want our sidebar to posses. since in this case we want the sidebar to fill 1/3 of the screen so we'll only grab 4 of the 12 columns (4/12) <br>

**The content**<br>
Now on this row we've still got 8 columns left next to our sidebar. So now we are going to fill those up with our content. See the example code below

    <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
        The menu
    </div>
    <div class="col-lg-4 col-md-4 col-sm-4 col-xs-4">
        The sidebar
    </div>
    <div class="col-lg-8 col-md-8 col-sm-8 col-xs-8">
        The main content
    </div>

Now since we've filled up the remaining 8 columns of our 12 columns on this row the next section will again start on a new row with 12 columns.

**The footer**<br>
The footer is, again just like the menu going to be a full width block on the screen so we'll grab all 12 columns on this row for our footer, see the example code below.

    <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
        The menu
    </div>
    <div class="col-lg-4 col-md-4 col-sm-4 col-xs-4">
        The sidebar
    </div>
    <div class="col-lg-8 col-md-8 col-sm-8 col-xs-8">
        The main content
    </div>
    <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
        The footer
    </div>i are described below.
So now we've created, with just a very small html file our first bootstrap template. But this is the very basics. Normally we would form this code a bit more to give it the full bootstrap experience. Some of these experiences are described down below.

**Using rows and containers**
As i've told in the simple example above bootstrap uses rows of 12 columns. when a row is filled with 12 columns bootstrap will start on a new row. the best way to show these rows is by using row classes. We'll also use a container class. this is like a body tag, in this container we'll put all of our code. You can either choose between a container or a container-fluid class. The only difference is that the container-fluid class uses the full width of a screen and the container class doesn't. An example of these basic features is down below.

    <div class="container-fluid">
        <div class="row">
            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
                The menu consisting of 12 columns
            </div>
        </div>
        <div class="row">
            <div class="col-lg-4 col-md-4 col-sm-4 col-xs-4">
                The sidebar consisting of 4 columns
            </div>
            <div class="col-lg-8 col-md-8 col-sm-8 col-xs-8">
                The main content consisting of 8 columns
            </div>
        </div>
        <div class="row">
            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
                The footer consisting of 12 columns
            </div>
        </div>
    </div>

So we've now made a full page template with bootstrap. It's a very simple one indeed but start at the basics and later on you'll be able to use all sorts of bootstrap classes and functionalities. Last thing. The lg - md - sm and xs column names in the classes correspond, as i've told to the width of the page. LarGe, MeDium, SMall and XSmall. Don't forget you can use that to even style to columns differently on different width's by changing the amount of columns. Do remember a full row consists of 12 columns!

For more information visit: http://getbootstrap.com/<br?
For great examples visit: http://expo.getbootstrap.com or http://bootsnipp.com/ <br>

**If you want me to add more information or if you'd come across some problems please let me know! and happy coding to you all!**

 

