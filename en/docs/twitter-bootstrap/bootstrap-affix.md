---
title: "Bootstrap Affix"
slug: "bootstrap-affix"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## On Navbar
Html:

    <nav class="navbar navbar-defalt" data-offset-top="120" data-spy="affix" >
        ...
    </nav>

Css:

    <style>
    .navbar {
        background-color: red;
    }
    .navbar.affix {
        background-color: green;
    }

    </style>

## Affix Example 2
    <div class="container" id="con">
        <div class="row">
            <div class="span12">
                <div class="well">
                    <h1> Header </h1>               
                </div>
            </div>
        </div>
    </div>
    <div class="container" data-spy="affix" data-offset-top="400" id="nav">
        <div class="navbar">
            <div class="navbar-inner">
                <div class="container">
                    <div class="span12">
                        <a class="brand" href="#">Home</a>
                         <ul class="nav">
                            <li class="active"><a href="#">Home</a></li>
                            <li><a href="#">Link</a></li>
                            <li><a href="#">Link</a></li>
                         </ul>
                    </div>
                </div> 
            </div> 
        </div> 
    </div>
    <div class="container">
        <div class="span3">
            <p style="height:1000px;padding:10px;">
              
                Long scrolling text here... Ovi lispmd idr. Blah goo bar foor foo.  Ovi lispmd idr. Blah goo bar foor foo. Ovi lispmd idr. Blah goo bar foor foo. Ovi lispmd idr. Blah goo bar foor foo. Ovi lispmd idr. Blah goo bar foor foo. Ovi lispmd idr. Blah goo bar foor foo. Ovi lispmd idr. Blah goo bar foor foo. Ovi lispmd idr. Blah goo bar foor foo. Ovi lispmd idr. Blah goo bar foor foo. Ovi lispmd idr. Blah goo bar foor foo.
                
            </p>
        </div>
    </div>
    
    <style>

    #con .well {
        height:400px;
    }
    
    #nav.affix {
        position: fixed;
        top: 0;
        width: 100%
    }
    </style>

    <script>
        $('#nav').affix();
    </script




