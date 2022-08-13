---
title: "Navbar"
slug: "navbar"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Basic Navbar (fixed at the top of page)
    <div class="navbar navbar-inverse navbar-fixed-top">
        <div class="container">
            <div class="navbar-header">
                <!--- vvv Hamburger icon that gets shown when window reaches a certain scale vvv --->
                <button type="button" class="navbar-toggle" data-toggle="collapse" data-target=".navbar-collapse">
                    <span class="sr-only">Toggle navigation</span>
                    <span class="icon-bar"></span>
                    <span class="icon-bar"></span>
                    <span class="icon-bar"></span>
                </button>
                <!--- ^^^ Hamburger icon that gets shown when window reaches a certain scale ^^^ --->
                <a class="navbar-brand" href="#">WebSite Title</a>
            </div>
            <div class="navbar-collapse collapse">
                <ul class="nav navbar-nav">
                    <li><a href="#">Home</a></li>
                    <li><a href="#">About</a></li>
                    <li><a asp-controller="Home" asp-action="Contact">Contact</a></li>
                </ul>
            </div>
        </div>
    </div>

## Submenu in navbar
    <div class="navbar navbar-inverse navbar-fixed-top">
        <div class="container">
            <div class="navbar-header">
                <button type="button" class="navbar-toggle" data-toggle="collapse" data-target=".navbar-collapse">
                    <span class="sr-only">Toggle navigation</span>
                    <span class="icon-bar"></span>
                    <span class="icon-bar"></span>
                    <span class="icon-bar"></span>
                </button>
                <a class="navbar-brand" href="#">WebSite Title</a>
            </div>
            <div class="navbar-collapse collapse">
                <ul class="nav navbar-nav">
                    <li><a href="#">Home</a></li>
                    <li><a href="#">About">About</a></li>
                    <li><a asp-controller="Home" asp-action="Contact">Contact</a></li>
                    <!--- vvv Create a submenu in the navbar vvv --->
                    <li class="dropdown">
                        <a href="#" class="dropdown-toggle" data-toggle="dropdown">Testing Stuff <b class="caret"></b></a>
                        <ul class="dropdown-menu">
                            <li><a href="#">SubItem</a></li>
                            <li><a href="#">Something Sub-y</a></li>
                        </ul>
                    </li>
                    <!--- ^^^ Create a submenu in the navbar ^^^ --->
                </ul>
            </div>
        </div>
    </div>

## Navbar divider
    <div class="navbar navbar-inverse navbar-fixed-top">
        <div class="container">
            <div class="navbar-header">
                <button type="button" class="navbar-toggle" data-toggle="collapse" data-target=".navbar-collapse">
                    <span class="sr-only">Toggle navigation</span>
                    <span class="icon-bar"></span>
                    <span class="icon-bar"></span>
                    <span class="icon-bar"></span>
                </button>
                <a class="navbar-brand" href="#">WebSite Title</a>
            </div>
            <div class="navbar-collapse collapse">
                <ul class="nav navbar-nav">
                    <li><a href="#">Home</a></li>
                    <!--- vvv Create a divider in the nav vvv --->
                    <li class="divider"></li>
                    <!--- ^^^ Create a divider in the nav ^^^ --->
                    <li><a href="#">About">About</a></li>
                    <li><a asp-controller="Home" asp-action="Contact">Contact</a></li>
                </ul>
            </div>
        </div>
    </div>

## Keep current navigation link "active"
    // Add active class to active navigation link
    $(document).ready(function () {
        $('ul.nav.navbar-nav').find('a[href="' + location.pathname + '"]')
            .closest('li').addClass('active');
    });

## Change Navbar breakpoint (mobile vs normal)
max-width is the breakpoint
    
    @media (max-width: 1200px) {
        .navbar-header {
            float: none;
        }
        .navbar-left,.navbar-right {
            float: none !important;
        }
        .navbar-toggle {
            display: block;
        }
        .navbar-collapse {
            border-top: 1px solid transparent;
            box-shadow: inset 0 1px 0 rgba(255,255,255,0.1);
        }
        .navbar-fixed-top {
            top: 0;
            border-width: 0 0 1px;
        }
        .navbar-collapse.collapse {
            display: none!important;
        }
        .navbar-nav {
            float: none!important;
            margin-top: 7.5px;
        }
        .navbar-nav>li {
            float: none;
        }
        .navbar-nav>li>a {
            padding-top: 10px;
            padding-bottom: 10px;
        }
        .collapse.in{
            display:block !important;
        }
       .navbar-nav .open .dropdown-menu {
           position: static;
           float: none;
           width: auto;
           margin-top: 0;
           background-color: transparent;
           border: 0;
           -webkit-box-shadow: none;
           box-shadow: none;
        }
    }

## Close collapsed navbar when clicking outside of the navbar
    jQuery('body').bind('click', function(e) {
        if(jQuery(e.target).closest('#navbar').length == 0) {
            // click happened outside of .navbar, so hide
            var opened = jQuery('.navbar-collapse').hasClass('collapse in');
            if ( opened === true ) {
                jQuery('#navbar2 .navbar-collapse').collapse('hide');
            }
        }
    });

