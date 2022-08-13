---
title: "LeftAndMain"
slug: "leftandmain"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

`LeftAndMain` is more of a lower-level API and not often required due to the existence of `ModelAdmin`. However if you wanted to create a custom user interface that did not necessarily require the functionality of `ModelAdmin` in the administration panel for your module, than `LeftAndMain` is where you would want to start.

## 1. Getting Started
This guide is intended to get you started on creating your own User Interface by subclassing the `LeftAndMain` class.

By the end of this guide, you will have created your first `Hello World` interface in the Administration Panel.

# Requirements

This guide requires you to have at least version `3.*` of the [framework][1] AND [CMS][2] but less than version `4.*`.

If you wish to use this guide, then you will need to swap out any class references with the Fully Quality Class Name (FQCN) as defined in the SS4 upgrade guide.

# Preparation

> **tl;dr** Ignore the following steps and simply create the structure below them

1. Create a folder with a name of anything you choose in the root directory for your SilverStripe project, for this example we'll be using `/helloworld/` and create an empty file within that folder named `_config.php`. A `_config.php` at the very minimum is required in every module directory for SilverStripe to detect its existence.
2. Within your new folder, create a sub folder named exactly `/code/` and within that folder, for organisation purposes; create another folder called `/admin/`
3. Create `/helloworld/code/admin/HelloWorldLeftAndMain.php` and place the following code into it for now.

        class HelloWorldLeftAndMain extends LeftAndMain {

        }

4. Create the template file that will be used with this class called `/helloworld/templates/Includes/HelloWorldLeftAndMain.ss` 

# Structure

    /framework/
    /cms/
    /helloworld/
        + _config.php
        + /code/
            + /admin/
                + /HelloWorldLeftAndMain.php
        + /templates/
            + /Includes/
                + /HelloWorldLeftAndMain_Content.ss


  [1]: https://github.com/silverstripe/silverstripe-framework
  [2]: https://github.com/silverstripe/silverstripe-cms

## 2. Configuring HelloWorldLeftAndMain.php
If you haven't already lets simply start this file of with:

    class HelloWorldLeftAndMain extends LeftAndMain {

    }

# Configure

The first thing you should do, is define the `$url_segment` that will be used to access the interface, and the title (`$menu_title`) that will appear in the side navigation menu of the administration panel:

    private static $url_segment = 'helloworld';
    private static $menu_title  = 'Hello World';

_The following configuration variable(s) are optional and not used in this guide:_

    private static $menu_icon   = 'helloworld/path/to/my/icon.png';
    private static $url_rule    = '/$Action/$ID/$OtherID';

# Adding Stylesheets and Javascript

`LeftAndMain` allows you to override the `init` method in it's parent, we can use this to require specific files for our interface. Undoubtedly you should always need to require a CSS stylesheet that will style the elements for your user interface. 

As a tip, it's recommended to never rely on the CSS classes provided by the CMS as these are subject to change without notice and will subsequently destroy the Look & Feel of your UI (for example, `3.*` to `4.*` has seen a complete makeover of the interface therefore any CSS classes you relied on in `3.*` need to be restyled for conversion to `4.*`)

So lets add our `helloworld/css/styles.css` file:

    public function init() {
        parent::init();

        Requirements::css('helloworld/css/styles.css');
        //Requirements::javascript('helloworld/javascript/script.min.js');
    }

We don't need any Javascript functionality for this example but in the above I have included how one would achieve adding Javascript a file using the [Requirements][1] class.

After which you can adopt what you have been used to when dealing `Page_Controller` such as `$allowed_actions` etc with one notable difference, however

> You **CANNOT** override `index()`. 

Instead `index()` is assumed as `HelloWorldLeftAndMain_Content.ss` and from there, it's required to deal with the indexes display via template functions (see example below)

# Complete Code

    class HelloWorldLeftAndMain extends LeftAndMain {
        private static $url_segment = 'helloworld';
        private static $menu_title  = 'Hello World';
        private static $allowed_actions = array(
            'some_action'
        );

        public function init() {
            parent::init();
    
            Requirements::css('helloworld/css/styles.css');
            //Requirements::javascript('helloworld/javascript/script.min.js');
        }

        public function Hello($who=null) {
            if (!$who) {
                $who = 'World';
            }

            return "Hello " . htmlentities($who);
        } 
    }

  [1]: https://api.silverstripe.org/3.5/class-Requirements.html

## 3. The Template (HelloWorldLeftAndMain_Content.ss)
The expected structure of this template can be a bit convoluted but it all boils down to this:

1. There are 3 sections worth noting for this guide:
    -
    - `.north`
    - `.center`
    - `.south`

2. It must be wrapped entirely within an element that has the `data-pjax-fragment="Content"` attribute. This is so the AJAX calls generated from the sidemenu, know where the "Content" is so that it may display it appropriately:

```
<div class="cms-content center $BaseCSSClasses" data-layout-type="border" data-pjax-fragment="Content">

</div>
``` 

I won't go into detail about template functionality, I have included comments where relevant but you shouldn't be reading this guide if you don't understand template syntax for SilverStripe

# Complete Code
The only thing from below; that you should expect to come out already styled is the `<% include CMSBreadcrumbs %>` everything else you must cater for yourself in the CSS file that was included earlier

```
<div class="cms-content center $BaseCSSClasses" data-layout-type="border" data-pjax-fragment="Content">
    <%-- This will add the breadcrumb that you see on every other menu item --%>
    <div class="cms-content-header north">
        <div class="cms-content-header-info">
            <% include CMSBreadcrumbs %>
        </div>
    </div>
    
    <div class="center">
        <%-- Our function in HelloWorldLeftAndMain.php --%>
        $Hello('USER');
        <%-- ^ outputs "Hello USER" --%>
    </div>

    <div class='south'>
        Some footer-worthy content
    </div>
</div>
```

Now all thats left to do is for you to `/dev/build` and `?flush=1` then you can check out our useless little module in the Administration Panel!


