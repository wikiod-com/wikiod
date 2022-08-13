---
title: "The autoloader"
slug: "the-autoloader"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

When you make any changes to the classes then you need to run a dev/build?flush=1 to rebuild the manifest.

## MyClass.php
    <?php
    
    class MyClass {
        ...
    }
    
    class OtherClass {
        ...
    }
    
    ?>

Any class that has the same name as it's file name will be auto loaded by Silverstripe.

OtherClass will be loaded too because it is in a file which is being read.

MyPage.php

    <?php
    
    class MyPage_Controller extends BookingPage_Controller {
        ...
    }
    
    ?>

For controller functions you can omit the "_Controller" part int he file name.

If a directory is to be ignored then include a file named "_manifest_exclude"

