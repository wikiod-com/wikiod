---
title: "Functions of PhantomJS"
slug: "functions-of-phantomjs"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Options and Services
**Options** are used for adding capabilities such as "User-Agent".

Example in C#:

    var options = new PhantomJSOptions();
    options.AddAdditionalCapability("phantomjs.page.settings.userAgent", 
        "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:25.0) Gecko/20100101 Firefox/25.0");

    using (var driver = new PhantomJSDriver(options))
    {
        //code
    }


----------


**Services** are used to modify behavior of your PhantomJS instance, such as hiding the command prompt or for disabling loading images.

[Here's a list of all services][1].

Example in C#:


    var service = PhantomJSDriverService.CreateDefaultService();
    service.HideCommandPromptWindow = true;
    service.LoadImages = false;

    using (var driver = new PhantomJSDriver(service))
    {
        //code
    }


  [1]: http://phantomjs.org/api/webpage/property/settings.html

