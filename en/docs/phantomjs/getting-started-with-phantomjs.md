---
title: "Getting started with phantomjs"
slug: "getting-started-with-phantomjs"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
For Visual Studio [NuGet]:
--------
The easiest way of installing PhantomJS is by using a NuGet Package Manager.

In your project, right click "References", and click on "Manage NuGet Packages" as shown:

[![Visual Studio screenshot][1]][1]

Then, type "PhantomJS" to the search bar, select it and install it as shown below.

[![NuGet Package Manager][2]][2]

Here's a list of other recommended packages:

- Selenium.WebDriver - To use PhantomJS with Selenium
- Selenium.Support - To further extend capabilities of Selenium

Now, add these references at the beginning:

    using OpenQA.Selenium;
    using OpenQA.Selenium.PhantomJS;

Now you can test it with a simple program like this [C#]:

    using (var driver = new PhantomJSDriver())
    {
        driver.Navigate().GoToUrl("http://stackoverflow.com/");
    
        var questions = driver.FindElements(By.ClassName("question-hyperlink"));
    
        foreach (var question in questions)
        {
            // This will display every question header on StackOverflow homepage.
            Console.WriteLine(question.Text);
        }
    }

  [1]: http://i.stack.imgur.com/ubRi5.png
  [2]: http://i.stack.imgur.com/bzSr4.png

## Loading a Webpage
    var page = require('webpage').create();
    page.open('http://www.google.com', function(status) {
      console.log("Status: " + status);
      var title = page.evaluate(function() {
        return document.title;
      });
      console.log("Loaded page: " + title);
      phantom.exit();
    });

