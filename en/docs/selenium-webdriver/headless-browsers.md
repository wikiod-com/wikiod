---
title: "Headless Browsers"
slug: "headless-browsers"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## PhantomJS [C#]
`PhantomJS` is a fully featured headless web browser **with** JavaScript support.

Before you start you will need to download a [PhantomJS][1] driver, and make sure to put this in the beginning of your code:

<!-- language: c# -->

    using OpenQA.Selenium;
    using OpenQA.Selenium.PhantomJS;

Great, now onto the initialization:

<!-- language: c# -->

    var driver = new PhantomJSDriver();
This will simply create a new instance of the PhantomJSDriver class. You can then use it the same way as every WebDriver such as:

<!-- language: c# -->

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

This works fine. However, the problem you probably encountered is, when working with UI, `PhantomJS` opens a new console window, which is not really wanted in most cases. Luckily, we can hide the window, and even slightly improve performance using `PhantomJSOptions`, and `PhantomJSDriverService`. Full working example below:

<!-- language: c# -->

    // Options are used for setting "browser capabilities", such as setting a User-Agent
    // property as shown below:
    var options = new PhantomJSOptions();
    options.AddAdditionalCapability("phantomjs.page.settings.userAgent", 
    "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:25.0) Gecko/20100101 Firefox/25.0");

    // Services are used for setting up the WebDriver to your likings, such as
    // hiding the console window and restricting image loading as shown below:
    var service = PhantomJSDriverService.CreateDefaultService();
    service.HideCommandPromptWindow = true;
    service.LoadImages = false;

    // The same code as in the example above:
    using (var driver = new PhantomJSDriver(service, options))
    {
        driver.Navigate().GoToUrl("http://stackoverflow.com/");

        var questions = driver.FindElements(By.ClassName("question-hyperlink"));

        foreach (var question in questions)
        {
            // This will display every question header on StackOverflow homepage.
            Console.WriteLine(question.Text);
        }
    }

*Pro tip: click on a class (e.g the `PhantomJSDriverService`), and press F12 to see exactly what they contain along with a brief description of what they do.*



  [1]: https://www.nuget.org/packages/PhantomJS/

## SimpleBrowser [C#]
`SimpleBrowser` is a lightweight WebDriver **without** JavaScript support. 

It is considerably faster than an aforementioned `PhantomJS`, however when it comes to functionality, it is limited to simple tasks with no fancy features.

Firstly, you will need to download the [SimpleBrowser.WebDriver][1] package and then put this code at the beginning:

    using OpenQA.Selenium;
    using SimpleBrowser.WebDriver;

Now, here is a short example on how to use it:

    using (var driver = new SimpleBrowserDriver())
    {
        driver.Navigate().GoToUrl("http://stackoverflow.com/");

        var questions = driver.FindElements(By.ClassName("question-hyperlink"));

        foreach (var question in questions)
        {
            // This will display every question header on StackOverflow homepage.
            Console.WriteLine(question.Text);
        }
    }

  [1]: https://www.nuget.org/packages/SimpleBrowser.WebDriver/

## Headless browser in Java
HTMLUnitDriver
--------------

HTMLUnitDriver is the most lightweight implementation of headless(GUI-less) browser for Webdriver based on HtmlUnit. It models HTML documents and provides an API that allows you to invoke pages, fill out forms, click links, etc. just like you do in your normal browser.
It supports JavaScript and works with AJAX libraries. It is used for testing and retrieving data from website. 


----------
**Example:**
Use of HTMLUnitDriver to fetch list of questions from `http://stackoverflow.com/`.


----------

    import java.util.List;
    import java.util.concurrent.TimeUnit;
    import org.openqa.selenium.By;
    import org.openqa.selenium.WebDriver;
    import org.openqa.selenium.WebElement;
    import org.openqa.selenium.htmlunit.HtmlUnitDriver;
            
    class testHeadlessDriver{
                private void getQuestions() {
                        WebDriver driver = new HtmlUnitDriver();
                        driver.get("http://stackoverflow.com/");
                        driver.manage().timeouts().implicitlyWait(60, TimeUnit.SECONDS);
                        List<WebElement> questions = driver.findElements(By.className("question-hyperlink"));
                        questions.forEach((question) -> {
                            System.out.println(question.getText());
                        });
                       driver.close();
                    }
        }


----------
It is same as any other browser(Mozilla Firefox, Google Chrome, IE), but it does not have GUI, execution is not visible on screen.

