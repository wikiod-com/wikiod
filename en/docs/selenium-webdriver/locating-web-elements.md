---
title: "Locating Web Elements"
slug: "locating-web-elements"
draft: false
images: []
weight: 9736
type: docs
toc: true
---

## Syntax
- ByChained(params By[] bys)

Items are found in Selenium through the use of *locators* and the `By` class. In order to make a robust automation project with Selenium, one should use locators for Web Elements smartly. The locators should be **descriptive, unique, and unlikely to change** so you won't get false positives in tests for example. The priority is to use:

 1. **ID** - since it's unique and you'll get exactly the element you want.
 2. **Class Name** - It's descriptive and can be unique in a given context.
 3. **CSS** ([better performance than xpath][1]) - For more complicated selectors.
 4. **XPATH** - Where CSS can't be used ([XPATH Axis][2]), e.g. `div::parent`.

The rest of the locators are prone to changes or rendering, and preferebly be avoided.

**Rule of thumb:** if your code cannot locate a particular element, one reason could be that your code hasn't waited for all the DOM elements to download. Consider telling your program to "wait" for a short period of time (try 3-5 seconds, and then slowly increase as needed) before searching for said element. Here is an example in Python, taken from [this question][3]:

    from selenium import webdriver
    import time
    
    browser = webdriver.Firefox()
    browser.get("https://app.website.com")
    
    reports_element = browser.find_element_by_xpath("//button[contains(text(), 'Reports')]")

    # Element not found! Try giving time for the browser to download all DOM elements:
    time.sleep(10)

    reports_element = browser.find_element_by_xpath("//button[contains(text(), 'Reports')]")
    # This returns correct value!

----------


  [1]: http://elementalselenium.com/tips/34-xpath-vs-css-revisited-2
  [2]: http://www.w3schools.com/xml/xpath_axes.asp
  [3]: http://stackoverflow.com/q/40733868/3856609

## Locating page elements using WebDriver
To interact with WebElements in a webpage, first we need to identify the location of the element.

***By*** is the keyword available in selenium.

You can locate the elements By..

 1. **By** *ID*
 2. **By** *Class Name*
 3. **By** *TagName*
 4. **By** *Name*
 5. **By** *Link Text*
 6. **By** *Partial Link Text*
 7. **By** *CSS Selector*
 8. **By** *XPath*
 9. **Using** *JavaScript*

Consider the below script example

    

    <form name="loginForm">
    Login Username: <input id="username" name="login" type="text" />
    Password: <input id="password" name="password" type="password" />
    <input name="login" type="submit" value="Login" />
</form>

In the above code the username and password are set using id's.
Now you are going to identify the elements with id.



    driver.findElement(By.id(username));

    driver.findElement(By.id(password));


   As selenium support 7 different languages, this document gives you an idea to locate the elements in all the languages.

<hr>

By ID
-----

Example of how to find an element using ID:

    <div id="coolestWidgetEvah">...</div>
    
    Java       -  WebElement element = driver.findElement(By.id("coolestWidgetEvah"));
    C#         -  IWebElement element = driver.FindElement(By.Id("coolestWidgetEvah"));
    Python     -  element = driver.find_element_by_id("coolestWidgetEvah")
    Ruby       -  element = driver.find_element(:id, "coolestWidgetEvah")
    JavaScript/Protractor -  var elm = element(by.id("coolestWidgetEvah"));

<hr>

By Class Name
-------------

Example of how to find an element using class name:

    <div class="cheese"><span>Cheddar</span></div>
    
    Java       -  WebElement element = driver.findElement(By.className("cheese"));
    C#         -  IWebElement element = driver.FindElement(By.ClassName("cheese"));
    Python     -  element = driver.find_element_by_class_name("cheese")
    Ruby       -  cheeses = driver.find_elements(:class, "cheese")
    JavaScript/Protractor -  var elm = element(by.className("cheese"));

<hr>

By Tag Name
-----------

Example of how to find an element using tag name:

    <iframe src="..."></iframe>
    
    Java       -  WebElement element = driver.findElement(By.tagName("iframe"));
    C#         -  IWebElement element = driver.FindElement(By.TagName("iframe"));
    Python     -  element = driver.find_element_by_tag_name("iframe")
    Ruby       -  frame = driver.find_element(:tag_name, "iframe")
    JavaScript/Protractor -  var elm = element(by.tagName("iframe"));
    
<hr>

By Name
-------

Example of how to find an element using name:

    <input name="cheese" type="text"/>
    
    Java       -  WebElement element = driver.findElement(By.name("cheese"));
    C#         -  IWebElement element = driver.FindElement(By.Name("cheese"));
    Python     -  element = driver.find_element_by_name("cheese")
    Ruby       -  cheese = driver.find_element(:name, "cheese")
    JavaScript/Protractor -  var elm = element(by.name("cheese"));

<hr>

By Link Text
------------

Example of how to find an element using link text:

    <a href="http://www.google.com/search?q=cheese">cheese</a>>
    
    Java       -  WebElement element = driver.findElement(By.linkText("cheese"));
    C#         -  IWebElement element = driver.FindElement(By.LinkText("cheese"));
    Python     -  element = driver.find_element_by_link_text("cheese")
    Ruby       -  cheese = driver.find_element(:link, "cheese")
    JavaScript/Protractor -  var elm = element(by.linkText("cheese"));

<hr>

By Partial Link Text
--------------------

Example of how to find an element using partial link text:

    <a href="http://www.google.com/search?q=cheese">search for cheese</a>>
    
    Java       -  WebElement element = driver.findElement(By.partialLinkText("cheese"));
    C#         -  IWebElement element = driver.FindElement(By.PartialLinkText("cheese"));
    Python     -  element = driver.find_element_by_partial_link_text("cheese")
    Ruby       -  cheese = driver.find_element(:partial_link_text, "cheese")
    JavaScript/Protractor -  var elm = element(by.partialLinkText("cheese"));

<hr>    

By CSS Selectors
------

    
Example of how to find an element using CSS Selectors:

    <div id="food" class="dairy">milk</span>

    Java       -  WebElement element = driver.findElement(By.cssSelector("#food.dairy")); //# is used to indicate id and . is used for classname.
    C#         -  IWebElement element = driver.FindElement(By.CssSelector("#food.dairy"));
    Python     -  element = driver.find_element_by_css_selector("#food.dairy")
    Ruby       -  cheese = driver.find_element(:css, "#food span.dairy.aged")
    JavaScript/Protractor -  var elm = element(by.css("#food.dairy"));

Here's an article about creating CSS Selectors: http://www.w3schools.com/cssref/css_selectors.asp

<hr>

By XPath
--------

Example of how to find an element using XPath:

    <input type="text" name="example" />
    
    Java       -  WebElement element = driver.findElement(By.xpath("//input"));
    C#         -  IWebElement element = driver.FindElement(By.XPath("//input"));
    Python     -  element = driver.find_element_by_xpath("//input")
    Ruby       -  inputs = driver.find_elements(:xpath, "//input")
    JavaScript/Protractor -  var elm = element(by.xpath("//input"));

Here's an article about XPath: http://www.w3schools.com/xsl/xpath_intro.asp

<hr>

Using JavaScript
----------------

You can execute arbitrary javascript to find an element and as long as you return a DOM Element, it will be automatically converted to a WebElement object.

Simple example on a page that has jQuery loaded:


    Java       -  WebElement element = (WebElement) 
    ((JavascriptExecutor)driver).executeScript("return $('.cheese')[0]");
    
    C#         -  IWebElement element = (IWebElement)
    ((IJavaScriptExecutor)driver).ExecuteScript("return $('.cheese')[0]");
    
    Python     -  element = driver.execute_script("return $('.cheese')[0]");
    Ruby       -  element = driver.execute_script("return $('.cheese')[0]")
    JavaScript/Protractor -
    
*Please note: This method will not work if your particular WebDriver doesn't support JavaScript, such as [SimpleBrowser][1].*


  [1]: https://www.wikiod.com/selenium-webdriver/headless-browsers#SimpleBrowser [C#]

## Selecting by multiple criteria [C#]
It's also possible to use selectors together. This is done by using the `OpenQA.Selenium.Support.PageObjects.ByChained` object:

<!-- language: c# -->

    element  = driver.FindElement(new ByChained(By.TagName("input"), By.ClassName("class"));

Any number of `By`s can be chained and are used as an AND type selection (i.e. all `By`s are matched)

## Selecting elements before the page stops loading
When calling `driver.Navigate().GoToUrl(url);`, the code execution stops until the page is fully loaded. This is sometimes unnecessary when you just want to extract data.

*Note: The code samples below could be considered hacks. There is no "official" way of doing this.*


----------


Create a new thread
-------------------

Create and launch a thread for loading a webpage, then use [Wait][1]. 

*C#*

    using (var driver = new ChromeDriver())
    {
        new Thread(() =>
        {
            driver.Navigate().GoToUrl("http://stackoverflow.com");
        }).Start();

        new WebDriverWait(driver, TimeSpan.FromSeconds(10))
          .Until(ExpectedConditions.ElementIsVisible(By.XPath("//div[@class='summary']/h3/a")));
    }


----------


Use Timeouts
------------

Using a WebDriverTimeout, you can load a page, and after a certain period of time, it will throw an exception, which will make the page stop loading. In the catch block, you can use [Wait][1].

*C#*

    using (var driver = new ChromeDriver())
    {
        driver.Manage().Timeouts().SetPageLoadTimeout(TimeSpan.FromSeconds(5));

        try
        {
            driver.Navigate().GoToUrl("http://stackoverflow.com");
        }
        catch (WebDriverTimeoutException)
        {
            new WebDriverWait(driver, TimeSpan.FromSeconds(10))
              .Until(ExpectedConditions.ElementIsVisible
                (By.XPath("//div[@class='summary']/h3/a")));
        }
    }

**The problem**: When you set the timeout for too short, the page will stop loading regardless of whether is your desired element present. When you set the timeout for too long, you're going to negate the performance benefit.



  [1]: https://www.wikiod.com/selenium-webdriver/wait

