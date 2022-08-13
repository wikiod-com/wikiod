---
title: "Navigation"
slug: "navigation"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Syntax
 - **C#**
 - void Back()
 - void Forward()
 - void GotToUrl(string url)
 - void Refresh()
 - **Python**
 - driver.back()
 - driver.forward()
 - driver.get("URL")
 - driver.refresh()
 - **Java**
 - driver.navigate().back();
 - driver.navigate().forward(); 
 - driver.navigate().to("URL"); 
 - driver.navigate().refresh();



## Navigate () [Java]
For Navigate to any url :
   
    driver.navigate().to("http://www.example.com");

For move backwards :

    driver.navigate().back();

For move forwards :

    driver.navigate().forward();

For refresh the page :

    driver.navigate().refresh();



## Navigate() [C#]
It's possible to navigate the browser directly, like using the standard toolbar commands available on all browsers: 

[![enter image description here][1]][1]

You can create a navigation object by calling `Navigate()` on the driver:

<!-- language: c# -->
    IWebDriver driver
    INavigation navigation = driver.Navigate();

A navigation object allows you to perform numerous actions that navigate the browser around the web:

<!-- language: c# -->

    //like pressing the back button
    navigation.Back();
    //like pressing the forward button on a browser
    navigation.Forward();
    //navigate to a new url in the current window
    navigation.GoToUrl("www.stackoverflow.com");
    //Like pressing the reload button
    navigation.Refresh();


  [1]: http://i.stack.imgur.com/eZaZE.png

## Browser methods in WebDriver
WebDriver, The main interface to use for testing, which represents an idealised web browser. The methods in this class fall into three categories:

 - Control of the browser itself 
 - Selection of WebElements 
 - Debugging aids

Key methods are get(String), which is used to load a new web page, and the various methods similar to findElement(By), which is used to find WebElements. In this post we are going to learn browser controlling methods.
get

    void get(java.lang.String url)

Load a new web page in the current browser window. This is done using an HTTP GET operation, and the method will block until the load is complete. it is best to wait until this timeout is over, since should the underlying page change whilst your test is executing the results of future calls against this interface will be against the freshly loaded page.
      **Usage**

    //Initialising driver
     WebDriver driver = new FirefoxDriver();
     
     //setting timeout for page load
     driver.manage().timeouts().pageLoadTimeout(20, TimeUnit.SECONDS);
     
     //Call Url in get method
     driver.get("https://www.google.com");
     //or
     driver.get("https://seleniumhq.org");

**getCurrentUrl**

    java.lang.String getCurrentUrl()

Get a string representing the current URL that the browser is looking at. It returns the URL of the page currently loaded in the browser.

*Usage*

    //Getting current url loaded in browser & comparing with expected url
     String pageURL = driver.getCurrentUrl();
     Assert.assertEquals(pageURL, "https://www.google.com");

**getTitle**

    java.lang.String getTitle()

It returns the title of the current page, with leading and trailing whitespace stripped, or null if one is not already set.

*Usage*

    //Getting current page title loaded in browser & comparing with expected title
     String pageTitle = driver.getTitle();
     Assert.assertEquals(pageTitle, "Google");

    getPageSource

    java.lang.String getPageSource()

Get the source of the last loaded page. If the page has been modified after loading (for example, by Javascript) there is no guarantee that the returned text is that of the modified page.

*Usage*

    //get the current page source
     String pageSource = driver.getPageSource();

**close**

    void close()

Close the current window, quitting the browser if it's the last window currently open. If there are more than one window opened with that driver instance this method will close the window which is having current focus on it.

*Usage*

    //Close the current window
         driver.close();

**quit**


    void quit()

Quits this driver, closing every associated window. After calling this method we can not use any other method using same driver instance.

*Usage*

    //Quit the current driver session / close all windows associated with driver
         driver.quit();

These are all very useful methods available in Selenium 2.0 to control browser as required. 





