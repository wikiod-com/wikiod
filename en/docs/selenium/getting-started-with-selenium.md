---
title: "Getting started with selenium"
slug: "getting-started-with-selenium"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Simple Selenium test in Java
Below code is simple java program using selenium.
The journey of the below code is

1.   Open Firefox browser
2.   Open google page
3.   Print title of Google page
4.   Find the search box location
5.   Pass the value as Selenium in the search box
6.   Submit the form
7.   Shutdown the browser


    package org.openqa.selenium.example;
    
    import org.openqa.selenium.By;
    import org.openqa.selenium.WebDriver;
    import org.openqa.selenium.WebElement;
    import org.openqa.selenium.firefox.FirefoxDriver;
    import java.util.concurrent.TimeUnit;
    
    public class Selenium2Example  {
        public static void main(String[] args) {
            // Create a new instance of the Firefox driver
            WebDriver driver = new FirefoxDriver();
    
            // An implicit wait is to tell WebDriver to poll the DOM for a certain amount of time 
            // when trying to find an element or elements if they are not immediately available. 
            // The default setting is 0. Once set, the implicit wait is set for the life of the WebDriver object instance.   
            driver.manage().timeouts().implicitlyWait(10, TimeUnit.SECONDS);

            // Maximize the browser window to fit into screen
            driver.manage().window().maximize();
            
            // Visit Google
            driver.get("http://www.google.com");
    
            // Check the title of the page
            System.out.println("Page title is: " + driver.getTitle());
    
            // Find the text input element by its name
            WebElement element = driver.findElement(By.name("q"));
    
            // Enter something to search for
            element.sendKeys("Selenium!");
    
            // Now submit the form. WebDriver will find the form for us from the element
            element.submit();
    
            //Close the browser
            driver.quit();
        }
    }

## Simple Selenium Example in C#
    //Create a new ChromeDriver
    IWebDriver driver = new ChromeDriver();

    //Navigate to www.google.com
    driver.Navigate().GoToUrl("https://www.google.com");

    //Find the WebElement of the search bar
    IWebElement element = driver.FindElement(By.Name("q"));

    //Type Hello World into search bar
    element.SendKeys("Hello World");

    //Submit the input
    element.Submit();

    //Close the browser
    driver.Quit();

## Simple selenium test in python
    from selenium import webdriver
    
    # Create a new chromedriver
    driver = webdriver.Chrome()
    
    # Go to www.google.com
    driver.get("https://www.google.com")
    
    # Get the webelement of the text input box
    search_box = driver.find_element_by_name("q")

    # Send the string "Selenium!" to the input box
    seach_box.send_keys("Selenium!")

    # Submit the input, which starts a search
    search_box.submit()

    # Wait to see the results of the search
    time.sleep(5)
    
    # Close the driver
    driver.quit()

## Setting up python Selenium via terminal (BASH)
The easiest way is to use [pip][1] and [VirtualEnv][2]. Selenium also requires [python 3.*][3].

Install virtualenv using:

    $: pip install virtualenv
Create/enter a directory for your Selenium files:

    $: cd my_selenium_project

Create a new VirtualEnv in the directory for your Selenium files:

    $: virtualenv -p /usr/bin/python3.0 venv
Activate the VirtualEnv:

    $: source venv/bin/active
You should see now see (venv) at the beginning of each bash line. Install Selenium using pip:

    $: pip install selenium
Selenium comes with the FireFox driver by default.<br>
If you want to run Selenium in google chrome, also do this:

    $: pip install chromedriver
    
You now have a version-controlled VirtualEnv. To make sure everything is set up correctly:

Start python:

    $: python
Prints out:

    Python 2.7.10 (default, Jul 14 2015, 19:46:27) 
    [GCC 4.2.1 Compatible Apple LLVM 6.0 (clang-600.0.39)] on darwin
    Type "help", "copyright", "credits" or "license" for more information.
Create a new webdriver (in this case, a chromedriver), and go to www.google.com:

    >>> from selenium import webdriver
    >>> driver = webdriver.Chrome()
    >>> driver.get("https://www.google.com")
Close the driver and python interpreter:

    >>> driver.quit()
    >>> quit()

Deactivate the VirtualEnv:
    
    $: deactivate

If the line `driver = webdriver.Chrome()` is throwing errors:
* Make sure you also have the chrome browser installed. If you don't, the Selenium chromedriver can not access the Chrome binary.
* webdriver.Chrome() can also take a parameter for your chromedriver location. If you installed it using pip, try (on mac) `driver = webdriver.Chrome("./venv/selenium/webdriver/chromedriver")`.


  [1]: https://pypi.python.org/pypi/pip
  [2]: http://docs.python-guide.org/en/latest/dev/virtualenvs/
  [3]: https://www.python.org/download/releases/3.0/

