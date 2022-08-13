---
title: "Executing Javascript in the page"
slug: "executing-javascript-in-the-page"
draft: false
images: []
weight: 9725
type: docs
toc: true
---

## Syntax
- object ExecuteAsyncScript(string script, params object[] args);
- object ExecuteScript(string script, params object[] args);

## Python
To execute Javascript in python, use `execute_script("javascript script here")`. 
execute_script is called on a webdriver instance, and can be any valid javascript.

<!-- language: python -->

    from selenium import webdriver
    driver = webdriver.Chrome()
    driver.execute_script("alert('running javascript');")


## C#
In order to execute JavaScript in a `IWebDriver` instance you need to cast the `IWebDriver` to a new interface, `IJavaScriptExecutor`

<!-- language: c# -->

    IWebDriver driver;
    IJavaScriptExecutor jsDriver = driver as IJavaScriptExecutor;

You can now access all the methods available on the `IJavaScriptExecutor` instance which allow you to execute Javascript, for example:

<!-- language: c# -->

    jsDriver.ExecuteScript("alert('running javascript');");



## Java
To execute Javascript in Java, create a new webdriver that supports Javascript. To use the `executeScript()` function, either the driver must be cast to a `JavascriptExecutor`, or a new variable can be set to the value of the casted driver: `((JavascriptExecutor)driver)`. `driver.executeScript()` takes in a String that is valid Javascript.
    
<!-- language: java -->

    WebDriver driver = new ChromeDriver();
    JavascriptExecutor JavascriptExecutor = ((JavascriptExecutor)driver);
    JavascriptExecutor.executeScript("alert('running javascript');");

## Ruby
<!-- language: ruby -->

    require "selenium-webdriver"

    driver = Selenium::WebDriver.for :chrome
    driver.execute_script("alert('running javascript');")

