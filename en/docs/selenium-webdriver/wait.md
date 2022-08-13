---
title: "Wait"
slug: "wait"
draft: false
images: []
weight: 9657
type: docs
toc: true
---

## Types of Wait in Selenium WebDriver
While running any web application it’s necessary to take loading time into consideration. If your code tries to access any element that is not yet loaded, WebDriver will throw an exception and your script will stop.

There are three types of Waits - 
- **Implicit Waits**
- **Explicit Waits**
- **Fluent Waits**

Implicit waits are used to set the waiting time throughout the program, while explicit waits are used only on specific portions.


<hr>

Implicit Wait
-------------

An implicit wait is to tell WebDriver to poll the DOM for a certain amount of time when trying to find an element or elements if they are not immediately available. Implicit waits are basically your way of telling WebDriver the latency that you want to see if specified web element is not present that WebDriver is looking for. The default setting is 0. Once set, the implicit wait is set for the life of the WebDriver object instance.
Implicit wait is declared in the instantiation part of the code using the following snippet.

Example in **Java**:

<!-- language: Java -->

    driver.manage().timeouts().implicitlyWait(15, TimeUnit.SECONDS);
    // You need to import the following class - import java.util.concurrent.TimeUnit;

Example in **C#**:

<!-- language: c# -->

    driver.Manage().Timeouts().ImplicitlyWait(TimeSpan.FromSeconds(15));

So in this case, you are telling WebDriver that it should wait 15 seconds in cases of specified element is not available on the UI (DOM).

<hr>

Explicit wait
-------------

You may encounter instances when some element takes more time to load. Setting implicit wait for such cases doesn’t make sense as browser will wait unnecessarily for the same time for every element, increasing the automation time. Explicit wait helps here by bypassing implicit wait altogether for some specific elements.

Explicit waits are intelligent waits that are confined to a particular web element. Using explicit waits you are basically telling WebDriver at the max it is to wait for X units of time before it gives up.

Explicit waits are done using the WebDriverWait and ExpectedConditions classes. In the below example, we shall wait up to 10 seconds for an element whose id is username to become visible before proceeding to the next command. Here are the steps.

Example in **Java**:

<!-- language: java -->

    //Import these two packages:
    import org.openqa.selenium.support.ui.ExpectedConditions;
    import org.openqa.selenium.support.ui.WebDriverWait;

    //Declare a WebDriverWait variable. In this example, we will use myWaitVar as the name of the variable.
    WebDriverWait myWaitVar = new WebDriverWait(driver, 30);

    //Use myWaitVar with ExpectedConditions on portions where you need the explicit wait to occur. In this case, we will use explicit wait on the username input before we type the text tutorial onto it.
    myWaitVar.until(ExpectedConditions.visibilityOfElementLocated(By.id(“username”)));
    driver.findElement(By.id(“username”)).sendKeys(“tutorial”);

ExpectedConditions class has some predefined common conditions to wait for an element. [Click here][2] to see list of these conditions in Java binding.

Example in **C#**:

<!-- language: c# -->

    using OpenQA.Selenium;
    using OpenQA.Selenium.Support.UI;
    using OpenQA.Selenium.PhantomJS;

    // You can use any other WebDriver you want, such as ChromeDriver.
    using (var driver = new PhantomJSDriver())
    {
        driver.Navigate().GoToUrl("http://somedomain/url_that_delays_loading");

        // We aren't going to use it more than once, so no need to declare this a variable.
        new WebDriverWait(driver, TimeSpan.FromSeconds(10))
            .Until(ExpectedConditions.ElementIsVisible(By.Id("element-id")));

        // After the element is detected by the previous Wait, 
        // it will display the element's text
        Console.WriteLine(driver.FindElement(By.Id("element-id")).Text);
    }

In this example, system will wait for 10 seconds until the element is visible. If the element will not be visible after the timeout, the WebDriver will throw a `WebDriverTimeoutException`.

*Please note: If the element is visible before the 10 second timeout, system will immediately proceed for further process.*

<hr>

Fluent wait
-------------

Unlike implicit and explicit wait, fluent wait uses two parameters. Timeout value and polling frequency. Let’s say we have timeout value as 30 seconds and polling frequency as 2 seconds. WebDriver will check for element after every 2 seconds until timeout value (30 seconds). After timeout value is exceeded without any result, exception is thrown. Below is a sample code which shows implementation of fluent wait.

Example in **Java**:

<!-- language: java -->

    Wait wait = new FluentWait(driver).withTimeout(30, SECONDS).pollingEvery(2, SECONDS).ignoring(NoSuchElementException.class);


    WebElement testElement = wait.until(new Function() {
        public WebElement apply(WebDriver driver) {
            return driver.findElement(By.id("testId"));
        }
    });

Another advantage of using fluent wait is, we can ignore specific types of exceptions (Eg. NoSuchElementExceptions) while waiting. Due to all these provisions, fluent wait is helpful in AJAX applications as well as in scenarios when element load time fluctuates often. Strategic use of fluent wait significantly improves automation efforts.

<hr>


  [1]: https://www.nuget.org/packages/Selenium.Support/
  [2]: https://seleniumhq.github.io/selenium/docs/api/java/org/openqa/selenium/support/ui/ExpectedConditions.html
  [3]: https://watirwebdriver.com/

## Different types of explicit wait conditions
In explicit wait, you expect for a condition to happen. For example you want to wait until an element is clickable.

Here is a demonstration of a few common problems.

*Please note: In all of these examples you can use any `By` as a locator, such as `classname`, `xpath`, `link text`, `tag name` or `cssSelector`*

<hr>

Wait until element is visible
---------------------------------

For example, if your website takes some time to load, you can wait until the page completes loading, and your element is visible to the WebDriver.

*C#*

<!-- language: c# -->

    WebDriverWait wait = new WebDriverWait(driver, TimeSpan.FromSeconds(10));
    wait.Until(ExpectedConditions.ElementIsVisible(By.Id("element-id")));

*Java*

<!-- language: java -->

    WebDriverWait wait = new WebDriverWait(driver, 10);
    wait.until(ExpectedConditions.visibilityOfElementLocated(By.id("element-id")));


<hr>

Wait until element is not visible anymore
-----------------------------------

Same as before, but reversed.

*C#*
    
<!-- language: c# -->

    WebDriverWait wait = new WebDriverWait(driver, TimeSpan.FromSeconds(10));
    wait.Until(ExpectedConditions.InvisibilityOfElementLocated(By.Id("element-id")));

*Java*

<!-- language: java -->

    WebDriverWait wait = new WebDriverWait(driver, 10);
    wait.until(ExpectedConditions.invisibilityOfElementLocated(By.id("element-id")));

<hr>

Wait until text is present in the specified element
----------------------------------------------------

*C#*
    
<!-- language: c# -->

    IWebElement element = driver.FindElement(By.Id("element-id"));

    WebDriverWait wait = new WebDriverWait(driver, TimeSpan.FromSeconds(10));
    wait.Until(ExpectedConditions.TextToBePresentInElement(element, "text"));

*Java*
    
<!-- language: java -->

    WebElement element = driver.findElement(By.id("element-id"));

    WebDriverWait wait = new WebDriverWait(driver, 10);
    wait.until(ExpectedConditions.textToBePresentInElement(element, "text"));


If you go to the given link above, you will see all the wait condition there.

The difference between the usage of these wait conditions are in their input parameter. 

That means you need to pass the WebElement if its input parameter is WebElement, you need to pass the element locator if it takes the By locator as its input parameter.

Choose wisely what kind of wait condition you want to use.

## Waiting For Ajax Requests to Complete
C#
==

<!-- language: c# -->

    using OpenQA.Selenium
    using OpenQA.Selenium.Chrome;
    using System.Threading;

    namespace WebDriver Tests
    {
        class WebDriverWaits
        {
            static void Main()
            {
                IWebDriver driver = new ChromeDriver(@"C:\WebDriver");
                
                driver.Navigate().GoToUrl("page with ajax requests");
                CheckPageIsLoaded(driver);

                // Now the page is fully loaded, you can continue with further tests.
            }

            private void CheckPageIsLoaded(IWebDriver driver)
            {
                while (true)
                {
                    bool ajaxIsComplete = (bool)(driver as IJavaScriptExecutor).ExecuteScript("return jQuery.active == 0");
                    if (ajaxIsComplete)
                        return;
                    Thread.Sleep(100);
                }
            }
        }
    }

This example is useful for pages where ajax requests are made, here we use the `IJavaScriptExecutor` to run our own JavaScript code. As it is within a `while` loop it will continue to run until `ajaxIsComplete == true` and so the return statement is executed.

We check that all ajax requests are complete by confirming that `jQuery.active` is equal to `0`. This works because each time a new ajax request is made `jQuery.active` is incremented and each time a request complements it is decremented, from this we can deduce that when `jQuery.active == 0` all ajax requests must be complete.

## Fluent Wait
Fluent wait is a superclass of **explicit** wait (`WebDriverWait`) that is more configurable since it can accept an argument to the wait function. I'll pass on **implicit** wait, since it's a [best practice][1] to avoid it.

Usage (Java):

    Wait wait = new FluentWait<>(this.driver)
            .withTimeout(driverTimeoutSeconds, TimeUnit.SECONDS)
            .pollingEvery(500, TimeUnit.MILLISECONDS)
            .ignoring(StaleElementReferenceException.class)
            .ignoring(NoSuchElementException.class)
            .ignoring(ElementNotVisibleException.class);

    WebElement foo = wait.until(ExpectedConditions.presenceOfElementLocated(By.yourBy));

    // or use your own predicate:
    WebElement foo = wait.until(new Function() {
      public WebElement apply(WebDriver driver) {
        return element.getText().length() > 0;
      }
    });

When you use [Explicit wait][2] with it's defaults it's simply a `FluentWait<WebDriver>` with defaults of: `DEFAULT_SLEEP_TIMEOUT = 500;` and ignoring `NotFoundException`.


  [1]: http://sqa.stackexchange.com/questions/12583/is-it-a-bad-practice-to-use-implicit-wait-in-selenium-webdriver-should-one-use
  [2]: http://grepcode.com/file/repo1.maven.org/maven2/org.seleniumhq.selenium/selenium-support/2.47.1/org/openqa/selenium/support/ui/WebDriverWait.java#WebDriverWait

## Fluent wait
Each FluentWait instance defines the maximum amount of time to wait for a condition, as well as the frequency with which to check the condition. Furthermore, the user may configure the wait to ignore specific types of exceptions whilst waiting, such as NoSuchElementExceptions when searching for an element on the page.It is associated with the driver.
  

    Wait<WebDriver> wait = new FluentWait<WebDriver>(driver)
    .withTimeout(30, SECONDS) //actuall wait for the element to pe present
    .pollingEvery(5, SECONDS) //selenium will keep looking for the element after every 5seconds
    .ignoring(NoSuchElementException.class); //while ignoring this condition
    wait.until(ExpectedConditions.visibilityOf(driver.findElement(By.id("username"));

