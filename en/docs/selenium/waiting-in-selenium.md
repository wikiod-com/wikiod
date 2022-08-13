---
title: "Waiting in Selenium"
slug: "waiting-in-selenium"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

One of the most common stumbling blocks for newer Selenium users is waiting until a page is fully loaded. Human users can easily tell if a page has fully loaded or if it is still loading. Selenium, however, just waits for a set amount of time. Therefore, it is often convenient to have a good way to wait for elements in a page. While it is *possible* to do this with a loop and sleep() functions, there are much cleaner ways already built into Selenium.

## Explicit Wait in Python
When browser navigates to a dynamic page (most commonly AJAX-based web application), the elements on the page can take different time to load, and furthermore: some elements will only load in response to some user actions. In such cases direct retrieval of an element may fail: 

    # Don't do this: may fail on dynamic page with ElementNotVisibleException
    element = driver.find_element_by_name('q') 

The most obvious solution it seems to introduce the wait before retrieving elements:

    # Don't do this: inefficient solution for ElementNotVisibleException 
    time.sleep(5) # delays for 5 seconds  
    element = driver.find_element_by_name('q') 

But such solution is inefficient, since it causes the test to always wait for 5 seconds, even when the element in most cases appears after 1 second (and only sometimes requires up to 5 seconds). It doesn't look much if it's one place, but usually each test deals with multiple elements, and there are multiple tests, which adds up to overall test duration. 

A better solution is to wait for element to appear for up to 5 seconds, but return from the wait as soon as element is found. `WebDriverWait` allows you to do just that.

The following example navigates to www.google.com, waits (for up to 5 seconds) for the search bar to load, and then searches for "selenium".

    from selenium import webdriver
    from selenium.webdriver.support.ui import WebDriverWait
    from selenium.webdriver.support import expected_conditions as EC
    from selenium.webdriver.common.keys import Keys
    from selenium.webdriver.common.by import By

    # Create a new chromedriver instance
    driver = webdriver.Chrome()

    # Go to www.google.com
    driver.get("https://www.google.com")

    try:
        # Wait as long as required, or maximum of 5 sec for element to appear
        # If successful, retrieves the element
        element = WebDriverWait(driver,5).until(
             EC.presence_of_element_located((By.NAME, "q")))

        # Type "selenium"
        element.send_keys("selenium")
        
        #Type Enter
        element.send_keys(Keys.ENTER)
    
    except TimeoutException:
        print("Failed to load search bar at www.google.com")
    finally:
        driver.quit()

## Wait in Java with selenium
***Explicit wait*** : Wait for a certain condition to occur before proceeding further in the code.

    WebDriver driver = new FirefoxDriver();
    driver.get("http://google.com");
    WebElement myElement = (new WebDriverWait(driver, 10))
      .until(ExpectedConditions.presenceOfElementLocated(By.id("myElement")));


***Implicit wait:***   Wait for a certain amount of time when trying to find an element or elements if they are not immediately available.

    WebDriver driver = new FirefoxDriver();
    driver.manage().timeouts().implicitlyWait(10, TimeUnit.SECONDS);
    driver.get("http://google.com");
    WebElement myElement = driver.findElement(By.id("myElement"));



