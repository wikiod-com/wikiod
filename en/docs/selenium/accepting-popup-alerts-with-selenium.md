---
title: "Accepting popup alerts with Selenium"
slug: "accepting-popup-alerts-with-selenium"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Python example of Accepting alert
    from selenium import webdriver
    
    # Create a new webdriver
    driver = webdriver.Chrome()
    # Get a page that has a popup window (Use mouse to click "try it" button)
    driver.get("http://www.w3schools.com/js/tryit.asp?filename=tryjs_alert")
    # Accept the opened alert
    driver.switch_to.alert.accept()

## C# Extensions to WebDriver
    public static IWebDriver dismissAlert(this IWebDriver driver)
    {
        try
        {
            IAlert alert = driver.SwitchTo().Alert();
            alert.Dismiss();
        }
        catch {}
        return driver;
    }
    public static IWebDriver acceptAlert(this IWebDriver driver)
    {
        try
        {
            IAlert alert = driver.SwitchTo().Alert();
            alert.Accept();
        }
        catch { }
        return driver;
    }


How to use:

    driver.acceptAlert();
    driver.dismissAlert();

## Java
For simple alert:

     Alert simpleAlert = driver.switchTo().alert();
    String alertText = simpleAlert.getText();
    System.out.println("Alert text is " + alertText);
    simpleAlert.accept();

For Prompt alert:

    Alert promptAlert  = driver.switchTo().alert();
    String alertText = promptAlert.getText();
    System.out.println("Alert text is " + alertText);
    //Send some text to the alert
    promptAlert.sendKeys("Accepting the alert");
    Thread.sleep(4000); //This sleep is not necessary, just for demonstration
    promptAlert.accept();

For Confirmation alert:

    Alert confirmationAlert = driver.switchTo().alert();
    String alertText = confirmationAlert.getText();
    System.out.println("Alert text is " + alertText);
    confirmationAlert.accept();

Another way you can do this, is wrap your code inside a try-catch:

    try{
       // Your logic here.
    } catch(UnhandledAlertException e){
      Alert alert = driver.switchTo().alert();
      alert.accept();
    }

    // Continue.

