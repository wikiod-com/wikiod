---
title: "Handle an alert"
slug: "handle-an-alert"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Selenium with Java
Here's how to Handle a popup alert in Java with Selenium:

There are 3 types of popups.

1. **Simple alert**       : alert("This is a simple alert");
2. **Confirmation alert** : var popuResult = confirm("Confirm pop up with OK and Cancel button");
3. **Prompt alert**       : var person = prompt("Do you like stackoverflow?", "Yes/No");

Its upto user which type of popup need to be handled in their test case.

Either you can 
1. accept() To accept the alert

2. dismiss() To dismiss the alert

3. getText() To get the text of the alert

4. sendKeys() To write some text to the alert

***For simple alert:***

        Alert simpleAlert = driver.switchTo().alert();
        String alertText = simpleAlert.getText();
        System.out.println("Alert text is " + alertText);
        simpleAlert.accept();

***For Confirmation alert :*** 

        Alert confirmationAlert = driver.switchTo().alert();
        String alertText = confirmationAlert.getText();
        System.out.println("Alert text is " + alertText);
        confirmationAlert.dismiss();

***For Prompt alert :*** 

        Alert promptAlert  = driver.switchTo().alert();
        String alertText = promptAlert .getText();
        System.out.println("Alert text is " + alertText);
        //Send some text to the alert
        promptAlert .sendKeys("Accepting the alert");
        Thread.sleep(4000); //This sleep is not necessary, just for demonstration
        promptAlert .accept();
    

according to your needs.

Another way you can do this, is wrap your code inside a try-catch:

    try{
       // Your logic here.
    } catch(UnhandledAlertException e){
      Alert alert = driver.switchTo().alert();
      alert.accept();
    }
    // Continue.

## C#
Here's how to close a popup alert in C# with Selenium:


    IAlert alert = driver.SwitchTo().Alert(); 
    // Prints text and closes alert
    System.out.println(alert.Text);
    alert.Accept();
    or
    alert.Dismiss();

according to your needs.

Another way you can do this, is wrap your code inside a try-catch:

    try{
       // Your logic here.
    } catch(UnhandledAlertException e){
      var alert = driver.SwitchTo().Alert();
      alert.Accept();
    }
    // Continue.

## Python
There are multiple ways to switch to alert pop-up in `Python`:

    

 1. *Deprecated*:


    alert = driver.switch_to_alert()

 2. *Using `switch_to`*:

 

    alert = driver.switch_to.alert

 3. *Using `ExplicitWait`*:

 

     from selenium.webdriver.common.by import By
     from selenium.webdriver.support.ui import WebDriverWait
     from selenium.webdriver.support import expected_conditions as EC
    
     alert = WebDriverWait(driver, TIMEOUT_IN_SECONDS).until(EC.alert_is_present())
   

 4. *By declaring the instance of `Alert` class*:


    from selenium.webdriver.common.alert import Alert
    
    alert = Alert(driver)

To fill input field in pop-up triggered by `JavaScript` `prompt()`:

    alert.send_keys('Some text to send')

To confirm dialog pop-up*:

    alert.accept()

To dismiss:

    alert.dismiss()

To get text from pop-up:

    alert.text

  **P.S. `alert.dismiss()` could be used to confirm pop-ups triggered by `JavaScript` `alert()` as well as `alert.confirm()`*
    

     



