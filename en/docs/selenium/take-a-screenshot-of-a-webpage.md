---
title: "Take a screenshot of a webpage"
slug: "take-a-screenshot-of-a-webpage"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Java Selenium take/save screenshot of webpage and add on report

    public void Screenshot() throws Throwable{
        final byte[] screenshot = ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);
        scenario.embed(screenshot, "image/png"); // ... and embed it in the report.
        Thread.sleep(1000);
    }

Alternately

            
     public static void captureScreenShot(WebDriver ldriver){
 
      // Take screenshot and store as a file format
      File src= ((TakesScreenshot)ldriver).getScreenshotAs(OutputType.FILE);
    try {
      // now copy the  screenshot to desired location using copyFile method
 
     FileUtils.copyFile(src, new File("C:/selenium/"+System.currentTimeMillis()+".png"));
       }
 
    catch (IOException e)
 
    { 
    System.out.println(e.getMessage());
      }
    }

## Python Selenium take/save screenshot of webpage
    from selenium import webdriver
    
    # Create a new cromedriver
    driver = webdriver.Chrome()
    # Go to www.google.com
    driver.get("https://www.google.com")
    # Saves a .png file with name my_screenshot_name to the directory that
    # you are running the program from.
    screenshot_name = "my_screenshot_name.png"
    driver.save_screenshot(screenshot_name)
driver.save_screenshot will return 'true' if the screenshot was taken, and 'false' if it was not. Saving screenshots also works with headless browsers.
If you want to save a screenshot in a different directory, just add the filepath (relative to where you are running the code from). For example:
    
    screenshot_name = "screenshots/my_screenshot_name.png"
Will save the screenshot in directory "screenshots" inside the directory that python is being run from.
    

## C# TakeScreenshot extension
    public static Screenshot TakeScreenshot(this IWebDriver _driver)
    {
        return ((ITakesScreenshot)_driver).GetScreenshot();
    }


Usage example:
  
    driver.TakeScreenshot().SaveAsFile(@"/Test/Test.png",ImageFormat.Png);

