---
title: "Taking Screenshots"
slug: "taking-screenshots"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Taking Screenshots and saving in a particular path

## Syntax
 - File src = ((TakesScreenshot)driver).getScreenshotAs(OutputType.FILE);
 - FileUtils.copyFile(src, new File("D:\\screenshot.png"));


## JAVA
Code to take and save screenshot :

    public class Sample 
    {
        public static void main (String[] args) 
        {
            *//Initialize Browser*
            System.setProperty("webdriver.gecko.driver", "**E:\\path\\to\\geckodriver.exe**");
            WebDriver driver = new FirefoxDriver();
            driver.manage().window().maximize();        
            driver.get("https://www.google.com/");
            
            //Take Screesnshot
            File src = ((TakesScreenshot)driver).getScreenshotAs(OutputType.FILE);
            try {
                //Save Screenshot in destination file
                FileUtils.copyFile(src, new File("D:\\screenshot.png"));
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }



Takes Screenshot :

    File src = ((TakesScreenshot)driver).getScreenshotAs(OutputType.FILE);

Stores Screenshot from source to destination :

    FileUtils.copyFile(src, new File("D:\\screenshot.png"));



