---
title: "Setting  Getting Browser window size"
slug: "setting--getting-browser-window-size"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

Setting or getting window size of any browser during automation

## Syntax
 - driver.manage().window().maximize();
 - driver.manage().window().setSize(*DimensionObject*);
 - driver.manage().window().getSize()


## JAVA
Set to maximum Browser window size :

    //Initialize Browser
    System.setProperty("webdriver.gecko.driver", "E:\\path\\to\\geckodriver.exe");
    WebDriver driver = new FirefoxDriver();
    driver.get("https://www.google.com/");

    //Set Browser window size
    driver.manage().window().maximize();

Set specific window size :

    //Initialize Browser
    System.setProperty("webdriver.gecko.driver", "E:\\path\\to\\geckodriver.exe");
    WebDriver driver = new FirefoxDriver();
    driver.get("https://www.google.com/");

    //Initialize Dimension class object and set Browser window size
    org.openqa.selenium.Dimension d = new org.openqa.selenium.Dimension(400, 500);
    driver.manage().window().setSize(d);


Get Browser window size :

    //Initialize Browser
    System.setProperty("webdriver.gecko.driver", "E:\\path\\to\\geckodriver.exe");
    WebDriver driver = new FirefoxDriver();
    driver.get("https://www.google.com/");

    //Get Browser window size and print on console
    System.out.println(driver.manage().window().getSize());

