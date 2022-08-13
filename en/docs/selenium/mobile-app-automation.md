---
title: "Mobile app automation"
slug: "mobile-app-automation"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Android + Chrome + Python
To be able to run tests `Chrome` broser should be pre-installed on `Android` device, 

## Python + Chrome + Android
To be able to work with web-application on `Android` device using `Selenium` below pre-conditions should be met:

 - [`Android SDK`][1] installed on computer
 - `Chrome` browser installed on `Android` device
 - [Debugging mode][2] enabled on `Android` device

Start `adb` and `chromedriver` server with below commands from `cmd`/`Terminal`:

    adb start-server
    chromedriver

Note down `chromedriver` server port number from log that looks like

> Starting ChromeDriver 2.15.322448 (52179c1b310fec1797c81ea9a20326839860b7d3) on port **9515**

Connect `Android` device to computer with `USB` cable

Below is simple `Python` code to get `Google` page:

    from selenium import webdriver

    capabilities = {
     'chromeOptions': {
       'androidPackage': 'com.android.chrome',
                       }
                    }
    driver = webdriver.Remote('http://localhost:9515', capabilities) # Specify your port number value 
    driver.get('http://google.com')
    driver.quit()


  [1]: http://www.androidcentral.com/installing-android-sdk-windows-mac-and-linux-tutorial
  [2]: https://www.kingoapp.com/root-tutorials/how-to-enable-usb-debugging-mode-on-android.htm

