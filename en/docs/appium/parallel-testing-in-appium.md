---
title: "Parallel Testing in Appium"
slug: "parallel-testing-in-appium"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Parallel execution in appium using selenium GRID concept.
Please find step by step process.



## Step By Step process
Parallel Testing with Appium using GRID:
I will describe the way which worked for me.
Create Selenium Grid with Appium

1)    Setup the selenium grid
Download selenium standalone server jar on local file system
Open your terminal and navigate to the directory to where you placed the jar file and execute the following command:

    java -jar selenium-server-standalone-2.53.3.jar -role hub
    Open http://localhost:4444/grid/console and you should be able to see GRID console in your browser.
2)    Setup the Appium Nodes
Here you have to create the json files. Suppose you want to run on two devices then create two different json file.
Here is one json file , I have as:
{
"capabilities": [
{
"applicationName": "ONEPLUS A3003",
"browserName": "ONEPLUS A3003",
"platformName": "ANDROID",
"maxInstances": 1
}
],
"configuration": {
"cleanUpCycle": 2000,
"timeout": 30000,
"proxy": "org.openqa.grid.selenium.proxy.DefaultRemoteProxy",
"host": "127.0.0.1",
"port": 4723,
"maxSession": 1,
"register": true,
"registerCycle": 5000,
"hubPort": 4444,
"hubHost": "your ip address"
}
}
save the above file as jasonFile1.json
Here applicationName will be -> Your Mobile->settings->about phone->Model number
Here hubHost will be your ip address
Here note that you need to go as default cmd location then run below command

appium --nodeconfig C:/richa/jasonfile1.json -p 4723 -bp 4724 -U xxxx

i)Note that you need to provide the absolute pasth of the json file located
ii) port as 4723
iii) Bootstrap port as 4724
iv) -U <device id> for example I have given as xxxx

you can find the device id as -> Your Mobile->settings->status->Serial number
You can also do “adb device” and check this device id.

Then it will create the Selenium Grid with one device.

Now again run the second json file and you will get appium started
Here is second json file:

{
"capabilities": [
{
"applicationName": "Lenovo K50a40",
"browserName": "Lenovo K50a40",
"platformName": "ANDROID",
"maxInstances": 1
}
],
"configuration": {
"cleanUpCycle": 2000,
"timeout": 30000,
"proxy": "org.openqa.grid.selenium.proxy.DefaultRemoteProxy",
"host": "127.0.0.1",
"port": 4730,
"maxSession": 1,
"register": true,
"registerCycle": 5000,
"hubPort": 4444,
"hubHost": "your ip address"
}
}
save the above file as jasonFile2.json

Start the second node with Lenovo mobile.
appium --nodeconfig C:/richa/ jasonFile2.json -p 4730 -bp 4731 -U xxxx

Selenium Grid will look like this



3)Create TestNG parallel execution methods to run you test.

<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE suite SYSTEM "http://testng.org/testng-1.0.dtd">
<suite name="Suite">
<test name="Test1">
<parameter name="deviceName_" value="xxxx"/>
<parameter name="applicationName_" value="ONEPLUS A3003"/>
<parameter name="platformVersion_" value="6.0.1"/>
<!-- <parameter name="URL_" value="localhost:4444/grid/console"/> -->
<classes>
<class name="com.trivago.TestCases.SearchHotelTestCase"/>
</classes>
</test>
<test name="Test2">
<parameter name="deviceName_" value="xxxx"/>
<parameter name="applicationName_" value="Lenovo K50a40"/>
<parameter name="platformVersion_" value="5.0"/>
<classes>
<class name="com.trivago.TestCases.SearchHotelTestCase"/>
</classes>
</test>

</suite>

<parameter name="deviceName_" value="xxxx"/>
Please note value of device name will be the udid you provided earlier.  You can get it by running adb devices on your command prompt.

4)
Now create SearchHotelTestCase.Java as below:
package com.trivago.TestCases;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.concurrent.TimeUnit;

import org.openqa.selenium.remote.DesiredCapabilities;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Parameters;
import org.testng.annotations.Test;

import com.trivago.pages.LocaleSelectionPage;
import com.trivago.pages.SearchLocation;
import com.trivago.pages.SplashScreenPage;

import io.appium.java_client.MobileElement;
import io.appium.java_client.android.AndroidDriver;

public class SearchHotelTestCase {
private  AndroidDriver<MobileElement> driver;


@Parameters({ "deviceName_","platformVersion_","applicationName_" })
@BeforeMethod
public void beforeMethod(String deviceName_,String platformVersion_,String applicationName_) throws MalformedURLException, InterruptedException {

DesiredCapabilities capabilities = new DesiredCapabilities();
capabilities.setCapability("deviceName", deviceName_);
capabilities.setCapability("platformVersion", platformVersion_);
capabilities.setCapability("platformName", "Android");
capabilities.setCapability("applicationName", applicationName_);
capabilities.setCapability("app", "/Users/richa.b.shrivastava/Downloads/com.trivago_2017-04-28.apk");
capabilities.setCapability("appPackage", "com.trivago");
capabilities.setCapability("appActivity", "com.trivago.activities.SplashActivity");

URL url = new URL("http://0.0.0.0:4723/wd/hub/");
System.out.println("before webdriver");
driver = new AndroidDriver(url, capabilities);
System.out.println("after webdriver");
driver.manage().timeouts().implicitlyWait(10, TimeUnit.SECONDS);
Thread.sleep(4000);
}

@Test
public void SearchHotel() {
//Create the objects of Page Class
LocaleSelectionPage localeSelectionPage = new LocaleSelectionPage(driver);
SplashScreenPage splashScreenPage = new SplashScreenPage(driver);
SearchLocation searchLocation = new SearchLocation(driver);

//Call the methods of page class
localeSelectionPage.selectLocale();
splashScreenPage.clickSplashSearchText();
searchLocation.inputSearchText("Paris");
searchLocation.selectSuggestions("Eiffel Tower, Paris");

}


}


