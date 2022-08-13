---
title: "Select Class"
slug: "select-class"
draft: false
images: []
weight: 9900
type: docs
toc: true
---

## Syntax
- **Java**
 - deselectAll()  
 - deselectByIndex(int index) 
 - deselectByValue(java.lang.String value)
 - deselectByVisibleText(java.lang.String text) 
 - getAllSelectedOptions() 
 - getFirstSelectedOption() 
 - getOptions() 
 - isMultiple() 
 - selectByIndex(int index)
 - selectByValue(java.lang.String value) 
 - selectByVisibleText(java.lang.String text) 


## Parameters
| Parameters | Details |
| ------ | ------ |
| index | The option at this index will be selected |
| value | The value to match against |
| text | The visible text to match against |


`Select` class of Selenium WebDriver provides useful methods for interacting with `select` options. User can perform operations on a select dropdown and also de-select operation using the below methods.

In **C#** the Select class is actually `SelectElement`


## Different ways to Select from DropDown list

Below is and HTML page

<!-- language: xml -->

    <html>
    <head>
    <title>Select Example by Index value</title>
    </head>
    <body>
    <select name="Travel"><option value="0" selected> Please select</option>
    <option value="1">Car</option>
    <option value="2">Bike</option>
    <option value="3">Cycle</option>
    <option value="4">Walk</option>
    </select>
    </body>
    </html>

JAVA
==

Select By Index
---------------------------------


To select the option by Index using Java

<!-- language: java -->

    public class selectByIndexExample {
        WebDriver driver;
        @Test
        public void selectSamples()
        {
            driver = new FirefoxDriver();
            driver.get("URL GOES HERE");
            WebElement element=driver.findElement(By.name("Travel")); //This is the 'Select' element locator
            Select sel=new Select(element);
            sel.selectByIndex(1); //This will select the first 'Option' from 'Select' List i.e. Car
        }
    }

  
Select By Value
---------------------------------

<!-- language: java -->

    public class selectByValueExample {
        WebDriver driver;
        @Test
        public void selectSamples()
        {
            driver = new FirefoxDriver();
            driver.get("URL GOES HERE");
            WebElement element=driver.findElement(By.name("Travel")); //This is the 'Select' element locator
            Select sel=new Select(element);
            sel.selectByValue("Bike"); //This will select the 'Option' from 'Select' List which has value as "Bike". 
            //NOTE: This will be case sensitive
        }
    }

Select By Visibility Text
---------------------------------

<!-- language: java -->

    public class selectByVisibilityTextExample {
        WebDriver driver;
        @Test
        public void selectSamples()
        {
            driver = new FirefoxDriver();
            driver.get("URL GOES HERE");
            WebElement element=driver.findElement(By.name("Travel")); //This is the 'Select' element locator
            Select sel=new Select(element);
            sel.selectByVisibleText("Cycle"); //This will select the 'Option' from 'Select' List who's visibility text is "Cycle". 
            //NOTE: This will be case sensitive
        }
    }

C#
==

All examples below are based on the generic `IWebDriver` interface

Select By Index
---------------

<!-- language: c# -->

    IWebElement element=driver.FindElement(By.name("Travel"));
    SelectElement selectElement = new SelectElement(title);
    selectElement.SelectByIndex(0);

Select By Value
---------------

<!-- language: c# -->

    IWebElement element=driver.FindElement(By.name("Travel"));
    SelectElement selectElement = new SelectElement(title);
    selectElement.SelectByIndex("1");
    //NOTE: This will be case sensitive

Select By Text
-------------------------

<!-- language: c# -->

    IWebElement element=driver.FindElement(By.name("Travel"));
    SelectElement selectElement = new SelectElement(title);
    selectElement.SelectByText("Walk");

