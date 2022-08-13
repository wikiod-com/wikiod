---
title: "Scrolling"
slug: "scrolling"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

This Topic will provide several approaches of how to perform scrolling with `selenium`

## Different Scrolling using java with different ways
> Below give solution can be also use in another supported programming languages with some syntax changes

---
1. To do **Scroll down** page/section/division in webpage while there is custom scroll bar(Not browser scroll). [Click Here For demo][1] and check scroll bar has its independent element. 
>In below given code pass your scroll bar element and require scroll points.

        public static boolean scroll_Page(WebElement webelement, int scrollPoints)
        {
        try
        {
            System.out.println("---------------- Started - scroll_Page ----------------");
            driver = ExecutionSetup.getDriver();
            dragger = new Actions(driver);

            // drag downwards
            int numberOfPixelsToDragTheScrollbarDown = 10;
            for (int i = 10; i < scrollPoints; i = i + numberOfPixelsToDragTheScrollbarDown)
            {
                dragger.moveToElement(webelement).clickAndHold().moveByOffset(0, numberOfPixelsToDragTheScrollbarDown).release(webelement).build().perform();
            }
            Thread.sleep(500);
            System.out.println("---------------- Ending - scroll_Page ----------------");
            return true;
        }
        catch (Exception e)
        {
            System.out.println("---------------- scroll is unsucessfully done in scroll_Page ----------------");
            e.printStackTrace();
            return false;
        }
      }
---
2. To do **Scroll Up** page/section/division in webpage while there is custom scroll bar(Not browser scroll). [Click Here For demo][1] and check scroll bar has its independent element. 

>In below given code pass your scroll bar element and require scroll points.
    
   

    public static boolean scroll_Page_Up(WebElement webelement, int scrollPoints)
    {
        try
        {
            System.out.println("---------------- Started - scroll_Page_Up ----------------");
            driver = ExecutionSetup.getDriver();
            dragger = new Actions(driver);
            // drag upwards
            int numberOfPixelsToDragTheScrollbarUp = -10;
            for (int i = scrollPoints; i > 10; i = i + numberOfPixelsToDragTheScrollbarUp)
            {
                dragger.moveToElement(webelement).clickAndHold().moveByOffset(0, numberOfPixelsToDragTheScrollbarUp).release(webelement).build().perform();
            }
            System.out.println("---------------- Ending - scroll_Page_Up ----------------");
            return true;
        }
        catch (Exception e)
        {
            System.out.println("---------------- scroll is unsucessfully done in scroll_Page_Up----------------");
            e.printStackTrace();
            return false;
        }
    }
---
3. To do scroll down when **multiple browser scroll** (In-Built browser) and you want to scroll down with **Page Down Key**. [Click Here for demo][2]

> In below given code pass your scroll area element like `<div>` and require page down key.

     public static boolean pageDown_New(WebElement webeScrollArea, int iLoopCount)
     {
        try
        {
            System.out.println("---------------- Started - pageDown_New ----------------");
            driver = ExecutionSetup.getDriver();
            dragger = new Actions(driver);

            for (int i = 0; i <= iLoopCount; i++)
            {
                dragger.moveToElement(webeScrollArea).click().sendKeys(Keys.PAGE_DOWN).build().perform();
            }
            System.out.println"---------------- Ending - pageDown_New ----------------");
            return true;
        }
        catch (Exception e)
        {
            System.out.println("---------------- Not able to do page down ----------------");
            return false;
        }
    }
---
4. To do scroll UP when **multiple browser scroll** (In-Built browser) and you want to scroll Up with **Page UP Key**. [Click Here for demo][2]

> In below given code pass your scroll area element like `<div>` and require page up key.

    public static boolean pageUp_New(WebElement webeScrollArea, int iLoopCount)
    {
        try
        {
            System.out.println("---------------- Started - pageUp_New ----------------");
            driver = ExecutionSetup.getDriver();
            dragger = new Actions(driver);

            for (int i = 0; i <= iLoopCount; i++)
            {
                dragger.moveToElement(webeScrollArea).click().sendKeys(Keys.PAGE_UP).build().perform();
            }
            System.out.println("---------------- Ending - pageUp_New ----------------");
            return true;
        }
        catch (Exception e)
        {
            System.out.println("---------------- Not able to do page up ----------------");
            return false;
        }
    }
---
5. To do scroll Down when **multiple browser scroll** (In-Built browser) and you want to scroll down with **Only Down arrow Key**. [Click Here for demo][2]

> In below given code pass your scroll area element like `<div>` and require down key.

    public static boolean scrollDown_Keys(WebElement webeScrollArea, int iLoopCount)
    {
        try
        {
            System.out.println("---------------- Started - scrollDown_Keys ----------------");
            driver = ExecutionSetup.getDriver();
            dragger = new Actions(driver);

            for (int i = 0; i <= iLoopCount; i++)
            {
                dragger.moveToElement(webeScrollArea).click().sendKeys(Keys.DOWN).build().perform();
            }
            System.out.println("---------------- Ending - scrollDown_Keys ----------------");
            return true;
        }
        catch (Exception e)
        {
            System.out.println("---------------- Not able to do scroll down with keys----------------");
            return false;
        }
    }
---
6. To do scroll Up when **multiple browser scroll** (In-Built browser) and you want to scroll Up with **Only Up arrow Key**. [Click Here for demo][2]

> In below given code pass your scroll area element like `<div>` and require up key.

    public static boolean scrollUp_Keys(WebElement webeScrollArea, int iLoopCount)
    {
        try
        {
            System.out.println("---------------- Started - scrollUp_Keys ----------------");
            driver = ExecutionSetup.getDriver();
            dragger = new Actions(driver);

            for (int i = 0; i <= iLoopCount; i++)
            {
                dragger.moveToElement(webeScrollArea).click().sendKeys(Keys.UP).build().perform();
            }
            System.out.println("---------------- Ending - scrollUp_Keys ----------------");
            return true;
        }
        catch (Exception e)
        {
            System.out.println("---------------- Not able to do scroll up with keys----------------");
            return false;
        }
    }
---
7. To do scroll Up/Down when **browser scroll** (In-Built browser) and you want to scroll Up/Down with **Only fixed point**. [Click Here for demo][2]

> In below given code pass your scroll point. Positive means down and negative means scroll up.

    public static boolean scroll_without_WebE(int scrollPoint)
    {
        JavascriptExecutor jse;
        try
        {
            System.out.println("---------------- Started - scroll_without_WebE ----------------");

            driver = ExecutionSetup.getDriver();
            jse = (JavascriptExecutor) driver;
            jse.executeScript("window.scrollBy(0," + scrollPoint + ")", "");

            System.out.println("---------------- Ending - scroll_without_WebE ----------------");
            return true;
        }
        catch (Exception e)
        {
            System.out.println("---------------- scroll is unsucessful in scroll_without_WebE ----------------");
            e.printStackTrace();
            return false;
        }
    }
---
 8. To do scroll Up/Down when **browser scroll** (In-Built browser) and you want to scroll Up/Down to **For make element in visible area or dynamic scroll**. [Click Here for demo][2]

> In below given code pass your element. 

    public static boolean scroll_to_WebE(WebElement webe)
    {
        try
        {
            System.out.println("---------------- Started - scroll_to_WebE ----------------");

            driver = ExecutionSetup.getDriver();
            ((JavascriptExecutor) driver).executeScript("arguments[0].scrollIntoView();", webe);

            System.out.println("---------------- Ending - scroll_to_WebE ----------------");
            return true;
        }
        catch (Exception e)
        {
            System.out.println("---------------- scroll is unsucessful in scroll_to_WebE ----------------");
            e.printStackTrace();
            return false;
        }
    }

---
***Note : Please verify your case and use methods. If any case is missing then let me know.***


  [1]: http://manos.malihu.gr/repository/custom-scrollbar/demo/examples/complete_examples.html
  [2]: http://www.apptools.com/examples/scroller.php

## Scrolling using Python
 *1. Scrolling to target element ("BROWSE TEMPLATES" button at the bottom of page) with `Actions`*


    from selenium import webdriver
    from selenium.webdriver.common.action_chains import ActionChains
    
    driver = webdriver.Chrome()
    driver.get('http://www.w3schools.com/')
    target = driver.find_element_by_link_text('BROWSE TEMPLATES')
    actions = ActionChains(driver)
    actions.move_to_element(target)
    actions.perform()

 *2. Scrolling to target element ("BROWSE TEMPLATES" button at the bottom of page) with `JavaScript`*


    from selenium import webdriver

    driver = webdriver.Chrome()
    driver.get('http://www.w3schools.com/')
    target = driver.find_element_by_link_text('BROWSE TEMPLATES')
    driver.execute_script('arguments[0].scrollIntoView(true);', target)

*3. Scrolling to target element ("BROWSE TEMPLATES" button at the bottom of page) with built-in method*


    from selenium import webdriver
    
        driver = webdriver.Chrome()
        driver.get('http://www.w3schools.com/')
        target = driver.find_element_by_link_text('BROWSE TEMPLATES')
        target.location_once_scrolled_into_view

*Note that `location_once_scrolled_into_view` also returns `x`, `y` coordinates of element after scrolling*

*4. Scrolling to the bottom of page with `Keys`*

    from selenium import webdriver
    from selenium.webdriver.common.keys import Keys 

    driver = webdriver.Chrome()
    driver.get('http://www.w3schools.com/')
    driver.find_element_by_tag_name('body').send_keys(Keys.END) # Use send_keys(Keys.HOME) to scroll up to the top of page

Note that `send_keys(Keys.DOWN)`/`send_keys(Keys.UP)` and `send_keys(Keys.PAGE_DOWN)`/`send_keys(Keys.PAGE_UP)` also could be used for scrolling



