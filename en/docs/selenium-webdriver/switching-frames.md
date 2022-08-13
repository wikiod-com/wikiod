---
title: "Switching Frames"
slug: "switching-frames"
draft: false
images: []
weight: 9766
type: docs
toc: true
---

## Syntax
 - **Java**
 - driver.switchTo().frame(String name); 
 - driver.switchTo().frame(String id);
 - driver.switchTo().frame(int index);
 - driver.switchTo().frame(WebElement frameElement); 
 - driver.switchTo().defaultContent(); 
 - **C#**
 - driver.SwitchTo().Frame(int frameIndex); 
 - driver.SwitchTo().Frame(IWebElement frameElement); 
 - driver.SwitchTo().Frame(string frameName); 
 - driver.SwitchTo().DefaultContent(); 
 - **Python**
 - driver.switch_to_frame(nameOrId)
 - driver.switch_to.frame(nameOrId)
 - driver.switch_to_frame(index)
 - driver.switch_to.frame(index)
 - driver.switch_to_frame(frameElement)
 - driver.switch_to.frame(frameElement)
 - driver.switch_to_default_content()
 - driver.switch_to.default_content()
 - **JavaScript**
 - driver.switchTo().frame(nameOrId)
 - driver.switchTo().frame(index)
 - driver.switchTo().defaultContent()

## Parameters
| parameter | details |
| ------ | ------ |
| nameOrId   | Select a frame by its name of id.   |
|index| Select a frame by its zero-based index.|
|frameElement|Select a frame using its previously located WebElement|

## To switch to a frame using Java
For an instance, if the html source code of an html view or element is wrapped by an iframe like this:

<!-- language: xml -->

    <iframe src="../images/eightball.gif" name="imgboxName" id="imgboxId">
       <p>iframes example</p>
       <a href="../images/redball.gif" target="imgbox">Red Ball</a>
    </iframe><br />

... then to perform any action on the web-elements of the iframe, you have to switch the focus to the iframe first, using any one of the below methods:

**Using frame Id** (should be used only if you know the id of the iframe).

<!-- language: java -->

    driver.switchTo().frame("imgboxId"); //imgboxId - Id of the frame

**Using frame name** (should be used only if you know the name of the iframe).

<!-- language: java -->

    driver.switchTo().frame("imgboxName"); //imgboxName - Name of the frame

**Using frame index** (should be used only if you do not have the id or name of the iframe), where the index defines the position of the iframe amongst all frames.

<!-- language: java -->

    driver.switchTo().frame(0); //0 - Index of the frame

Note: If you have three frames in the page, then the first frame will be at index 0, the second at index 1, and the third at index 2.

**Using previously located webelement** (should be used only if you have already located the frame and have returned it as a `WebElement`).

<!-- language: java -->

    driver.switchTo().frame(frameElement); //frameElement - webelement that is the frame


So, to click on the `Red Ball` anchor:
    
<!-- language: java -->

    driver.switchTo().frame("imgboxId");
    driver.findElement(By.linkText("Red Ball")).Click();

## Switch to a frame using C#
 **1. Switch to a frame by Index.**

Here we are switching to index 1. Index refers to the order of frames on the page. This should be used as a last resort, as frame id or names are much more reliable.

<!-- language: c# -->

    driver.SwitchTo().Frame(1);

**2. Switch to a frame by Name**

<!-- language: c# -->

    driver.SwitchTo().Frame("Name_Of_Frame");

**3. Switch to a frame by Title, Id, or others by passing IWebElement**

If you want to switch to a frame by id or title you have to pass in a web element as a parameter:

<!-- language: c# -->

    driver.SwitchTo().Frame(driver.FindElement(By.Id("ID_OF_FRAME")));
    driver.SwitchTo().Frame(driver.FindElement(By.CssSelector("iframe[title='Title_of_Frame']")));

Also note that if your frame takes a few seconds to come up, you may have to use a [wait][1]:

<!-- language: c# -->

    new WebDriverWait(driver, TimeSpan.FromSeconds(10))
        .Until(ExpectedConditions.ElementIsVisible(By.Id("Id_Of_Frame")));

**Get out of a frame:**

<!-- language: c# -->

    driver.SwitchTo().DefaultContent()

  [1]: https://www.wikiod.com/selenium-webdriver/wait

## To get out of a frame using Java
To switch focus to either main document or first frame of the page. You have to use below syntax.

<!-- language: java -->

    driver.switchTo().defaultContent();


## Wait for your frames to load
In quite a few cases your frame might not show up immediately and you probably have to wait till it is loaded to switch. Or else you will have NoSuchFrameException.

So its always a good choice to wait before you switch. Following is a ideal way to wait till a frame is loaded.

    try{
            new WebDriverWait(driver, 300).ignoring(StaleElementReferenceException.class).
                            ignoring(WebDriverException.class).
                            until(ExpectedConditions.visibilityOf((driver.findElement(By.id("cpmInteractionDivFrame"))));}
        catch{
// throws exception only if your frame is not visible with in your wait time 300 seconds
            }



## To get out of a frame using C#
To switch focus to either main document or first frame of the page. You have to use below syntax.

webDriver.SwitchTo().DefaultContent();

## Switch among Child Frames of a Parent Frame.
Consider you have a Parent Frame(Frame-Parent). and 2 child frames(Frame_Son,Frame_Daughter). Lets see various conditions and how to handle.

1.From Parent to Son or Daughter:
    
     driver.switchTo().frame("Frame_Son");
     driver.switchTo().frame("Frame_Daughter");
2.From Son to Parent: If parent is default frame, switch to default frame, else from  default frame switch to parent frame. But you cannot switch directly from son to Parent.
  
    driver.switchTo().defaultContent(); 
    driver.switchTo().frame("Frame_Parent");

3.From Son to Daughter: If your sister does some mistake don't yell at her, just reach out to your Parent. Similarly, you give control to parent frame and then to daughter frame. 
  
    driver.switchTo().defaultContent(); 
    driver.switchTo().frame("Frame_Parent");
    driver.switchTo().frame("Frame_Daughter");
  



