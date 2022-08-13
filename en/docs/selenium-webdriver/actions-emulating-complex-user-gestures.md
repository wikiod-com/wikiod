---
title: "Actions (Emulating complex user gestures)"
slug: "actions-emulating-complex-user-gestures"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

The `Actions` class gives us a way of emulating precisely how a user would interact with a web page/elements. Using an instance of this class you can describe a series of actions, such as clicking, double-clicking, dragging, pressing keys, etc.

Once these actions are described, in order to carry the actions out, you must call must build the actions (`.Build()`) and then instruct them to be performed (`.Perform()`).

So we must describe, build, perform. The examples below will expand upon this.

## Syntax
 - dragAndDrop(WebElement source, WebElement target)
 - dragAndDropBy(WebElement source, int xOffset, int yOffset)
 - perform()

## Parameters
| Parameters | Details |
| ------ | ------ |
| source | Element to emulate button down at. |
| target | Element to move to and release the mouse at.
| xOffset | x co-ordinate to move to. |
| yOffset | y co-ordinate to move to. |

This section contains information of Actions class of Selenium WebDriver. The Actions class provides you convenient methods to perform complex user gestures like drag and drop, hold and click etc. 

## Drag and Drop
C#
==
<!-- language: c# -->

    using OpenQA.Selenium;
    using OpenQA.Selenium.Firefox;
    using OpenQA.Selenium.Interactions;

    namespace WebDriverActions
    {
        class WebDriverTest
        {
            static void Main()
            {
                IWebDriver driver = new FirefoxDriver();

                driver.Navigate().GoToUrl("");
                IWebElement source = driver.FindElement(By.CssSelector(""));
                IWebElement target = driver.FindElement(By.CssSelector(""));
                Actions action = new Actions(driver);
                action.DragAndDrop(source, target).Perform();
            }
        }
    }

The above will find an `IWebElement`, `source`, and drag it to, and drop it into the second `IWebElement`, `target`.

Java
====

Drag and Drop using source and target webelement.

A convenience method that performs click-and-hold at the location of the source element, moves to the location of the target element, then releases the mouse.

<!-- language: java -->

    import org.openqa.selenium.By;
    import org.openqa.selenium.WebDriver;
    import org.openqa.selenium.WebElement;
    import org.openqa.selenium.firefox.FirefoxDriver;
    import org.openqa.selenium.interactions.Actions;
    
    /**
     * Drag and Drop test using source and target webelement
     */
    public class DragAndDropClass {
        public static void main(String[] args) {
            WebDriver driver = new FirefoxDriver();
            driver.get("");
            WebElement source = driver.findElement(By.cssSelector(""));
            WebElement target = driver.findElement(By.cssSelector(""));
            Actions action = new Actions(driver);
            action.build();
            action.dragAndDrop(source, target).perform();
        }
    }

Drag an element and drop it at a given offset.

A convenience method that performs click-and-hold at the location of the source element, moves by a given offset (x and y, both integers), then releases the mouse.

<!-- language: java -->

    WebElement source = driver.findElement(By.cssSelector(""));
    Actions action = new Actions(driver);
    action.build()
    action.dragAndDropBy(source, x, y).perform(); // x and y are integers value

## Move to Element
C#
==

Suppose you want to test that when you hover over an element, a drop list is displayed. You may want to check the contents of this list, or perhaps select an option from the list.

First create an Action, to hover over the element *(e.g. my element has link text "Admin")*:

<!-- language: c# -->

    Actions mouseHover = new Actions(driver);
    mouseHover.MoveToElement(driver.FindElement(By.LinkText("Admin"))).Perform();

In the example above:

-    You have created the action `mouseHover`
-    You have told `driver` to move to a specific element
-    From here you can perform other `Actions` with the `mouseHover` object or continue testing with your `driver` object

**This approach is of particular use when clicking on an element performs a different function than hovering over it.**

A full example:

<!-- language: c# -->

    Actions mouseHover = new Actions(driver);
    mouseHover.MoveToElement(driver.FindElement(By.LinkText("Admin"))).Perform();
    
    Assert.IsTrue(driver.FindElement(By.LinkText("Edit Record")).Displayed);
    Assert.IsTrue(driver.FindElement(By.LinkText("Delete Record")).Displayed);


