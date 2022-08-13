---
title: "Interaction With Web Element"
slug: "interaction-with-web-element"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

## C#
Clearing the Content of element(Generally Text Box)

<!-- language: c# -->

    interactionWebElement.Clear();  
Entering data to element (Generally Text Box)
    
<!-- language: c# -->

    interactionWebElement.SendKeys("Text");
Storing the value of the element.
    
<!-- language: c# -->

    string valueinTextBox = interactionWebElement.GetAttribute("value");
Storing Text of element.

<!-- language: c# -->

    string textOfElement = interactionWebElement.Text; 
Clicking on an Element

<!-- language: c# -->

    interactionWebElement.Click();
Submitting a Form

<!-- language: c# -->

    interactionWebElement.Submit(); 
 Identifing the visibility of an element on the page

<!-- language: c# -->

    bool isDisplayed=interactionWebElement.Displayed; 
 Identifing the state of an element on the page

<!-- language: c# -->

    bool isEnabled = interactionWebElement.Enabled; 

    bool isSelected=interactionWebElement.Selected; 

Locating child element of interactionWebElement

<!-- language: c# -->

    IWebElement childElement = interactionWebElement.FindElement(By.Id("childElementId")); 

Locating child elements of interactionWebElement
    
<!-- language: c# -->

    Ilist<IWebElement> childElements = interactionWebElement.FindElements(By.TagName("childElementsTagName"));
            
            

## Java
Clearing the content of a web element: (note - when simulating user actions in tests, it's better to send backspace, see next action)

    interactionWebElement.clear();

Entering data - simulating sending keystrokes:

    interactionWebElement.sendKeys("Text");
    interactionWebElement.sendKeys(Keys.CONTROL + "c"); // copy to clipboard.

Getting the value of an element's attribute:

    interactionWebElement.getAttribute("value");
    interactionWebElement.getAttribute("style");

Getting element's text:

    String elementsText = interactionWebElement.getText();

Selecting from dropdown:

    Select dropDown = new Select(webElement);
    dropDown.selectByValue(value);

Self explanatory:

    interactionWebElement.click();
    interactionWebElement.submit(); //for forms
    interactionWebElement.isDisplayed();
    interactionWebElement.isEnabled(); // for exampale - is clickable.
    interactionWebElement.isSelected(); // for radio buttons.

----------
**Actions using** `org.openqa.selenium.interactions.Actions`:


Drag & Drop:

    Action dragAndDrop = builder.clickAndHold(someElement)
       .moveToElement(otherElement)
       .release(otherElement)
       .build();

    dragAndDrop.perform();

Select multiple:

    Action selectMultiple = builder.keyDown(Keys.CONTROL)
       .click(someElement)
       .click(someOtherElement)
       .keyUp(Keys.CONTROL);

    dragAndDrop.perform();

Self explanatory (using builder):

    builder.doubleClick(webElement).perform();
    builder.moveToElement(webElement).perform(); //hovering

See [here][1] for more examples of advanced actions and a complete list.


----------
Using Javascript:

    // Scroll to view element:
    ((JavascriptExecutor) driver).executeJavaScript("arguments[0].scrollIntoView(true);", webElement);
    


    


  [1]: https://github.com/SeleniumHQ/selenium/wiki/Advanced-User-Interactions

