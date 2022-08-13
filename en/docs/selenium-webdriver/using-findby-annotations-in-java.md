---
title: "Using @FindBy annotations in Java"
slug: "using-findby-annotations-in-java"
draft: false
images: []
weight: 9613
type: docs
toc: true
---

## Syntax
 - CLASS_NAME: @FindBy(className = "classname")
 - CSS: @FindBy(css = "css")
 - ID: @FindBy(id = "id")
 - ID_OR_NAME: @FindBy(how = How.ID_OR_NAME, using  ="idOrName")
 - LINK_TEXT: @FindBy(linkText= "text")
 - NAME: @FindBy(name= "name")
 - PARTIAL_LINK_TEXT: @FindBy(partialLinkText= "text")
 - TAG_NAME: @FindBy(tagName="tagname")
 - XPATH: @FindBy(xpath="xpath")

Note that there are two ways of using the annotation. Examples:

<!-- language: lang-java -->
    @FindBy(id = "id")

and

<!-- language: lang-java -->
    @FindBy(how = How.ID, using  ="id")

are equal and both look for element by its ID. In case of `ID_OR_NAME` you can only use

<!-- language: lang-java -->
    @FindBy(how = How.ID_OR_NAME, using ="idOrName")
`PageFactory.initElements()` must be used after page object instantiation to find elements marked with `@FindBy` annotation.

## Basic example
Assume that we are using [Page object model][1].
Page Object class:

<!-- language: lang-java -->

    import org.openqa.selenium.WebDriver;
    import org.openqa.selenium.WebElement;
    import org.openqa.selenium.firefox.FirefoxDriver;
    import org.openqa.selenium.support.FindBy;
    import org.openqa.selenium.support.PageFactory;
    
    public class LoginPage {
        
        @FindBy(id="loginInput") //method used to find WebElement, in that case Id
        WebElement loginTextbox;
        
        @FindBy(id="passwordInput")
        WebElement passwordTextBox;
        
        //xpath example:
        @FindBy(xpath="//form[@id='loginForm']/button(contains(., 'Login')") 
        WebElement loginButton;
        
        public void login(String username, String password){
            // login method prepared according to Page Object Pattern
            loginTextbox.sendKeys(username);
            passwordTextBox.sendKeys(password);
            loginButton.click();
            // because WebElements were declared with @FindBy, we can use them without
            // driver.find() method
        }
    }
     
And Tests class:

<!-- language: lang-java -->

    class Tests{
        public static void main(String[] args) {
            WebDriver driver = new FirefoxDriver();
            LoginPage loginPage = new LoginPage();

            //PageFactory is used to find elements with @FindBy specified
            PageFactory.initElements(driver, loginPage);
            loginPage.login("user", "pass");
        }
    }

There are few methods to find WebElements using @FindBy - check Syntax section.

  [1]: https://www.wikiod.com/selenium-webdriver/page-object-model


