---
title: "Page Object Model"
slug: "page-object-model"
draft: false
images: []
weight: 9894
type: docs
toc: true
---

A significant role in automating web sites and web applications involves identifying items on the screen and interacting with them. Items are found in Selenium through the use of locators and the `By` class. These locators and interactions are put inside Page Objects as a best practice to avoid duplicate code and make maintenance easier. It encapsulates `WebElements` and suppose to contain behavior and return info on the page (or part of a page in a web app).


Page object model is a pattern where we write object oriented classes that serve as an interface to a particular view of web page. We use the methods of that page class to perform the required action. Few years back, we were manipulating the HTML code of webpage in test classes directly which was very difficult to maintain along with brittle to changes in UI. 

However, having your code organized in a way of Page Object Pattern provides an application specific API, allowing you to manipulate the page elements without digging around the HTML. The basic Rue of thumb says, your page object should have everything which a human can do on that webpage. For example, to access the text field on a webpage you should a method there to get the text and return string after doing all the modifications.

Few important points you should keep in mind while designing the page objects:
1. Page object usually should not build only for pages, but you should prefer to build it for significant elements of page. For example, a page having multiple tabs to show different charts of your academics should have same number of pages as the count of tabs.

2. Navigating from one view to other should return the instance of page classes.

3. The utility methods which are required to be there only for a specific view or webpage should belong to that page class only.

4. The assertion methods shouldn't be taken care by page classes, you can have methods to return boolean but don't verify them there. For example, to verify user full name you can have method to get boolean value:

        public boolean hasDisplayedUserFullName (String userFullName) {
            return driver.findElement(By.xpath("xpathExpressionUsingFullName")).isDisplayed();
        }

5. If your webpage is based on iframe, prefer to have page classes for iframes too.

Advantages of Page Object Pattern:

1. Clean separation between test code and page code
2. In case of any change in UI of webpage, no need to change your code at multiple places. Change only in page classes.
3. No scattered element locators.
4. Makes code easier to understand
5. Easy maintenance 

 





## Introduction (Using Java)
An example to perform login test based on Page object pattern:
    
    import org.openqa.selenium.WebDriver;
    import org.openqa.selenium.WebElement;
    import org.openqa.selenium.support.FindBy;
    import org.openqa.selenium.support.PageFactory;

    /**
     * Class which models the view of Sign-In page
     */
    public class SignInPage {
        
        @FindBy(id="username")
        private usernameInput;

        @FindBy(id="password")
        private passwordInput;

        @FindBy(id="signin")
        private signInButton;

        private WebDriver driver;

        public SignInPage(WebDriver driver) {
            this.driver = driver;
        }
        
        /**
         * Method to perform login
         */
        public HomePage performLogin(String username, String password) {
            usernameInput.sendKeys(username);
            passwordInput.sendKeys(password);
            signInButton.click();
            return PageFactory.initElements(driver, HomePage.class);
        }
    }
    

    import org.openqa.selenium.WebDriver;
    import org.openqa.selenium.WebElement;
    import org.openqa.selenium.support.FindBy;
    import org.openqa.selenium.support.PageFactory;
    /**
     * Class which models the view of home page
     */
    public class HomePage {
        @FindBy(id="logout")
        private logoutLink;

        private WebDriver driver;

        public HomePage(WebDriver driver) {
            this.driver = driver;
        }
        
        /**
         * Method to log out
         */
        public SignInPage logout() {
            logoutLink.click();
            wait.ForPageToLoad();
            return PageFactory.initElements(driver, SignInPage.class);
        }
    }
    
    /**
     * Login test class
     */
    public class LoginTest {
        public void testLogin() {
            SignInPage signInPage = new SignInPage(driver);
            HomePage homePage = signInPage.login(username, password);
            signInPage = homePage.logout();
        }
    }

## C#
Page Objects should contain behavior, return info for assertions and possibly a method for page ready state method on initialization. Selenium supports Page Objects using annotations. In C# it's as follows:

    using OpenQA.Selenium;
    using OpenQA.Selenium.Support.PageObjects;
    using OpenQA.Selenium.Support.UI;
    using System;
    using System.Collections.Generic;
    
    public class WikipediaHomePage
    {
        private IWebDriver driver;
        private int timeout = 10;
        private By pageLoadedElement = By.ClassName("central-featured-logo");

        [FindsBy(How = How.Id, Using = "searchInput")]
        [CacheLookup]
        private IWebElement searchInput;

        [FindsBy(How = How.CssSelector, Using = ".pure-button.pure-button-primary-progressive")]
        [CacheLookup]
        private IWebElement searchButton;

        public ResultsPage Search(string query) 
        {
            searchInput.SendKeys(query);
            searchButton.Click();
        }

        public WikipediaHomePage VerifyPageLoaded() 
        {
            new WebDriverWait(driver, TimeSpan.FromSeconds(timeout)).Until<bool>((drv) =>  return drv.ExpectedConditions.ElementExists(pageLoadedElement));

            return this;
        }
    }

notes: 

 - `CacheLookup` saves the element in the cache and saves returning a new element each call. This improves performance but isn't good for dynamically changing elements.
 - `searchButton` has 2 class names and no ID, that's why I can't use class name or id.
 - I verified that my locators will return me the element I want using Developer Tools (for Chrome), in other browsers you can use FireBug or similar.
 - `Search()` method returns another page object (`ResultsPage`) as the search click redirects you to another page.



## Best Practices Page Object Model
- Create separate files for header and footer(as they are common for all the pages and it does not make sense to make them a part of a single page)
- Keep common elements(Like Search/Back/Next etc) in separate file(The idea is to remove any kind of duplication and keeping the segregation logical)
- For Driver, its a good idea to create a separate Driver class and keep Driver as static so that it can be accessed across all pages! (I have all my webpages extend DriverClass)
- The functions used in PageObjects are broken down into smallest possible chunk keeping in mind the frequency and the way in which they will be called(The way you have done for login- although login can be broken down into enterUsername and enterPassword functions but still keeping it as Login function is more logical because in majority of the cases, the Login function would be called rather than separate calls to enterUsername and enterPassword functions)
- Using PageObjects itself segregates Test script from the elementLocators
- Have utility functions in separate utils folder(like DateUtil, excelUtils etc)
- Have configurations in separate conf folder(like setting the environment on which the tests need to be run, configuring the output and input folders)
- Incorporate screenCapture on failure
- Have a static wait variable in the DriverClass with some implicit wait time as you have done
Always try to have conditional waits rather than static waits like: wait.until(ExpectedConditions). This ensures that the wait is not slowing down the execution unnecessarily.

