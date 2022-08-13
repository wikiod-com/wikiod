---
title: "Basic Selenium Webdriver Program"
slug: "basic-selenium-webdriver-program"
draft: false
images: []
weight: 9885
type: docs
toc: true
---

This topic aims to show the basic web driver program in selenium supported languages like C#, Groovy, Java, Perl, PHP, Python and Ruby.

Journey includes opening browser driver --> Google Page --> shutdown the browser

## C#
<!-- language: c# -->

    using OpenQA.Selenium;
    using OpenQA.Selenium.Chrome;

    namespace BasicWebdriver
    {
        class WebDriverTest
        {
            static void Main()
            {
                using (var driver = new ChromeDriver())
                {
                    driver.Navigate().GoToUrl("http://www.google.com");
                }
            }
        }
    }





The above 'program' will navigate to the Google homepage, and then close down the browser after fully loading the page.

<!-- language: c# -->

    using (var driver = new ChromeDriver())

This instantiates a new WebDriver object using the `IWebdriver` interface and creates a new browser window instance. In this example we are using `ChromeDriver` (though this could be replaced by the appropriate driver for whichever browser we wanted to use). We are wrapping this with a `using` statement, because `IWebDriver` implements `IDisposable`, thus not needing to explicitly type in `driver.Quit();`.

In case you haven't downloaded your WebDriver using [NuGet][1], then you will need to pass an argument in the form of a path to the directory where the driver itself "chromedriver.exe" is located.


----------

Navigating
----------

<!-- language: c# -->

    driver.Navigate().GoToUrl("http://www.google.com");

and

<!-- language: c# -->

    driver.Url = "http://www.google.com";

Both of these lines do the same thing. They instruct the driver to navigate to a specific URL, and to wait until the page is loaded before it moves to the next statement. 

There are other methods tied to navigation such as `Back()`, `Forward()` or `Refresh()`.


----------

After that, the `using` block safely quits, and disposes the object.

  [1]: https://www.wikiod.com/selenium-webdriver/getting-started-with-selenium-webdriver#Installation or Setup

## Java

The code below is all about 3 steps.
1. Opening a chrome browser 
2. Opening google page 
3. Shutdown the browser

<!-- language: java -->

    import org.openqa.selenium;
    import org.openqa.selenium.chrome;

    public class WebDriverTest {
        public static void main(String args[]) {
            System.setProperty("webdriver.chrome.driver", "C:\\path\\to\\chromedriver.exe");
            WebDriver driver = new ChromeDriver();

            driver.get("http://www.google.com");
            driver.quit();
        }
    }

The above 'program' will navigate to the Google homepage, and then close down the browser before completing.

<!-- language: java -->

    System.setProperty("webdriver.chrome.driver", "C:\\path\\to\\chromedriver.exe");
    WebDriver driver = new ChromeDriver();

The first line tells the system where to find the `ChromeDriver` (chromedriver.exe) executable. We then create our driver object by calling the `ChromeDriver()` constructor, again we could be calling our constructor here for any browser/platform.

<!-- language: java -->

    driver.get("http://www.google.com");

This tells our driver to navigate to the specified url: http://www.google.com. The Java WebDriver API provides the `get()` method directly on the WebDriver interface, though further navigation methods can be found via the `navigate()` method, e.g. `driver.navigate.back()`.

Once the page has finished loading we immediately call:

<!-- language: java -->

    driver.quit();

This tells the driver to close all open browser windows and dispose of the driver object, as we have no other code after this call this effectively ends the program.

<!-- language: java -->

    driver.close();

Is an instruction (not shown here) to the driver to close only the active window, in this instance as we only have a single window the instructions would cause identical results to calling `quit()`.

## Python
<!-- language: python -->
    from selenium import webdriver
    from selenium.webdriver.common.keys import Keys
 
    def set_up_driver():
        path_to_chrome_driver = 'chromedriver'
        return webdriver.Chrome(executable_path=path_to_chrome_driver)
    
    def get_google():
        driver = set_up_driver()
        driver.get('http://www.google.com')
        tear_down(driver)
    
    def tear_down(driver):
        driver.quit()
    
    if '__main__' == __name__:
        get_google()

The above 'program' will navigate to the Google homepage, and then close down the browser before completing.

<!-- language: python -->
    if '__main__' == __name__:
        get_google()

First we have our main function, our point of entry into the program, that calls `get_google()`.

<!-- language: python -->
    def get_google():
        driver = set_up_driver()

`get_google()` then starts by creating our `driver` instance via `set_up_driver()`:

<!-- language: python -->
    def set_up_driver():
        path_to_chrome_driver = 'chromedriver'
        return webdriver.Chrome(executable_path=path_to_chrome_driver)

Whereby we state where `chromedriver.exe` is located, and instantiate our driver object with this path. The remainder of `get_google()` navigates to Google:

<!-- language: python -->
    driver.get('http://www.google.com')

And then calls `tear_down()` passing the driver object:

<!-- language: python -->
    tear_down(driver)

`tear_down()` simply contains one line to shut down our driver object:

<!-- language: python -->
    driver.quit()

This tells the driver to close all open browser windows and dispose of the browser object, as we have no other code after this call this effectively ends the program.

## Java - Best practise with page classes
Usecase : Login to FB account

    import org.openqa.selenium.WebDriver;
    import org.openqa.selenium.firefox.FirefoxDriver;
    import org.testng.annotations.BeforeClass;
    import org.testng.annotations.Test;

    public class FaceBookLoginTest {
        private static WebDriver driver;
        HomePage homePage;
        LoginPage loginPage;
        @BeforeClass
        public void openFBPage(){
            driver = new FirefoxDriver();
            driver.get("https://www.facebook.com/");
            loginPage = new LoginPage(driver);
        }
        @Test
        public void loginToFB(){
           loginPage.enterUserName("username");
           loginPage.enterPassword("password");
           homePage = loginPage.clickLogin();
            System.out.println(homePage.getUserName());
        }

    }

Page classes : Login Page & Home Page
Login page class :

    import org.openqa.selenium.WebDriver;
    import org.openqa.selenium.WebElement;
    import org.openqa.selenium.support.FindBy;

    public class LoginPage {

        WebDriver driver;
        public LoginPage(WebDriver driver){
            this.driver = driver;
        }
        @FindBy(id="email")
        private WebElement loginTextBox;

        @FindBy(id="pass")
        private WebElement passwordTextBox;

        @FindBy(xpath = ".//input[@data-testid='royal_login_button']")
        private WebElement loginBtn;

        public void enterUserName(String userName){
            if(loginTextBox.isDisplayed()) {
                loginTextBox.clear();
                loginTextBox.sendKeys(userName);
            }
            else{
                System.out.println("Element is not loaded");
            }
        }
        public void enterPassword(String password){
            if(passwordTextBox.isDisplayed()) {
                passwordTextBox.clear();
                passwordTextBox.sendKeys(password);
            }
            else{
                System.out.println("Element is not loaded");
            }
        }
        public HomePage clickLogin(){
            if(loginBtn.isDisplayed()) {
                loginBtn.click();
            }
            return new HomePage(driver);
        }
    }
Home page class:

    import org.openqa.selenium.WebDriver;
    import org.openqa.selenium.WebElement;
    import org.openqa.selenium.support.FindBy;

    public class HomePage {

        WebDriver driver;
        public HomePage(WebDriver driver){
            this.driver = driver;
        }

        @FindBy(xpath=".//a[@data-testid='blue_bar_profile_link']/span")
        private WebElement userName;

        public String getUserName(){
            if(userName.isDisplayed()) {
                return userName.getText();
            }
            else {
                return "Username is not present";
            }
        }

    }


