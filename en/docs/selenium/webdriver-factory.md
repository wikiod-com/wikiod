---
title: "WebDriver Factory"
slug: "webdriver-factory"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

## WebDriver Factory C#
    using OpenQA.Selenium;
    using OpenQA.Selenium.Chrome;
    using OpenQA.Selenium.Firefox;
    using OpenQA.Selenium.IE;
    
    
    /// <summary>
    /// Factory for creating WebDriver for various browsers.
    /// </summary>
    public static class WebDriverFactory
    {
        /// <summary>
        /// Initilizes IWebDriver base on the given WebBrowser name.
        /// </summary>
        /// <param name="name"></param>
        /// <returns></returns>
        public static IWebDriver CreateWebDriver(WebBrowser name)
        {
            switch (name)
            {
                case WebBrowser.Firefox:
                    return new FirefoxDriver();
                case WebBrowser.IE:
                case WebBrowser.InternetExplorer:
                    InternetExplorerOptions ieOption = new InternetExplorerOptions();
                    ieOption.IntroduceInstabilityByIgnoringProtectedModeSettings = true;
                    ieOption.EnsureCleanSession = true;
                    ieOption.RequireWindowFocus = true;
                    return new InternetExplorerDriver(@"./", ieOption);
                case "safari":
                    return new RemoteWebDriver(new Uri("http://mac-ip-address:the-opened-port"), DesiredCapabilities.Safari());
                case WebBrowser.Chrome:
                default:
                    ChromeOptions chromeOption = new ChromeOptions();
                    string location = @"./";
                    chromeOption.AddArguments("--disable-extensions");
                    return new ChromeDriver(location, chromeOption);
            }
        }
    }
    
    public enum WebBrowser
    {
        IE,
        InternetExplorer,
        Firefox,
        Chrome
    }


    // Usage
    var driver = WebDriverFactory.CreateWebDriver(WebBrowser.Chrome);

