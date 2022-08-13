---
title: "Selenium simple example C# and Nunit"
slug: "selenium-simple-example-c-and-nunit"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

## Simple page load test and make sure the title of the page is correct
    public class HomepageTests
    {
        private IWebDriver _driver;
        
        [SetUp]
        public void LoadDriver()
        {
           _driver = new ChromeDriver();
           
        }

        [Test]
        public void Valid_Home_Page_Title()
        {
            _driver.Navigate().GoToUrl("Your homeoage url / local or remote");
            StringAssert.Contains("Expected title of your page", _driver.Title);

        }

        [TearDown]
        public void UnloadDriver()
        {
            _driver.Quit();
        }
    }

