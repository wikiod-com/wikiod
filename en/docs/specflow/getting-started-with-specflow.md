---
title: "Getting started with specflow"
slug: "getting-started-with-specflow"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Setup for Specflow

Pre -Requsite:

Download Visual Studio IDE

1. Create a new project 

2. Install specflow visual studio integration, Nunit Adapter & Nunit framework

[![enter image description here][2]][2]

3. Download specflow for visual studio as shown below

[![enter image description here][1]][1] 



  [1]: https://i.stack.imgur.com/lYUB1.png
  [2]: https://i.stack.imgur.com/EsOLa.png

## A Simple Google Search using Specflow
This is a simple example to search in Google. It containt two parts,

1. Feature File 
2. Step Definition File 

Am not going into much details here as the code itself is self explanatory. 

**Feature File**

    Feature:Google  Key word search

    @mytag

    Scenario: search Spec Flow in Google search bar
    Given I have entered the Google Home page
    And I have entered spec flow into google search bar
    When I press search button
    Then the result should be a new pages with results for spec flow


**Step Definition File**
    
    using OpenQA.Selenium;
    using OpenQA.Selenium.Firefox;
    using System;
    using TechTalk.SpecFlow;
    using static NUnit.Core.NUnitFramework;
    
    namespace GoogleSearch.GoogleSearch
    {
        [Binding]
        public class GoogleKeyWordSearchSteps
        {
              IWebDriver driver = new FirefoxDriver();
            [Given(@"I have entered the Google Home page")]
            public void GivenIHaveEnteredTheGoogleHomePage()
            {
                driver.Navigate().GoToUrl("https://www.google.co.nz");
            }
            
            [Given(@"I have entered spec flow into google search bar")]
            public void GivenIHaveEnteredSpecFlowIntoGoogleSearchBar()
            {
            driver.FindElement(By.XPath("/html/body/div/div[3]/form/div[2]/div[2]/div[1]/div[1]/div[3]/div/div[3]/div/input[1]")).SendKeys("Spec Flow");
            }
            
            [When(@"I press search button")]
            public void WhenIPressSearchButton()
            {
            driver.FindElement(By.XPath("/html/body/div/div[3]/form/div[2]/div[3]/center/input[1]")).Click();
            }
            
            [Then(@"the result should be a new pages with results for spec flow")]
            public void ThenTheResultShouldBeANewPagesWithResultsForSpecFlow()
            {
                //  Assert.AreEqual("Google", driver.Title);
            }
        }
    }
    


