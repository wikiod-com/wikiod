---
title: "Selenium e2e setup"
slug: "selenium-e2e-setup"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

This topic covers the end to end setup of Selenium i.e. Selenium Webdriver + TestNG + Maven + Jenkins.

For report addition, please refer to topic https://www.wikiod.com/selenium-webdriver/html-reports

## TestNG Setup
TestNG is your updated test framework for junit. We are going to utilize **testng.xml** for invoking test suites. This is helpful when we are going to use CI ahead.

# testng.xml #

In the root folder of your project create an xml file with the name testng.xml. Note that name can be different as well, but for convenience its used as "testng" everywhere. 

Below is the simple code for testng.xml file.

    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE suite SYSTEM "http://testng.org/testng-1.0.dtd">
    <suite name="Smoke"> //name of the suite 
        <test name="Test1"> //name of the test
            <classes>
                <class name="test.SearchTest">
                <methods>
                    <include name="searchTest"/>
                </methods>
                </class>
            </classes>
        </test>
    </suite>

## Maven Setup
TBD. How to setup pom.xml for calling testng.xml

## Jenkins Setup
TBD. Will cover the Jenkins setup for pulling code from git/bitbucket etc.

