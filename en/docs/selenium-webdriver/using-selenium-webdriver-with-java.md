---
title: "Using Selenium Webdriver with Java"
slug: "using-selenium-webdriver-with-java"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Selenium webdriver is web automation framework which allows you to test your web application against different web browsers. Unlike Selenium IDE, webdriver allows you to develop your own test cases in programming language of your choice. It supports Java, .Net, PHP, Python, Perl, Ruby.
 

## Opening browser window with specific URL using Selenium Webdriver in Java
    import org.openqa.selenium.WebDriver;
    import org.openqa.selenium.firefox.FirefoxDriver;
    
    class test_webdriver{
        public static void main(String[] args) {
           WebDriver driver = new FirefoxDriver();
           driver.get("http://stackoverflow.com/");
           driver.close();
        }
    }

## Opening a browser window with to() method
Opening a browser with to() method.

    import org.openqa.selenium.WebDriver;
    import org.openqa.selenium.firefox.FirefoxDriver;
    class navigateWithTo{
        public static void main(String[] args) {
           WebDriver driver = new FirefoxDriver();
           driver.navigate().to("http://www.example.com");
           driver.close();
        }
    }


