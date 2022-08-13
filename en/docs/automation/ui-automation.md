---
title: "UI Automation"
slug: "ui-automation"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

The focus of this section would be list out tools, design approaches and common challenges for any UI Automation effort.

## Web UI Automation
Design Approaches
-----------------

 - Driving Factor: What should be our driving engine? Should it be test functions, keywords, data or behavior? Here we list different approaches with examples. 
 -     Data Driven/Keyword Driven/Hybrid:
 -     Code Driven:
 -     BDD/TDD/ATDD:


   



## Cucumber-Java with Page Factory Approach
Steps for Getting Started:
-----

 - Download Eclipse
 - Create a Java Maven Project with following example package organization <br/>
        <code>**src/test/java**
            com.example.pageobjects
            com.example.steps
            com.example.runner
         **src/test/resources**</code>
 - Add Cucumber Eclipse Plugin :
   https://github.com/cucumber/cucumber-eclipse/wiki
 - Add Cucumber Dependency to POM:https://cucumber.io/docs/reference/jvm#java 
 - Add Cucumber JUnit Dependency: 
   https://cucumber.io/docs/reference/jvm#junit-runner
 - Add Selenium Dependency:http://repo1.maven.org/maven2/org/seleniumhq/selenium/selenium-java/3.3.1/selenium-java-3.3.1.pom

TBD

