---
title: "Testing non-angular apps with Protractor"
slug: "testing-non-angular-apps-with-protractor"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

Protractor is made for testing Angular applications. However, it is still possible to test non-angular applications with Protractor if needed.

## Changes needed to test non-angular app with Protractor
Use **`browser.driver`** instead of **`driver`**

Use **`browser.driver.ignoreSynchronization = true`** 

***Reason***: Protractor waits for angular components to load completely on a web-page befor it begins any execution. However, since our pages are non-angular, Protractor keeps waiting for 'angular' to load till the test fails with timeout. So, we need to explicitly tell the Protractor to not to wait for 'angular'

