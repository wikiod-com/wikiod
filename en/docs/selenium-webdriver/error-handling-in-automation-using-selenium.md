---
title: "Error Handling in Automation using Selenium"
slug: "error-handling-in-automation-using-selenium"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Python
[`WebDriverException`][1] is a base `Selenium-WebDriver` exception that could be used to catch [all other `Selenium-WebDriver` exceptions][2]

To be able to catch exception it should be imported first:

    from selenium.common.exceptions import WebDriverException as WDE

and then:

    try:
        element = driver.find_element_by_id('ID')
    except WDE:
        print("Not able to find element")

In the same way you can import other more specific exceptions:

    from selenium.common.exceptions import ElementNotVisibleException
    from selenium.common.exceptions import NoAlertPresentException
    ...
    

If you want to extract exception message only:

    from selenium.common.exceptions import UnexpectedAlertPresentException

    try:
        driver.find_element_by_tag_name('a').click()
    except UnexpectedAlertPresentException as e:
        print(e.__dict__["msg"])

  [1]: http://selenium-python.readthedocs.io/api.html#selenium.common.exceptions.WebDriverException
  [2]: http://selenium-python.readthedocs.io/api.html#module-selenium.common.exceptions

