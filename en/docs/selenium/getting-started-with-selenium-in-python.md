---
title: "Getting started with Selenium in python"
slug: "getting-started-with-selenium-in-python"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

What is Selenium?

Selenium is a library of commands to help a programmer interface with a browser like a real user.

Things that Selenium does:

Finding element(s) in a webpage's html

 - Finds a single element:
     
    - `driver.find_element_by_css_selector("css.selector.of.element")` [CSS Selector help][1]
    - `driver.find_element_by_xpath("//xpath//of//element")`  [XPATH help][2]
    - `driver.find_element_by_name("name_of_element")`
    - `driver.find_element_by_id("id_of_element")`
    - `driver.find_element_by_partial_link_text("element_link_text")`
    - `driver.find_element_by_class_name("class_name_of_element")`
    - `driver.find_element_by_tag_name("tag_name_of_element")`

 - Finds a list of elements:
    - `driver.find_elements_by_css_selector("css.selector.of.elements")`
    - `driver.find_elements_by_xpath("//xpath//of//elements")`
    - `driver.find_elements_by_name("name_of_elements")`
    - `driver.find_elements_by_partial_link_text("elements_link_text")`
    - `driver.find_elements_by_class_name("class_name_of_elements")`
    - `driver.find_elements_by_tag_name("tag_name_of_elements")`
 - Official documentation: [selenium-python read the docs][3]

Interact with elements:

"method" represents any of the above methods to find an element or list of elements.

 - click function:
   - `driver.find_element_by_method.click()`

 - send_keys function:
   - `driver.find_element_by_method.send_keys("text")` sends the String "text" to the element found.
   - `driver.find_element_by_method.send_keys(KeyCode.UP)` sends the KeyCode for the up arrow key to the element found.



  [1]: http://www.w3schools.com/cssref/css_selectors.asp
  [2]: http://www.w3schools.com/xsl/xpath_intro.asp
  [3]: http://selenium-python.readthedocs.io/locating-elements.html

## Basic python Selenium
    from selenium import webdriver
    
    driver = webdriver.Chrome()  # Creates a new chromedriver instance
    driver.get("https://www.python.org/")  # Go to https://www.python.org/
    # Sends the text "python" to the text search box
    driver.find_element_by_id("id-search-field").send_keys("python")
    # Click on the search button
    driver.find_element_by_css_selector("button[type=\"submit\"]").click()

## Basic Selenium testcase
This is a basic example of a Selenium testcase using the python Unittest library

    from selenium import webdriver
    import unittest

    class SeleniumTest(Unittest.testcase):

        def setUp(self):
            self.driver = webdriver.Chrome()
            self.driver.implicitly_wait(30)
            
        def test(self):
            self.driver.get("https//www.google.com")
            self.driver.find_element_by_id("lst-ib").send_keys("python")
            self.driver.find_element_by_css_selector("span[class=\"sbico\"]").click()

        def tearDown(self):
            self.driver.quit()

