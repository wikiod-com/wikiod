---
title: "Selenium-webdriver with Python, Ruby and Javascript along with CI tool"
slug: "selenium-webdriver-with-python-ruby-and-javascript-along-with-ci-tool"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

This is one way of running selenium tests with CircleCI

## CircleCI integration with Selenium Python and Unittest2
Circle.yml

    machine:
      python:
        # Python version to use - Selenium requires python 3.0 and above
        version: pypy-3.6.0
    dependencies:
        pre:
            # Install pip packages
            - pip install selenium
            - pip install unittest
    test:
      override:
        # Bash command to run main.py
        - python main.py
    
main.py

    import unittest2
    
    # Load and run all tests in testsuite matching regex provided
    loader = unittest2.TestLoader()
    # Finds all the tests in the same directory that have a filename that ends in test.py
    testcases = loader.discover('.', pattern="*test.py")
    test_runner = unittest2.runner.TextTestRunner()
    # Checks that all tests ran
    success = test_runner.run(testcases).wasSuccessful()

example_test.py

    class example_test(unittest.TestCase):
        def test_something(self):
            # Make a new webdriver instance
            self.driver = webdriver.Chrome()
            # Goes to www.gooogle.com
            self.driver.get("https://www.google.com")


