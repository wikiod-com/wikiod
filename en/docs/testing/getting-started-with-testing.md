---
title: "Getting started with testing"
slug: "getting-started-with-testing"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Test case
TEST CASE –

A test case is a set of conditions and steps that when followed/applied, a tester can determine whether a feature, an application or a software system is working as originally designed. 

A test case usually contains References to the original task, Pre-conditions ( if the pre-conditions are not met, testing can not continue), Steps (steps describe what a tester needs to do in order to get a result), Expected Result (what should happen after following all the described steps) but may also contain Estimation, Priority, Environment on which to perform test.

Example of a test case :-

Title – Perform a login action on mail.google.com with an invalid password

Preconditions:
1. Access to supported browsers: IE 11, Firefox, Chrome.
2. Internet connection
3. A valid gmail account

Test steps :
1.    Launch the URL "mail.google.com" on one of the supported browsers
2.    Enter your email address in the email field
3.    Click the NEXT button.
4.    Enter an incorrect password in the Password field
5.    Click the Sign-In button.

Expected Result – A error message will be displayed under the Password field with red letters stating "Wrong password. Try again"


## Installation or Setup
Detailed instructions on getting testing set up or installed.

## Test case
A test case or test script is a more or less formal description of the actions needed to prove that a requirement is met. It has a descriptive title, may start off with some preparation and ends with an expected result. Usually the test cases for a *system under test* are organised into *test suites*.

Log on to system

Step | Result
------ | ------
Start system  | Startup screen shows, logon asked
Type username | username shows
Type password   |  '*' characters show the number of characters entered
Press Enter | Main screen shows

