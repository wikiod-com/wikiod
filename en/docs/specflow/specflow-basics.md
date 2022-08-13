---
title: "Specflow Basics"
slug: "specflow-basics"
draft: false
images: []
weight: 9994
type: docs
toc: true
---



 You can write tests in non-techinical way as below using specflow.

Specflow is an open source extension available in visual studio, that lets you to write specifications using Gherkin syntax.

Just if you are aware about cucumber for BDD approach, then specflow is cucumber for .net.

## Example feature file
       
***Example Feature file:***


**Feature**: Calculator

       In order to avoid silly mistakes
       As a math idiot
       I want to be told the sum of two numbers


@mytag

**Scenario**: Add two numbers

       Given I have entered 50 into the calculator
       And I have also entered 70 into the calculator
       When I press add
       Then the result should be 120 on the screen

## Data Driven test in Specflow
        Given I have entered <FirstOperand> into the calculator
        And I have also entered <SecondOperand> into the calculator
        When I press add
        Then the result should be <Result> on the screen
        |FirstOperand|SecondOperand|Result|
        |20          |30           |50    |
        |60          |40           |100   |
it will run test 2 times for 2 set of test data

