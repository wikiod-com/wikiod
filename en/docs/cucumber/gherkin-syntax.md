---
title: "Gherkin Syntax"
slug: "gherkin-syntax"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

Gherkin is a business readable language for test automation and test documentation. It is understood by Cucumber and together exists as a Behavior Driven Development tool. 

## Syntax
 - Feature: this keyword signifies that what follows is a basic
   description or name of the feature being tested or documented.
 - Background: this keyword signifies steps that will be ran before every scenario in the feature.
 - Scenario: this keyword represents the name or basic description of a
   particular scenario testing the feature. 
 - Scenario Outline: This keyword signifies that the scenario will run N times for every argument listed in examples explicitly passed by column name wrapped in angled brackets.
 - Examples: this keyword notes the list of static arguments that will be passed into a scenario outline. 
 - Given: this keyword represents a given step, or precondition that is assumed before
   continuing. In the Arrange, Act, Assert paradigm, given represents
   "Arrange". 
 - When: this keyword represents a when step, or the behavior
   that is to be asserted against. In the Arrange, Act, Assert paradigm,
   given represents "Act". 
 - Then: this keyword represents a then step, or
   in other words, the step in which a behavior's result is validated.
   In the Arrange, Act, Assert paradigm, given represents "Assert". 
 - And: This keyword is used in conjunction with any of the keywords above.
   If you have two given statements, instead of explicitly calling Given
   twice, you can say, " Given A And B".

## Parameterized Steps
When writing Gherkin, there may be times in which you want to parameterize your steps for reusability by the engineer who is implementing the test plans. Steps receive parameters through regular expression capturing groups. (**Engineering Note:** If you do not have matching parameters for each capturing group in your regular expression you can expect an "CucumberException: Arity mismatch" to be thrown) In the below example, we have decided to wrap arguments in double quotes, as well as accept integers as arguments.

     Feature: Product Login
        As a user, I would like to be able to use my credentials to successfully 
        login. 
        
        Rules:
        - The user must have a valid username
        - The user must have a valid password
        - The user must have an active subscription 
        - User is locked out after 3 invalid attempts
        - User will get a generic error message following 
          login attempt with invalid credentials 
    
        Scenario: The user successfully logs in with valid credentials 
            This scenario tests that a user is able to successfully login
            provided they enter a valid username, valid password, and 
            currently have an active subscription on their account. 
    
            Given the user is on the login page
            When the user signs in with "valid" credentials
            Then the user should be logged in

        Scenario: The user attempts to log in with invalid credentials 
            This scenario tests that a user is not able to log in when
            they enter invalid credentials
    
            Given the user is on the login page
            When the user signs in with "invalid" credentials
            Then the user should be logged in

        Scenario: The user is locked out after too many failed attempts
            This scenario validates that the user is locked out
            of their account after failing three consecutive 
            attempts to log in
    
            Given the user is on the login page
            When the user fails to log in 3 times
            Then the user should be locked out of their account

## Feature Background
As you may have noticed in the example above, we are rewriting the same step multiple times: 

    Given the user is on the login page

This can be exceptionally tedious, especially if we have more than one given step that is reused. Gherkin provides a solution for this by giving us another keyword to work with: **Background:**. 

The background keyword serves to run the steps declared underneath it before every scenario in the Feature. Be sure not to add background step unless you are positive it is necessary for every scenario. Like the other keywords, Background is followed by a description or name and can have comments listed below it. Much like Feature and Scenario, Background must be proceeded by a colon.

 
    Feature: Product Login
        As a user, I would like to be able to use my credentials to successfully 
        login. 
        
        Rules:
        - The user must have a valid username
        - The user must have a valid password
        - The user must have an active subscription 
        - User is locked out after 3 invalid attempts
        - User will get a generic error message following 
          login attempt with invalid credentials 
    
        Background: The user starts out on the login page
            Given the user is on the login page

        Scenario: The user successfully logs in with valid credentials 
            This scenario tests that a user is able to successfully login
            provided they enter a valid username, valid password, and 
            currently have an active subscription on their account. 
    
            When the user signs in with "valid" credentials
            Then the user should be logged in
    
        Scenario: The user attempts to log in with invalid credentials 
            This scenario tests that a user is not able to log in when
            they enter invalid credentials
    
            When the user signs in with "invalid" credentials
            Then the user should be logged in

        Scenario: The user is locked out after too many failed attempts
            This scenario validates that the user is locked out
            of their account after failing three consecutive 
            attempts to log in
    
            When the user fails to log in 3 times
            Then the user should be locked out of their account


## Tags
For the purposes of documentation, you may want to filter test plans or scenarios by categories. Developers may want to run tests based on those same categories. Gherkin allows you to categorize Features as well as individual Scenarios via the user of Tags. In the example below, notice the above the Feature keyword is the Tag "@Automation". Gherkin recognizes this as a tag by the user of the "@" symbol. In this example, the engineer wants to make it clear that these tests are used for automation, where not every test is automate-able, some tests must be done by manual QA. 

Notice as well that the tag @Production has been added to the scenario testing user lock out. In this example, this is because this scenario is only active in the production environment of the application. The developers don't want their sandbox accounts locked out during development. This tags allows them to enforce that this test will only be ran against the production environment. 

Lastly, the Scenario Outline has the tag @Staging. For the purposes of this example, this is because the accounts being used are staging accounts and will not working in the other environments. Like the @Production tag, this ensures that these tests will only be ran in the Staging environment. 

These are just a few examples of where, how, and why you might use tags. Ultimately these tags are going to have meaning to you and the developers and can be anything and used to categorize however you see fit.     

    @Automation
    Feature: Product Login
        As a user, I would like to be able to use my credentials to successfully 
        login. 
        
        Rules:
        - The user must have a valid username
        - The user must have a valid password
        - The user must have an active subscription 
        - User is locked out after 3 invalid attempts
        - User will get a generic error message following 
          login attempt with invalid credentials 
    
        Background: The user starts out on the login page
            Given the user is on the login page

        Scenario: The user successfully logs in with valid credentials 
            This scenario tests that a user is able to successfully login
            provided they enter a valid username, valid password, and 
            currently have an active subscription on their account. 
    
            When the user signs in with "valid" credentials
            Then the user should be logged in
    
        Scenario: The user attempts to log in with invalid credentials 
            This scenario tests that a user is not able to log in when
            they enter invalid credentials
    
            When the user signs in with "invalid" credentials
            Then the user should be logged in

        @Production
        Scenario: The user is locked out after too many failed attempts
            This scenario validates that the user is locked out
            of their account after failing three consecutive 
            attempts to log in
    
            When the fails to log in 3 times
            Then the user should be locked out of their account

        @Staging
        Scenario Outline: The user successfully logs in with their account
             This scenario outlines tests in which various users attempt
             to sign in successfully 

             When the user enters their <username>
             And the user enters their <password>
             Then the user should be successfully logged on

             Examples:
             | username | password |
             | frank    | 1234     |
             | jack     | 4321     |

## Gherkin Tips
- Each scenario tests one behaviour
- Scenarios are written in a declarative way
- Avoid incidental details inside the scenario
- Omit the obvious
- Avoid conjunctive steps
- Keep your scenarios short
- Don’t have to many scenarios in the same feature
- Use descriptive scenario names
- Have only one When step
- Use the “should” in Then steps

## The Basics
This example will go over the basic structure of a Cucumber feature file in Gherkin. Feature files use several keywords in the basic syntax.

Lets look at the basic keywords:

 - **Feature:** this keyword signifies that what follows is a basic description or name of the feature being tested or documented. 
 - **Scenario:** this keyword represents the name or basic description of a particular scenario testing the feature. 
 - **Given** this keyword represents a given step, or precondition that is assumed before continuing. In the Arrange, Act, Assert paradigm, given represents "Arrange". 
 - **When** this keyword represents a when step, or the behavior that is to be asserted against. In the Arrange, Act, Assert paradigm, given represents "Act". 
 - **Then** this keyword represents a then step, or in other words, the step in which a behavior's result is validated. In the Arrange, Act, Assert paradigm, given represents "Assert". 
 - **And** This keyword is used in conjunction with any of the keywords above. If you have two given statements, instead of explicitly calling Given twice, you can say, " Given A And B". 
- **But** This keyword is used in conjunction **Given**, **When** and **Then** to signify that something should not happen. Then A But not B.

All keywords must be on a new line and must be the first word on a new line in order to be recognized by the Gherkin parser. The Feature and Scenario keywords must have a colon immediately after, as expressed in the example below. Given, When, Then, and And do not require a colon. 

In addition to keywords, you can write descriptions and comments. Descriptions occur after the keyword but on the same line, where as comments occur on lines underneath the keywords. When writing Feature comments, it is customary to provided explicit rules outlining edges and conditions that lead to different outcomes of behaviors. 

 
    
    Feature: Product Login
        As a user, I would like to be able to use my credentials to successfully 
        login. 
        
        Rules:
        - The user must have a valid username
        - The user must have a valid password
        - The user must have an active subscription 
        - User is locked out after 3 invalid attempts
        - User will get a generic error message following 
          login attempt with invalid credentials 

        Scenario: The user successfully logs in with valid credentials 
            This scenario tests that a user is able to successfully login
            provided they enter a valid username, valid password, and 
            currently have an active subscription on their account. 

            Given the user is on the login page
            When the user signs in with valid credentials
            Then the user should be logged in


## Scenario Outline
In some cases you may want to rerun the same scenario over and over, substituting out the arguments. In this case, Gherkin provides several new keywords to accommodate this situation, **Scenario Outline:** and **Example:**. The Scenario Outline keyword tells Cucumber that the scenario is going to run multiple times substituting out arguments from a list. The Examples keyword is called before the list is explicitly notated. Arguments for Scenario Outlines should be wrapped in angled brackets. In the example below, note that the argument names wrapped in the angled brackets correspond to the column names listed under Examples. Each column is separated by vertical bars, with column names on the first row. 

    Feature: Product Login
        As a user, I would like to be able to use my credentials to successfully 
        login. 
        
        Rules:
        - The user must have a valid username
        - The user must have a valid password
        - The user must have an active subscription 
        - User is locked out after 3 invalid attempts
        - User will get a generic error message following 
          login attempt with invalid credentials 
    
        Background: The user starts out on the login page
            Given the user is on the login page

        Scenario Outline: The user successfully logs in with their account
             This scenario outlines tests in which various users attempt
             to sign in successfully 

             When the user enters their <username>
             And the user enters their <password>
             Then the user should be successfully logged on

             Examples:
             | username | password |
             | frank    | 1234     |
             | jack     | 4321     |

