---
title: "Features"
slug: "features"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

You can use cucumber as a plugin in QTP and Selenium.

The steps in the cucumber scenario are global variables.

You can implement once and call many times. Hence reduces the code  maintenance, and can reuse the same code when required.


Cucumber features are written in the Gherkin language and stored in files with the suffix `.feature`. This topic gives examples of each feature of Gherkin.

## A minimal Cucumber feature
In features/documentation.feature:

    Feature: Documentation
    
      Scenario: User views documentation
        When I go to the "Cucumber" documentation
        Then I should see the "Cucumber" documentation

A minimal feature has a `Feature` line and a `Scenario` with one or more steps beginning with `When`, `Then` or another Gherkin keyword.

 A sensible scenario would probably have more than one step.




## Scenario Outline
Template as below


    Scenario Outline: As a homemaker i want to buy and pay for the below product
      Given I purchase <a product>
        And I require a carry bag to take things to home
      When I pay bill using <payment method> to successfully checkout
      Then I should have a receipt
    
    Examples:
    | a product     | payment method |
    |  Cake         | Visa           |
    |  Coke         | Paypal         |

## Syntax Usage
    Feature: Some terse yet descriptive text of what is desired
         Textual description of the business value of this feature
         Business rules that govern the scope of the feature
         Any additional information that will make the feature easier to understand

    Background:
        Given some precondition needed for all scenarios in this file
            And another precondition

    Scenario: Some determinable business situation
      Textual description of the business value of this scenario
      Business rules that govern the scope of the scenario
      Any additional information that will make the scenario easier to understand
        Given some precondition
          And some other precondition
        When some action by the actor
          And some other action
          And yet another action
        Then some testable outcome is achieved
          And something else we can check happens too
          But something else we can check does not happen

    Scenario Outline: Some determinable business situation
        Given I am <precondition>
            And some other precondition
        When some action by the actor
        Then I have <outcome> rights

    Examples:
        | precondition | outcome  |
        | username1    | customer |
        | username2    | admin    |

The following keywords are interchangable, but depending on context, may be better to use:

 - `Feature:` | `Ability:` | `Business Need:`
 - `Scenario Outline:` | `Scenario Template:`
 - `Examples:` | `Scenarios:`
 - `Given` | `When` | `Then` | `And` | `But` | `*` |

