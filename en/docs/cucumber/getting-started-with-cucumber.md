---
title: "Getting started with cucumber"
slug: "getting-started-with-cucumber"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## A Cucumber feature
Cucumber uses [Gherkin syntax][1] to describe your software's behaviors in structured natural language.

As such Cucumber is **not** a test framework (a common misunderstanding), but a _system documentation framework_, not very different from others like Use Case Scenario.
The common misunderstanding is due to the fact Cucumber documentation _can be automated in order to ensure it reflects the real system behavior_.

A Cucumber documentation suite is composed of `Features`, each describing a feature of your software, written in Gherkin and hosted in its own file.
By organizing those files into a directory structure you can _group_ and _organize_ features:

 - banking/
   - withdrawal.feature
   - atm.feature
   - personal-loan.feature
 - trading/
   - portfolio.feature
   - intraday.feature
 - mortgage/
   - evaluation.feature
   - accounting.feature


Each `Feature` is a plain text file composed by an optional, unstructured, purely informational introductory section and one or more `Scenarios`, each one representing a usage condition or use case.

Example:

    Feature: Documentation
    As a StackOverflow user or visitor
    I want to access the documentation section
        
        Scenario: search documentation on Stack Overflow
            Given I am on StackOverflow
            And I go to the Documentation section
            When I search for "cucumber"
            And I follow the link to "cucumber"
            Then I should see documentation for "cucumber"

Each line beginning with _Given_, _When_, _And_, _But_ or _Then_ is called a `Step`.
Any step can begin with any of those words regardless of order, but it is conventional to use them in the most natural way possible.

Features can also be organized via `Tags`, annotations the editor can put on a `Feature` or a `Scenario` to further categorize it.

Executability of a Feature is achieved via _glue_ code which can be written in many different languages (Java, Ruby, Scala, C/C++): each `Step` is matched against the glue code in order to identify `Step Definitions` (commonly abbreviated to _StepDef_) via regular expressions.

Every `Step` can have only one associated `Step Definition`.

When a `Feature` is executed each composing `Scenario` is executed, meaning each StepDef matching the `Step`s in every `Scenario` gets executed.


  [1]: https://cucumber.io/docs/reference#gherkin

## A Cucumber step definition in Ruby
In features/step_definitions/documentation.rb:

    When /^I go to the "([^"]+)" documentation$/ do |section|
      path_part =
        case section
          when "Documentation"
            "documentation"
          else
            raise "Unknown documentation section: #{section}"
        end
      visit "/documentation/#{path_part}/topics"
    end

    Then /^I should see the "([^"]+) documentation"$/ do |section|
      expect(page).to have_css('h2.doctag_title a', text: section)
    end

These steps exercise a web application. They are about as simple as they can be while still being practical.

Each step begins with a Gherkin keyword, which in a step definition file is a method which registers a step with Cucumber. The step-defining method takes a regular expression, which matches a line in a scenario, and a block, which is executed when the scenario gets to a matching line. Capture groups in the regular expression are passed to the block as block parameters.

The `When` step has a simple, in-line example of going from a human-readable reference to a page ("Documentation") to a URL. Real Cucumber suites usually put this logic in a separate method. The `visit` method is provided by Capybara. Capybara is not required to use Cucumber, although it is very commonly used with it. `visit` tells the browser controlled by Capybara to visit the given URL.

The `Then` step shows how the content of a page can be tested. `expect`/`to` is provided by RSpec (again, not required by Cucumber but very commonly used with it). `have_css` is provided by Capybara. The expectation is that the given CSS selector matches an element on the page which contains the given text. Note that this expectation would fail if the browser request had failed.

For more examples, see [the "Step definition" topic][1].


  [1]: https://www.wikiod.com/cucumber/step-definitions

## Pure Ruby Installation
To install Cucumber for use with Ruby simply use the command

    gem install cucumber

Alternatively, if you are using bundler, you can add the following line to your Gemfile

    gem 'cucumber'

And then run bundler

    bundle install

[I think this belongs in its own topic, Installation. I created that topic and copied this example there. When that topic is approved I'll move this there and delete the copy.]

