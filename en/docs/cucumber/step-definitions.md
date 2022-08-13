---
title: "Step definitions"
slug: "step-definitions"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Step definitions are in the programming language supported by a given implementation of Cucumber. This topic gives examples of step definitions in each supported programming language and examples of using Cucumber API calls in step definitions.

## Some simple Ruby step definitions
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

