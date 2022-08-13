---
title : cucumber Tutorial
slug : cucumber-tutorial
weight : 9962
draft : false
images : []
type : docs
---

**About Cucumber**

Cucumber is a tool which runs executable specifications of software. Specifications, called "features", are written in structured natural language. Cucumber executes a feature by mapping each of its steps to a "step definition" written in the programming language supported by that implementation of Cucumber. Cucumber is implemented in [many programming languages including Ruby (the original), Java and Javascript][1]. It is also translated into many human languages.

Cucumber was written to support the agile methodology called Behavior-Driven Development (BDD). In BDD, one begins development outside-in by writing acceptance tests which describe the software's functionality from the user's point of view (rather than from a programmer's point of view such as in unit tests). Cucumber features serve as these acceptance tests.

In general, Cucumber features are human-readable documentation which is also an executable test suite, meaning that documentation and tests always agree. Cucumber is useful in communicating with non-programmer stakeholders about documentation and tests. It also allows programmers to write tests at a conceptual level without irrelevant programming-language concerns.

Cucumber is most often used to specify and test web applications, using a browser driver such as Selenium or PhantomJS. However, it can be used with any software that can be executed and whose state or results can be determined from the programming language that a Cucumber implementation supports.

**Other documentation**

Official documentation is at https://cucumber.io/docs. Documentation generated from the Cucumber features which describe Cucumber implementations is at

- JavaScript: https://relishapp.com/cucumber/cucumber-js/docs
- Ruby: https://relishapp.com/cucumber/cucumber/docs

https://relishapp.com/explore includes some other Cucumber-related tools and examples, although not, unfortunately, Cucumber-JVM.

**This topic**

This topic should only give a few examples which introduce the reader to Cucumber concepts. Other sections will give complete examples of installation, command-line and IDE usage, features, step definitions, etc.

  [1]: https://cucumber.io/docs#cucumber-implementations

