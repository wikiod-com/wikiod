---
title : unit-testing Tutorial
slug : unit-testing-tutorial
weight : 9905
draft : false
images : []
type : docs
---

[Unit testing][1] describes the process of testing individual units of code in isolation from the system that they are a part of.  What constitutes a unit can vary from system to system, ranging from an individual method to a group of closely related classes or a module.  

The unit is isolated from its dependencies using [test doubles][2] when necessary and setup into a known state.   Its behaviour in reaction to stimuli (method calls, events, simulated data) is then tested against the expected behaviour.

Unit testing of entire systems can be done using custom written test harnesses, however many test frameworks have been written to help streamline the process and take care of much of the plumbing, repetitive and mundane tasks.  This allows developers to concentrate on what they want to test.

When a project has enough unit tests any modification of adding new functionality or performing a code refactoring can be done easily by verifying at the end that everything works as before. 

**Code Coverage**, normally expressed as a percentage, is the typical metric used to show how much of the code in a system is covered by Unit Tests; note that there is no hard and fast rule about how high this should be, but it is generally accepted that the higher, the better.
   
Test Driven Development [(TDD)][3] is a principle that specify that a developer should start coding by writing a failing unit test and only then to write the production code that make the test pass.
When practicing TDD, it can be said that the tests themselves are the first consumer of the code being created; therefore they help to audit and drive the design of the code so that it is as simple to use and as robust as possible.


  [1]: http://stackoverflow.com/tags/unit-testing/info
  [2]: https://www.wikiod.com/unit-testing/test-doubles
  [3]: https://en.wikipedia.org/wiki/Test-driven_development

