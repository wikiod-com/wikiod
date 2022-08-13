---
title: "Attributes"
slug: "attributes"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

There are some terms you must be aware of before going to write the Jasmine test cases. 

 1. Suites

A suit is the starting point of a Jasmine test cases, it actually calls the global jasmine function describe. It can have two parameters, a string value which describes the suit, and a function which implements the suit.

 2. Spec

Like suites, a spec starts with a string which can be the title of the suit and a function where we write the tests. A spec can contain one or more expectation that test the state of our code.

 3. Expectation

Value of an expectation is either true or false, an expectation starts with the function expect. It takes a value and call the actual one.

## Suites
    describe("Includes validations for index page", function () {
     
    });

## Spec
    it("Spy call for datepicker date validation", function () {
       
    });

## Expectation
    describe("Includes validations for index page", function () {
        var indexPage;
       
        it("Check for null values", function () {
            // We are going to pass "" (null) value to the function
            var retVal = indexPage.isNullValue("");
            expect(retVal).toBeTruthy();
        });
     
    });

