---
title: "Custom Matchers"
slug: "custom-matchers"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Adding Custom Matchers
Custom matchers can be added in jasmine using the syntax:

    jasmine.addMatchers([
        toMatch: function () {
        return {
            compare: function (actual, expected) {
                return {
                    pass: actual===expected,
                    message: "Expected actual to match expected
                }
            }
        }
    }
    ]);

This matcher can now be called with:

    expected(actual).toMatch(expected);

## Negative Matchers
Custom matchers will have their pass value negated when using 'not'. Custom matchers can have a negative compare attribute to explicitly handle cases where their negation is desired:

    toMatch: function () {
            return {
                compare: function (actual, expected) {
                    return {
                        pass: actual===expected,
                        message: "Expected actual to match expected"
                    }
                },
                negativeCompare: function(actual, expected){
                    return {
                        pass: actual!==expected,
                        message: "Expected actual not to match expected"
                    }
                }
            }
        }

