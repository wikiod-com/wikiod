---
title: "angular-cli test coverage"
slug: "angular-cli-test-coverage"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

test coverage is defined as a technique which determines whether our test cases are actually covering the application code and how much code is exercised when we run those test cases.

Angular CLI has built in code coverage feature with just a simple command `ng test --cc`

## A simple angular-cli command base test coverage
If you want to see overall test coverage statistics than of course in Angular CLI you can just type below command, and see the bottom of your command prompt window for results.

    ng test --cc // or --code-coverage

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/omjN2.png

## Detailed individual component base graphical test coverage reporting
if you want to see component's individual coverage of tests follow these steps.

1.    `npm install --save-dev karma-teamcity-reporter`

2.     Add `require('karma-teamcity-reporter')` to list of plugins in karma.conf.js

3. `ng test --code-coverage --reporters=teamcity,coverage-istanbul`

note that list of reporters is comma-separated, as we have added a new reporter, teamcity.

after running this command you can see the folder `coverage` in your dir and open `index.html` for a graphical view of test coverage.

[![enter image description here][1]][1]

You can also set the coverage threshold that you want to achieve, in `karma.conf.js`, like this.

 

    coverageIstanbulReporter: {
          reports: ['html', 'lcovonly'],
          fixWebpackSourcePaths: true,
          thresholds: {
            statements: 90,
            lines: 90,
            branches: 90,
            functions: 90
          }
        },


  [1]: https://i.stack.imgur.com/luaq2.png

