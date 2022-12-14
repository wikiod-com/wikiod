---
title: "Getting started with flowtype"
slug: "getting-started-with-flowtype"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation
Flow works best when installed per-project with explicit versioning rather than globally.

Luckily, if you’re already familiar with `npm` or `yarn`, this process should be pretty familiar!

Add a `devDependency` on the flow-bin `npm` package:

    yarn add --dev flow-bin
    //
    npm install --save-dev flow-bin

## Run Flow:

    yarn run flow
    //
    npm run flow

Running the command `flow init` will generate a `.flowconfig` file within the directory that be modified as needed.

## Getting Started
Flow is a static type checker for your JavaScript code. It does a lot of work to make you more productive. Making you code faster, smarter, more confidently, and to a bigger scale.

Flow checks your code for errors through static type annotations. These types allow you to tell Flow how you want your code to work, and Flow will make sure it does work that way.


    // @flow
    function square(n: number): number {
      return n * n;
    }
    
    square("2"); // Error!

Because Flow understands JavaScript so well, it doesn’t need many of these types. You should only ever have to do a minimal amount of work to describe your code to Flow and it will infer the rest. A lot of the time, Flow can understand your code without any types at all.

    // @flow
    function square(n) {
      return n * n; // Error!
    }
    
    square("2");

You can also adopt Flow incrementally and easily remove it at anytime, so you can try Flow out on any codebase and see how you like it.



