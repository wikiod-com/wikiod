---
title: "Control Flow and Promises"
slug: "control-flow-and-promises"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

Protractor/WebDriverJS has this mechanism called [Control Flow](http://www.protractortest.org/#/control-flow) - it is an internal queue of promises, it keeps the code execution organized.  

## Understanding the Control Flow
Consider the following test:

    it('should test something', function() {
      browser.get('/dashboard/');

      $("#myid").click();
      expect(element(by.model('username')).getText()).toEqual('Test');

      console.log("HERE");
    });

In the following test, when the `console.log()` is executed and you see `HERE` on the console, none of the Protractor commands from previous lines have been executed. This is an entirely *asynchronous* behavior. The commands are represented as promises and were put on the Control Flow which would execute and resolve the promises sequentially, one by one.

See more at [Promises and the Control Flow][1].


  [1]: http://www.protractortest.org/#/control-flow

