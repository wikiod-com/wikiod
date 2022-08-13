---
title: "Explicit waits with browser.wait()"
slug: "explicit-waits-with-browserwait"
draft: false
images: []
weight: 9618
type: docs
toc: true
---

## browser.sleep() vs browser.wait()
When it comes to dealing with timing issue, it is tempting and easy to put a "quick" `browser.sleep(<timeout_in_milliseconds>)` and move on.

The problem is, it would some day fail. There is no golden/generic rule on what sleep timeout to set and, hence, at some point due to network or performance or other issues, it might take more time for a page to load or element to become visible etc. Plus, most of the time, you would end up waiting more than you actually should.

[`browser.wait()`][1] on the other hand works differently. You provide an [Expected Condition function][2] for Protractor/WebDriverJS to execute and wait for the result of the function to evaluate to true. *Protractor would continuously execute the function and stop once the result of the function evaluates to true or a configurable timeout has been reached.*

There are multiple built-in Expected Conditions, but you can also create and use a custom one (sample [here][3]).


  [1]: http://www.protractortest.org/#/api?view=webdriver.WebDriver.prototype.wait
  [2]: http://www.protractortest.org/#/api?view=ProtractorExpectedConditions
  [3]: http://stackoverflow.com/a/30220003/771848

