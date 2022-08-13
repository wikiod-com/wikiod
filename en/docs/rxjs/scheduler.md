---
title: "Scheduler"
slug: "scheduler"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Using a TestScheduler to advance time manually
Most Rx operators take an optional scheduler on which to schedule their future iterations. If not supplied they will use their default configured scheduler. Supplying a scheduler can be useful for testing purposes in which we like to talk about virtual time instead of real time for speed of test execution.

    const scheduler = new Rx.TestScheduler();
    scheduler.stop();
    Rx.Observable.interval(100, scheduler)
      .do(i => console.log(i))
      .subscribe();
    scheduler.advanceBy(10 * 100);

