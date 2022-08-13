---
title: "Getting started with selenium-grid"
slug: "getting-started-with-selenium-grid"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## What is Selenium Grid?
Selenium-Grid is a configuration of *Hub* & *Node* which allows you to run your tests on different machines against different browser combinations in parallel. That is, running multiple tests at the same time against *different machines* running *different browsers* on *different operating systems*. In other words Selenium Grid supports running the tests in **Distributed Environment**. 

**When to use it**

 - **To run your tests against multiple browsers, multiple versions of browser, and browsers running on different operating systems.** This will ensure that the application you are testing is fully compatible with a wide range of browser-O.S combinations.
 - **To reduce the time it takes for the test suite to complete a test pass.** Lets say you setup your Grid to run 8 tests at a time, your execution would finish 8 times faster as compared to your normal run.

Selenium-Grid is used to speed up the execution of a test by using multiple machines to run tests in parallel.

## What is a Hub & Node?
**The Hub**

 - The *Hub* is the main engine/central point of the entire configuration, point where all the nodes are connected.
 - *Hub* should run only on a single machine.
 - There should only be 1 hub running where all the tests are loaded.
 - Tests will be run on the machines where hub is running, but you can see the browsers on the node machines.

**The Nodes**

 - Nodes are the instances (machines) which will execute the tests that are loaded on the hub.
 - There are no limitations on Node machines, a user can setup n number of Nodes.
 - Nodes can be launched on different machines with different OS and browser combinations.
 - Machines running the nodes can be of different/same configurations as of Hub Machine.

