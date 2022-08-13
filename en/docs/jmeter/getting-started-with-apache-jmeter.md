---
title: "Getting started with Apache JMeter"
slug: "getting-started-with-apache-jmeter"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
1. Download a distributed archive from Binaries section of JMeter from [Download Apache JMeter][1] page. 
2. Depending on the version you downloaded, check [minimal Java version requirements][2] and install Java if needed. Ensure the `JAVA_HOME` environment variable is set and points to a correct version.
3. Extract the distribution archive in the directory of your choice.
4. Open JMeter UI:
   
   * **On Windows**: navigate to `<jmeter_location>\bin` directory and run `jmeterw.bat` or `jmeter.bat`
   * **On Linux/Mac**: navigate to `<jmeter_location>/bin` directory and run `jmeter` or 'jmeter.sh`. 

       For example:
    
         cd /Users/me/apache-jmeter/bin
         ./jmeter 

       **Note**: if the above command fails with `Permission denied` error, set execute permission on `jmeter` file:

         cd /Users/me/apache-jmeter/bin
         chmod u+x ./jmeter

If you are able to see JMeter UI, basic setup was successful.

[![JMETER_UI][4]][4]


  [1]: http://jmeter.apache.org/download_jmeter.cgi#binaries
  [2]: http://jmeter.apache.org/usermanual/get-started.html#java_versions
  [3]: https://i.stack.imgur.com/4osd0.png
  [4]: https://i.stack.imgur.com/wh5qS.png

## Overview of Apache JMeter components at high level
Apache JMeter segregated all the components into following groups based on their functionality:

 1. `Test Plan`: Starting point for scripting. JMeter saves the Test Plan in .jmx format. You add components to the Test Plan by Right Click on the Test Pand and navigating to the component you want to add.
 2. `Workbench`: Is a temporary place to start scripting. Along with all the components available in Test Plan, you get `HTTP(s) Test Script Recorder` in order to `record` the browser actions. Scripts can be saved in the Workbench provided you check the "Save Workbench" checkbox, otherwise they are no.
 3. `Threads (Users)`: you can define a number of (virtual) users to run, ramp-up time and loop count. you can also define on Test Plan whether Thread Groups need to run in sequential or parallel in the case of multiple Thread Groups. some examples are `Thread Group, setUp Thread Group, and tearDown Thread Group`
 4. `Logic Controller`: Allows you define the flow of execution and grouping of the samplers. one of the useful examples is Transaction Controller, where you combine all the samplers of Login page (all resources including images, .css, and .js files) so that combined response time can be retrieved. 
 5. `Sampler`: Sampler is the core of the JMeter. It gives components to simulate requests of various protocols such as HTTP, JDBC, FTP, SMTP etc. for example, HTTP sampler allows you simulate an HTTP packet (of GET, POST or any supported methods). Main stream protocols are supported, for others you can use Free or Commercial plugins.
 6. `Config Element`: Configuration elements can be used to set up defaults and variables for later use by samplers. Note that these elements are usually processed at the start of the scope in which they are found, i.e. before any samplers in the same scope. `CSV Dataset Config` allows you to provide test data like usernames, passwords of Login scenario `from a file`. `User Defined variables` config element allows you define variables which can be used across the Test Plan but where each Thread has its own copy. 
 7. `Timer`: By default, a JMeter thread executes samplers in sequence without pausing. Components presented here provide the functionality to introduce `User Think Time` in various forms among samplers. some examples are `Constant Timer, Constant Throughput Timer.`
 8. `Pre Processors`: allow you to perform operations/actions before sampler gets executed. `JSR223 Pre Processor` with [Apache Groovy][1] (similar to java coding style) allows you to make changes to the sampler before sending it.
 9. `Post Processors`: allow you perform operations/actions after sampler get executed. some useful examples are retrieving dynamic value such as Session ID, using `Regular Expression Extractor` post processor for any type of text, `CSS/JQuery Extractor` for HTML, `JSON Extractor` for JSON, `XPath Extractor` for XML.
 10. `Assertions`: As the name suggests, you can assert the response of samplers in different ways like searching for some text, the size of the response, and duration to receive the response etc. For example, you can use `Response Assertion` to search for some text in the response. If Assertion fails, JMeter marks the sampler, to which Assertion is applied, as Failure.
 11. Listeners: Listeners allow you to save the test results, see the test execution etc. for example, using `View Results Tree`, you can see the samplers request/response and whether they marked as PASS (green colour)/FAIL (red colour) by JMeter. using Aggregate Report, you can save the test results in CSV format. Important note is that, you use listeners either before the test run (for test script debug) or after the test run (to view results in graphs or summary) but not during the run. we must remove the listeners during the test as it consume a lot of system resources. So, we run the test in non-GUI mode and save the results using `-l` option in `.csv/.jtl` formats. Post the test, you can load this saved files into any of the listeners in the JMeter to view graphs/summary.

Following is the general syntax (`you add any component on need basis`):

    Test Plan
        Thread Group
            Config Element
            Logic Controller
                Pre Processor
                Sampler
                Timer
                Post Processor
                Assertion
            Listener


References:

 1. [Test Plan and Components][2]
 2. [Execution Order][3]
 3. [Scoping Rules][4]


  [1]: http://groovy-lang.org
  [2]: http://jmeter.apache.org/usermanual/test_plan.html
  [3]: http://jmeter.apache.org/usermanual/test_plan.html#executionorder
  [4]: http://jmeter.apache.org/usermanual/test_plan.html#scoping_rules

