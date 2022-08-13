---
title: "Governance"
slug: "governance"
draft: false
images: []
weight: 9813
type: docs
toc: true
---

# Governance

"Governance" is the name given to NetSuite's system for detecting and halting long-running, runaway, or resource-intensive scripts.

Each script type has governance limits that it cannot exceed, and there are four types of governance limits in place for each script type.

* API usage limit
* Instruction Count limit
* Timeout limit
* Memory usage limit

If a script exceeds its governance limit in **any one** of these four areas, NetSuite will **throw an *uncatchable* exception** and terminate the script immediately.

## API Usage Limit

NetSuite limits the API usage of your scripts with a system based on "usage units". Some NetSuite API calls, particularly the ones that perform a read or write action on the database, cost a specific number of units each time they are invoked. Each script type then has a maximum number of units that can be used during each execution of the script. 

*If a script exceeds its API usage limit, NetSuite terminates the script by throwing an `SSS_USAGE_LIMIT_EXCEEDED` error.*

Below are a few examples of unit costs for common operations. For an exhaustive list of Governance costs, see the article titled "API Governance" in NetSuite Help.

| Operation | Unit Cost |
| --- | --- |
| Loading a Saved Search | `5` |
| Retrieving Search Results | `10` |
| Scheduling a task | `10` |
| Requesting a URL | `10` |
| Sending an email | `10` |
| Creating a custom record | `2` |
| Creating an Employee record | `5` |
| Creating a Sales Order record | `10` |
| Saving a custom record | `4` |
| Saving a Contact record | `10` |
| Saving a Purchase Order record | `20` |

Different operations use different amounts of units, and certain operations cost a different amount based on the record type being used. The larger the number of units a function costs, typically the longer it will take to execute.

Transactions are the largest of the record types, so working with them costs the largest amount of units. Conversely, custom records are very lightweight, and so do not cost many units. Standard NetSuite records that are *not* Transactions, like Customers, Employees, or Contacts, sit in between the two in terms of cost.

These are the usage limits by script type:

| Script Type | Usage Limit |
| --- | --- |
| Client | 1,000 |
| User Event | 1,000 |
| Suitelet | 1,000 |
| Portlet | 1,000 |
| Workflow Action | 1,000 |
| RESTlet | 5,000 |
| Scheduled | 10,000 |
| Map/Reduce | 10,000 |
| Bundle Installation | 10,000 |
| Mass Update | 10,000 per record |

## Timeout and Instruction Count Limits

NetSuite also uses the governance system to detect and halt runaway scripts by using a timeout mechanism and an instruction counter.

If a script takes too much time to run, NetSuite will stop it by throwing an `SSS_TIME_LIMIT_EXCEEDED` error.

In addition, runaway scripts can be detected and halted based on their "Instruction Count". If the defined instruction count limits are exceeded, NetSuite will stop the script by throwing an `SSS_INSTRUCTION_COUNT_EXCEEDED` error.

There is, unfortunately, **no** Help documentation that defines:

* the timeout for each script type
* the instruction count limits for each script type
* what constitutes a single "instruction"

It is simply important to know that if you encounter either the `SSS_TIME_LIMIT_EXCEEDED` error or the `SSS_INSTRUCTION_COUNT_EXCEEDED` error in one of your scripts, you have processing that is taking too long. Focus your investigation on your loop structures to determine where optimizations may be made.

## Memory Usage Limit

If your script exceeds the memory usage limit, NetSuite will terminate your script by throwing a `SSS_MEMORY_USAGE_EXCEEDED` error.

Every variable declared, every function defined, every Object stored contributes to the memory usage of your script. 

Both the **Scheduled Script** and the **Map/Reduce Script** have documented `50MB` memory limits. There is also a documented limit of `10MB` for the size of any String passed in to or returned from a RESTlet. There is no other documentation on the specific limits for a given script.


## How many units do I have remaining?
<!-- language-all: lang-js -->

In SuiteScript 1.0, use `nlobjContext.getRemainingUsage()` to retrieve the remaining units. An `nlobjContext` reference is retrieved using the global `nlapiGetContext` function.

    // 1.0
    var context = nlapiGetContext();
    nlapiLogExecution("DEBUG", "Governance Monitoring", "Remaining Usage = " + context.getRemainingUsage());

    nlapiSearchRecord("transaction"); // uses 10 units
    nlapiLogExecution("DEBUG", "Governance Monitoring", "Remaining Usage = " + context.getRemainingUsage());

In SuiteScript 2.0, use the `getRemainingUsage` method of the `N/runtime` module's `Script` object.

    // 2.0
    require(["N/log", "N/runtime", "N/search"], function (log, runtime, s) {
        var script = runtime.getCurrentScript();
        log.debug({
            "title": "Governance Monitoring",
            "details": "Remaining Usage = " + script.getRemainingUsage()
        });

        s.load({"id":"customsearch_mysearch"}); // uses 5 units
        log.debug({
            "title": "Governance Monitoring",
            "details": "Remaining Usage = " + script.getRemainingUsage()
        });
    });

