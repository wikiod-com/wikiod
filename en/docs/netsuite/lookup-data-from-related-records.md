---
title: "Lookup Data from Related Records"
slug: "lookup-data-from-related-records"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

When processing a given record, you will oft need to retrieve data from one of its related records. For example, when working with a given Sales Order, you may need to retrieve data from the related Sales Rep. In SuiteScript terminology, this is called a **lookup**.

Lookup functionality is provided by the `nlapiLookupField` global function in SuiteScript 1.0 and the `N/search` module's `lookupFields` method in SuiteScript 2.0

## Syntax
* nlapiLookupField(recordType, recordId, columns);

## Parameters
| Parameter | Details |
| --- | --- |
| recordType | `String` - The internal ID of the type of record being looked up (e.g. `salesorder`, `employee`) |
| recordId | `String` or `Number` - The internal ID of the record being looked up |
| columns | `String` or `String[]` - The list of fields to retrieve from the record. Field IDs can be referenced from the "Search Columns" section of the [Records Browser](https://www.wikiod.com/netsuite/using-the-netsuite-records-browser). Joined fields can be retrieved using dot syntax (e.g. `salesrep.email`) |

# Performance

A Lookup is just shorthand for performing a search that filters on the internal ID of a single record for the result. Under the hood, lookups are actually performing a search, so the performance will be similar to that of a search that returns a single record.

This also means that a lookup will perform faster than loading the record to retrieve the same information.

# Limitations

Lookups can only be used to retrieve body field data. You cannot retrieve data from the sublists of a related record using a lookup. If you need sublist data, you will either need to perform a search or load the related record.

## [1.0] Lookup Joined Fields
<!-- language-all: lang-js -->

    /**
     * An example of nlapiLookupField to retrieve joined fields from a related record
     */
    
    var repId = nlapiGetFieldValue("salesrep");
    
    // Retrieve multiple fields from the associated Sales Rep
    var repData = nlapiLookupField("employee", repId, ["email", "firstname", "department.name"]);
    
    console.log(repData);
    console.log(repData.firstname + "'s email address is " + repData.email);
    console.log(repData.firstname + "'s department is " + repData["department.name"]);

## [1.0] Lookup Multiple Fields
<!-- language-all: lang-js -->

    /**
     * An example of nlapiLookupField to retrieve multiple fields from a related record
     */
    
    // Get the Sales Rep record ID
    var repId = nlapiGetFieldValue("salesrep");
    
    // Retrieve multiple fields from the associated Sales Rep
    var repData = nlapiLookupField("employee", repId, ["email", "firstname"]);
    
    console.log(repData);
    console.log(repData.firstname + "'s email address is " + repData.email);

## [1.0] Lookup Single Field
<!-- language-all: lang-js -->

    /**
     * An example of nlapiLookupField to retrieve a single field from a related record
     */
    
    // Get the Sales Rep record ID
    var repId = nlapiGetFieldValue("salesrep");
    
    // Get the name of the Sales Rep
    var repName = nlapiGetFieldText("salesrep");
    
    // Retrieve the email address from the associated Sales Rep
    var repEmail = nlapiLookupField("employee", repId, "email");
    
    console.log(repEmail);
    console.log(repName + "'s email address is " + repEmail);

## [2.0] Lookup Single Field
<!-- language-all: lang-js -->

    require(["N/search", "N/currentRecord"], function (s, cr) {
    
        /**
         * An example of N/search#lookupFields to retrieve a single field from a related record
         */
        (function () {
    
            var record = cr.get();
    
            // Get the Sales Rep record ID
            var repId = record.getValue({
                "fieldId": "salesrep"
            });
    
            // Get the name of the Sales Rep
            var repName = record.getText({
                "fieldId": "salesrep"
            });
    
            // Retrieve the email address from the associated Sales Rep
            var repData = s.lookupFields({
                "type": "employee",
                "id": repId,
                "columns": ["email"]
            });
    
            console.log(repData);
            console.log(repName + "'s email address is " + repData.email);
        })();
    });


## [2.0] Lookup Multiple Fields
<!-- language: lang-js -->

    require(["N/search", "N/currentRecord"], function (s, cr) {
    
        /**
         * An example of N/search#lookupFields to retrieve multiple fields from a related record
         */
        (function () {
    
            var record = cr.get();
    
            // Get the Sales Rep record ID
            var repId = record.getValue({
                "fieldId": "salesrep"
            });
    
            // Retrieve the email address from the associated Sales Rep
            var repData = s.lookupFields({
                "type": "employee",
                "id": repId,
                "columns": ["email", "firstname"]
            });
    
            console.log(repData);
            console.log(repData.firstname + "'s email address is " + repData.email);
        })();
    });


## [2.0] Lookup Joined Fields
<!-- language: lang-js -->

    require(["N/search", "N/currentRecord"], function (s, cr) {
    
        /**
         * An example of N/search#lookupFields to retrieve joined fields from a related record
         */
        (function () {
    
            var record = cr.get();
    
            // Get the Sales Rep record ID
            var repId = record.getValue({
                "fieldId": "salesrep"
            });
    
            // Retrieve the email address from the associated Sales Rep
            var repData = s.lookupFields({
                "type": "employee",
                "id": repId,
                "columns": ["email", "firstname", "department.name"]
            });
    
            console.log(repData);
            console.log(repData.firstname + "'s email address is " + repData.email);
            console.log(repData.firstname + "'s department is " + repData["department.name"]);
        })();
    });


