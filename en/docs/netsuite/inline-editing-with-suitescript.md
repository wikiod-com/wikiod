---
title: "Inline Editing with SuiteScript"
slug: "inline-editing-with-suitescript"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

Inline editing allows users to very quickly modify and update the data for a particular record without having to load the entire record on a page, edit the form, then save the record.

NetSuite developers have a corresponding functionality called `submitFields`. The `submitFields` functionality is provided by the `nlapiSubmitField` global function in SuiteScript 1.0 and the `N/record#submitFields` method in SuiteScript 2.0.

## Syntax
* nlapiSubmitField(recordType, recordId, fieldId, fieldValue);
* nlapiSubmitField(recordType, recordId, fieldIds, fieldValues);
* nlapiSubmitField(recordType, recordId, fieldId, fieldValue, doSourcing);

## Parameters
| Parameter | Details |
| --- | --- |
| recordType | `String` - The internal ID of the type of record being updated |
| recordId | `String` or `Number` - The internal ID of the record being updated |
| fieldIds | `String` or `String[]` - The internal ID(s) of the field(s) being updated |
| fieldValues | `any` or `any[]` - The corresponding values to be set in the given fields |
| doSourcing | `Boolean` - Whether dependent values should be sourced in upon record submission. Default is `false` |

The `submitFields` functionality is a companion feature to the [`lookupFields`](https://www.wikiod.com/netsuite/lookup-data-from-related-records) functionality.

# Performance and Limitations

`submitFields` performs significantly faster and uses less governance than making the same changes by loading and submitting the full record.

Multiple fields can be updated at once for the same cost as updating a single field. Updating more fields with `submitFields` *does not* incur a higher governance cost.

However, you must be aware that only certain fields on each record type are inline-editable, and the performance savings *only* applies to these inline-editable fields. If you use the `submitFields` function on any non-inline-editable field, the field *will* be updated correctly, but behind the scenes, NetSuite will actually load and submit the record, thus taking more time and using more governance. You can determine whether a field is inline-editable by referring to the "nlapiSubmitField" column in the [Records Browser](https://www.wikiod.com/netsuite/using-the-netsuite-records-browser).

`submitFields` functionality is also limited to the *body* fields of a record. If you need to modify sublist data, you will need to load the record to make your changes, then submit the record.

# References:

* NetSuite Help: "Inline Editing and SuiteScript Overview"
* NetSuite Help: "Inline Editing Using nlapiSubmitField"
* NetSuite Help: "Consequences of Using nlapiSubmitField on Non Inline Editable Fields"
* NetSuite Help: "Field APIs"
* NetSuite Help: "record.submitFields(options)"

## [1.0] Submit a Single Field
<!-- language-all: lang-js -->

    /**
     * A SuiteScript 1.0 example of using nlapiSubmitField to update a single field on a related record
     */
    
    // From a Sales Order, get the Customer ID
    var customerId = nlapiGetFieldValue("entity");
    
    // Set a comment on the Customer record
    nlapiSubmitField("customer", customerId, "comments", "This is a comment added by inline editing with SuiteScript.");


## [1.0] Submit Multiple Fields
<!-- language: lang-js -->

    /**
     * A SuiteScript 1.0 example of using nlapiSubmitField to update multiple fields on a related record
     */
    
    // From a Sales Order, get the Customer ID
    var customerId = nlapiGetFieldValue("entity");
    
    // Set a Comment and update the Budget Approved field on the Customer record
    nlapiSubmitField("customer", customerId,
        ["comments", "isbudgetapproved"],
        ["The budget has been approved.", "T"]);


## [2.0] Submit a Single Field
<!-- language: lang-js -->

    /**
     * A SuiteScript 2.0 example of using N/record#submitFields to update a single field on a related record
     */
    
    require(["N/record", "N/currentRecord"], function (r, cr) {
    
        // From a Sales Order, get the Customer ID
        var customerId = cr.get().getValue({"fieldId": "entity"});
    
        // Set a Comment on the Customer record
        r.submitFields({
            "type": r.Type.CUSTOMER,
            "id": customerId,
            "values": {
                "comments": "This is a comment added by inline editing with SuiteScript."
            }
        });
    });


## [2.0] Submit Multiple Fields
<!-- language: lang-js -->

    /**
     * A SuiteScript 2.0 example of using N/record#submitFields to update multiple fields on a related record
     */
    
    require(["N/record", "N/currentRecord"], function (r, cr) {
    
        // From a Sales Order, get the Customer ID
        var customerId = cr.get().getValue({"fieldId": "entity"});
    
        // Set a Comment and check the Budget Approved box on the Customer record
        r.submitFields({
            "type": r.Type.CUSTOMER,
            "id": customerId,
            "values": {
                "comments": "The budget has been approved.",
                "isbudgetapproved": true
            }
        });
    });


