---
title: "Working with Sublists"
slug: "working-with-sublists"
draft: false
images: []
weight: 9817
type: docs
toc: true
---

NetSuite Records are divided into Body fields and Sublists. There are four types of sublists: Static, Editor, Inline Editor, and List.

We are able to add, insert, edit, and remove line items using Sublist APIs.

For a reference on exactly which sublists support SuiteScript, see the NetSuite Help page titled "Scriptable Sublists".

## Sublist Indices

Each line item in a sublist has an index that we can use to reference it.

In SuiteScript 1.0, these indices are `1`-based, so the first line item has index `1`, the second has index `2`, and so on.

In SuiteScript 2.0, these indices are `0`-based, so the first line item has index `0`, the second has index `1`, and so on. This of course more closely matches Array indexing in most languages, including JavaScript.

## Standard vs Dynamic Mode

The API we use for interacting with a sublist depends on whether we are working with the record in Standard or Dynamic mode.

The Standard-mode APIs simply let us provide the index of the line we want to work with as a parameter to the appropriate function.

The Dynamic-mode APIs follow a pattern:

1. Select the line we want to work with
1. Modify the selected line as desired
1. Commit the changes to the line

In Dynamic Mode, if we do not commit the changes to *each* line we modify, then those changes will not be reflected when the record is saved.

## Limitations

In order to work with sublist data via SuiteScript, we must have a reference in memory to the record. This means the record either needs to be retrieved from the script context, or we need to load the record from the database. 

We *cannot* work with sublists via either [lookup](https://www.wikiod.com/netsuite/lookup-data-from-related-records) or [submitFields](https://www.wikiod.com/netsuite/inline-editing-with-suitescript) functionality.

*Static* sublists do not support SuiteScript at all.

## References:

* NetSuite Help: "What is a Sublist?"
* NetSuite Help: "Sublist Types"
* NetSuite Help: "Scriptable Sublists"
* NetSuite Help: "Working with Sublist Line Items"
* NetSuite Help: "Sublist APIs"
* NetSuite Help: "Working with Records in Dynamic Mode"

## [1.0] How many lines on a sublist?
<!-- language: lang-js -->

    // How many Items does a Sales Order have...
    
    // ... if we're in the context of a Sales Order record
    var itemCount = nlapiGetLineItemCount("item");
    
    // ... or if we've loaded the Sales Order
    var order = nlapiLoadRecord("salesorder", 123);
    var itemCount = order.getLineItemCount("item");

## [1.0] Sublists in Standard Mode
<!-- language: lang-js -->

    // Working with Sublists in Standard mode ...
    
    // ... if the record is in context:

    // Add item 456 with quantity 10 at the end of the item sublist
    var nextIndex = nlapiGetLineItemCount("item") + 1;
    nlapiSetLineItemValue("item", "item", nextIndex, 456);
    nlapiSetLineItemValue("item", "quantity", nextIndex, 10);
    
    // Inserting item 234 with quantity 3 at the beginning of a sublist
    nlapiInsertLineItem("item", 1);
    nlapiSetLineItemValue("item", "item", 1, 234);
    nlapiSetLineItemValue("item", "quantity", 1, 3);
    
    // Insert item 777 with quantity 2 before the end of the last item
    var itemCount = nlapiGetLineItemCount("item");
    nlapiInsertLineItem("item", itemCount);
    nlapiSetLineItemValue("item", "item", itemCount, 777);
    nlapiSetLineItemValue("item", "quantity", itemCount, 2);
    
    // Remove the first line item
    nlapiRemoveLineItem("item", 1);
    
    // Remove the last line item
    nlapiRemoveLineItem("item", nlapiGetLineItemCount("item"));

    // ... or if we have a reference to the record (rec):

    // Add item 456 with quantity 10 at the end of the item sublist
    var nextIndex = rec.getLineItemCount("item") + 1;
    rec.setLineItemValue("item", "item", nextIndex, 456);
    rec.setLineItemValue("item", "quantity", nextIndex, 10);

    // Insert item 777 with quantity 3 at the beginning of the sublist
    rec.insertLineItem("item", 1);
    rec.setLineItemValue("item", "item", 1, 777);
    rec.setLineItemValue("item", "quantity", 1, 3);

    // Remove the first line
    rec.removeLineItem("item", 1);

    // Remove the last line
    rec.removeLineItem("item", rec.getLineItemCount("item"));

## [1.0] Sublists in Dynamic Mode
<!-- language: lang-js -->

    // Adding a line item to the end of a sublist in Dynamic Mode...
    
    // ... if the record is in context:
    nlapiSelectNewLineItem("item");
    nlapiSetCurrentLineItemValue("item", "item", 456);
    nlapiSetCurrentLineItemValue("item", "quantity", 10);
    nlapiCommitLineItem("item");
    
    // ... or if we have a reference to the record (rec):
    rec.selectNewLineItem("item");
    rec.setCurrentLineItemValue("item", "item", 456);
    rec.setCurrentLineItemValue("item", "quantity", 10);
    rec.commitLineItem("item");

## [1.0] Find a Line Item
<!-- language: lang-js -->

    // Which line has item 456 on it...
    
    // ... if we're in the context of a record
    var index = nlapiFindLineItemValue("item", "item", 456);
    if (index > -1) {
        // we found it...
    } else {
        // item 456 is not in the list
    }
    
    // ... or if we have a reference to the record (rec)
    var index = rec.findLineItemValue("item", "item", 456);
    if (index > -1) {
        // we found it on line "index"...
    } else {
        // item 456 is not in the list
    }


## [2.0] How many lines on a sublist?
<!-- language: lang-js -->

    // How many lines in a sublist in SuiteScript 2.0...
    
    require(["N/record"], function (r) {
        var rec = r.load({
            "type": r.Type.SALES_ORDER,
            "id": 123
        });
    
        // How many lines are on the Items sublist?
        var itemCount = rec.getLineCount({"sublistId": "item"});
    });


## [2.0] Sublists in Standard Mode
<!-- language: lang-js -->

    // Working with a sublist in Standard Mode in SuiteScript 2.0...
    
    require(["N/record"], function (r) {
        var rec = r.create({
            "type": r.Type.SALES_ORDER,
            "isDynamic": false
        });
    
        // Set relevant body fields ...
    
        // Add line item 456 with quantity 10 at the beginning of the Items sublist
        rec.setSublistValue({"sublistId": "item", "fieldId": "item", "value": 456, "line": 0});
        rec.setSublistValue({"sublistId": "item", "fieldId": "quantity", "value": 10, "line": 0});
    
        // Insert line item 238 with quantity 5 at the beginning of the Items sublist
        rec.insertLine({"sublistId": "item", "line": 0});
        rec.setSublistValue({"sublistId": "item", "fieldId": "item", "value": 238, "line": 0});
        rec.setSublistValue({"sublistId": "item", "fieldId": "quantity", "value": 5, "line": 0});
    
        // Insert line item 777 with quantity 3 before the last line of the Items sublist
        var lastIndex = rec.getLineCount({"sublistId": "item"}) - 1; // 2.0 sublists have 0-based index
        rec.insertLine({"sublistId": "item", "line": lastIndex}); // The last line will now actually be at lastIndex + 1
        rec.setSublistValue({"sublistId": "item", "fieldId": "item", "value": 777, "line": lastIndex});
        rec.setSublistValue({"sublistId": "item", "fieldId": "quantity", "value": 3, "line": lastIndex});

        // Remove the first line
        rec.removeLine({"sublistId": "item", "line": 0});
    
        // Remove the last line
        rec.removeLine({"sublistId": "item", "line": rec.getLineCount({"sublistId": "item"}) - 1});

        rec.save();
    });


## [2.0] Sublists in Dynamic Mode
<!-- language: lang-js -->

    // Working with Sublists in Dynamic Mode in SuiteScript 2.0...
    
    require(["N/record"], function (r) {
        var rec = r.create({
            "type": r.Type.SALES_ORDER,
            "isDynamic": true
        });
    
        // Set relevant body fields ...
    
        // Add line item 456 with quantity 10 at the end of the Items sublist
        var itemCount = rec.selectNewLine({"sublistId": "item"});
        rec.setCurrentSublistValue({"sublistId": "item", "fieldId": "item", "value": 456});
        rec.setCurrentSublistValue({"sublistId": "item", "fieldId": "quantity", "value": 10});
        rec.commitLine({"sublistId": "item"});
    
        // Insert line item 378 with quantity 2 at the beginning of the Items sublist
        rec.insertLine({"sublistId": "item", "line": 0});
        rec.selectLine({"sublistId": "item", "line": 0});
        rec.setCurrentSublistValue({"sublistId": "item", "fieldId": "item", "value": 378});
        rec.setCurrentSublistValue({"sublistId": "item", "fieldId": "quantity", "value": 2});
        rec.commitLine({"sublistId": "item"});
    
        // Insert line item 777 with quantity 3 before the last line of the Items sublist
        var lastIndex = rec.getLineCount({"sublistId": "item"}) - 1; // 2.0 sublists have 0-based index
        rec.insertLine({"sublistId": "item", "line": lastIndex}); // The last line will now actually be at lastIndex + 1
        rec.selectLine({"sublistId": "item", "line": lastIndex});
        rec.setCurrentSublistValue({"sublistId": "item", "fieldId": "item", "value": 777});
        rec.setCurrentSublistValue({"sublistId": "item", "fieldId": "quantity", "value": 3});
        rec.commitLine({"sublistId": "item"});

        // Remove the first line
        rec.removeLine({"sublistId": "item", "line": 0});
    
        // Remove the last line
        rec.removeLine({"sublistId": "item", "line": rec.getLineCount({"sublistId": "item"}) - 1});

        rec.save();
    });


## [2.0] Find a Line Item
<!-- language: lang-js -->

    // Finding a specific line item in SuiteScript 2.0...
    
    require(["N/record"], function (r) {
        var rec = r.load({
            "type": r.Type.SALES_ORDER,
            "id": 123
        });
    
        // Find the line that contains item 777
        var index = rec.findSublistLineWithValue({"sublistId": "item", "fieldId": "item", "value": 777});
    
        // find returns -1 if the item isn't found
        if (index > -1) {
            // we found it on line "index"
        } else {
            // item 777 is not in the list
        }
    });


