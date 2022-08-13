---
title: "Sourcing"
slug: "sourcing"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Parameters
| Parameter | Details |
| ------ | ------ |
| Source List | The field on the destination record which links to the source record. You must choose a source list before you can choose your source field. |
| Source From | The field on the source record from which data will actually be pulled. The field you choose must match the type of the destination field. For example, if you are sourcing from a *Phone Number* field, the destination field must be a *Phone Number* field as well. |

## Impact of *Store Value*
The *Store Value* setting on the custom field definition plays a very important role in the behaviour of Sourcing:
* When *Store Value* is **checked**, data is sourced into the field *only upon initial creation* of the record. After that, NetSuite breaks the sourcing link between the fields, and they become two independent fields. This effectively allows you to leverage Sourcing as a mechanism for setting the initial or default value of your custom field.
* When *Store Value* is **unchecked**, data is sourced dynamically into the field **every time the record is loaded**. Any changes a user or script might make to the field are **never saved**. If you leave *Store Value* unchecked, it is a good idea to make your field read-only.

## Limitations of Sourcing
* Sourcing cannot be applied to *native NetSuite fields*. If you need a native field as your destination field, then you will need to either create a workflow or write a script to perform the data sourcing.
* Sourcing cannot be applied to *sublist column fields*. If you need a sublist column as your destination field, then you will need to either create a workflow or write a script to perform the data sourcing.

## Pulling data into a custom field on Field Changed
<!-- language: lang-js -->

    // If you find yourself doing something like this...
    function fieldChanged(type, name, index) {
        if (name == 'salesrep') {
            var salesRepId = nlapiGetFieldValue('salesrep');
            var salesRepEmail = nlapiLookupField('employee', salesRepId, 'email');
            nlapiSetFieldValue('custbody_salesrep_email', salesRepEmail);
        }
    }
    // Stop! and consider using Sourcing for your custom field instead of code

## Defining Sourcing
While not strictly a SuiteScript topic, *Sourcing* is an incredibly powerful feature of NetSuite, and it's an important tool in the toolbelt for any SuiteScript developer. Sourcing allows us to *pull data into a record from any of its related records*, without writing any code or building a workflow to do so.

Less code is always more maintainable code.

Sourcing is defined on the *Sourcing & Filtering* tab of a Custom Field definition.

[![Sourcing parameters on a custom Entity Field definition][1]][1]


  [1]: http://i.stack.imgur.com/zDNMk.png

