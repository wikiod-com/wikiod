---
title: "Using the NetSuite Records Browser"
slug: "using-the-netsuite-records-browser"
draft: false
images: []
weight: 9901
type: docs
toc: true
---

## Using the NetSuite Records Browser
The *Records Browser* defines the schema for all scriptable record types; it is an extremely critical reference tool for every SuiteScript developer. When you need to know how to reference a particular field on a specific record type in your script, the *Records Browser* is your guide.

[Direct Link](https://system.na1.netsuite.com/help/helpcenter/en_US/srbrowser/Browser2016_1/script/record/account.html)











## Other Schema

You may also notice tabs at the top of the *Records Browser* for *Schema Browser* and *Connect Browser*. These are very similar to the *Records Browser*, but for different NetSuite APIs.

The *Schema Browser* provides the schema for the SOAP-based Web Services API, while the *Connect Browser* provides the schema for the ODBC connector.



## Navigating the Records Browser
You browse the *Records Browser* first by Record Type, i.e. "Sales Order", "Invoice", "Employee". There is no searching capability within the *Records Browser*, so all navigation is done manually. Record Types are organized alphabetically, so you first click on the first letter of the record type you are interested in, then select the Record Type itself at the left.

For example, if you wanted to see the schema for the *Subisidary* record type, you would first click on *S* at the top, then *Subsidiary* at the left.

## Reading the Schema
Each schema provides you with an overwhelming amount of information about each record type. It is important to know how to break down all of this information.

At the top of the schema is the name of the Record Type followed by the Internal ID of the record type; this internal ID is the programmatic reference for the record type. The schema is then broken up into several sections:

* *Fields*: The *Fields* section lists the details for all of the record's *body* fields. The fields described here can be used when you are working with the record currently in context, or with a direct reference to a record object.
* *Sublists*: The *Sublists* section shows all of the sublists on the record and every scriptable column within each sublist. The fields in this section again apply when you are working with the record currently in context, or with a direct reference to a record object.
* *Tabs*: The *Tabs* section describes all of the native subtabs on the record type.
* *Search Joins*: The *Search Joins* section describes all of the related records through which you can build joins in your searches of this record type.
* *Search Filters*: The *Search Filters* section describes all of the fields that are available as a search filter for this record type. The internal ID when using a specific field as a search filter *does not always match* its internal ID as a body field.
* *Search Columns*: The *Search Columns* section describes all of the fields that are available as a search column for this record type. The internal ID when using a specific field as a search column *does not always match* its internal ID as a body field.
* *Transform Types*: The *Transform Types* section describes all of the record types that this one can be transformed into using the record transformation API.

## Finding a Field
As stated previously, there is no searching capability built in to the *Records Browser*. Once you've navigated to the appropriate Record Type, if you don't already know a particular field's Internal ID, the easiest way to find it is to use your browser's Find function (usually `CTRL+F`) to locate the field by its name in the UI.

## Required Fields
The *Required* column of the schema indicates whether this field is required to save the record. If this column says `true`, then you will need to provide a value for this field when saving any record of this type.

## nlapiSubmitField and Inline Editing
The `nlapiSubmitField` column is a critical piece to understand. This column indicates whether the field is available for inline editing. If `nlapiSubmitField` is `true`, then the field can be edited inline. This greatly impacts how this field is handled when trying to use the `nlapiSubmitField` or `record.submitFields` functions in your scripts.

When this column is `true`, you can safely use the Submit Fields APIs to update this field inline. When it is `false`, *you can still use these functions to update the field*, but what actually happens behind the scenes changes significantly.

When `nlapiSubmitField` is `false` for a particular field, and you utilize one of the Submit Fields APIs on it, the scripting engine behind the scenes will actually do a full load of the record, update the field, and submit the change back to the database. The end result is the same, but because the entire record is loaded and saved, your script will actually use a lot more governance than you might expect and will take longer to execute.

You can read about this in more detail on the Help page titled "Consequences of Using nlapiSubmitField on Non Inline Editable Fields."

