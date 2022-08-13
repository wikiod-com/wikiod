---
title: "Using Web API with jQuery"
slug: "using-web-api-with-jquery"
draft: false
images: []
weight: 9943
type: docs
toc: true
---

## Using expand to get lookup properties
If you fetch a single record and when that record has a lookup, you can also fetch values of the lookup value using the expand option. This reduces the number of calls you need to make to the API.

The sample gets all accounts and the last name of the primary contact:

<!-- language: lang-js -->

    $.ajax({
        url: Xrm.Page.context.getClientUrl() + '/api/data/v8.0/accounts?$select=name,primarycontactid&$expand=primarycontactid($select=lastname)',
        headers: { 
            'Accept': 'Application/json'
        }
    }).done(function (result) {
        $.each(result.value, function (key, value) {
            var lastname = value.primarycontactid.lastname;
        });
    });



## Getting accounts
This sample fetches accounts using a jQuery ajax method. On thing to note is that you need to set the header in the call to make the work. 

<!-- language: lang-js -->

    $.ajax({
        url: Xrm.Page.context.getClientUrl() + '/api/data/v8.0/accounts',
        headers: { 
            'Accept': 'Application/json' 
        }
    }).done(function (result) {
        var accounts = result.value;
    });



## Using filter to filter your API query
You can use the filter property to retrieve a subset of values from CRM. In this example only the accounts where the company name equals CompanyName are returned.

<!-- language: lang-js -->

    $.ajax({
        url: Xrm.Page.context.getClientUrl() + '/api/data/v8.0/accounts?$filter=name eq CompanyName',
        headers: { 
            'Accept': 'Application/json' 
        }
    }).done(function (result) {
        var accounts = result.value;
    });

## Using the verbose option to get optionset and lookup values
By default you will get de codes and id's for optionsets and lookups. If you want to get the label as well, you need to add an extra header to the call.

<!-- language: lang-js -->

    $.ajax({
        url: Xrm.Page.context.getClientUrl() + '/api/data/v8.0/contacts',
        headers: { 
            'Accept': 'Application/json', 
            'Prefer': 'odata.include-annotations="OData.Community.Display.V1.FormattedValue"' 
        }
    }).done(function (result) {
        $.each(result.value, function (key, value) {
            //sample to access a label
            var gendercodeLabel = value['gendercode@OData.Community.Display.V1.FormattedValue'];
            var gendercodeValue = value.gendercode;
        });
    });

## Using select to reduce the number of fields
For performance reasons you should minimize the number of fields you are requesting from the API. You can use the select property to do so.

This example fetches the name property of all accounts:

<!-- language: lang-js -->

    $.ajax({
        url: Xrm.Page.context.getClientUrl() + '/api/data/v8.0/accounts?$select=name',
        headers: { 
            'Accept': 'Application/json'
        }
    }).done(function (result) {
        $.each(result.value, function (key, value) {
            var lastname = value.primarycontactid.lastname;
        });
    });

