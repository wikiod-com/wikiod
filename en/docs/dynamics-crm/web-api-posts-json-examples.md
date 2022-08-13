---
title: "Web API posts JSON examples"
slug: "web-api-posts-json-examples"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

Be sure to add the following header to the post request. Otherwise the request will fail:

    Content-Type: application/json

## Creating a note / annotation with attachment
**url:** /api/data/v8.0/annotations

**json:**

    {
        "isdocument": true,
        "mimetype": "text/plain",
        "documentbody": "dGVzdA==",
        "objectid_account@odata.bind" : "/accounts(c6da77b6-d53e-e611-80b9-0050568a6c2d)",
        "filename": "test.txt"
    }

As the objectid can be almost every entity in CRM you need to define the entity with _entity name after objectid.

## Creating an account
**url:** /api/data/v8.0/accounts

**json:**

    {
        "name" : "New account"
    }

## Creating a contact with a parent customer
**url:** /api/data/v8.0/contacts

**json:**

    {
        "firstname" : "New",
        "lastname" : "Contact",
        "parentcustomerid_account@odata.bind" : "/accounts(c6da77b6-d53e-e611-80b9-0050568a6c2d)"
    }

As the parentcustomerid can be an account or contact you need to define the type of entity you want to set with _entityname after parentcustomerid.

## Creating a quote detail
**url:** /api/data/v8.0/quotedetails

**json:**

    {
        "productid@odata.bind": "/products(11c0dbad-91df-e311-b8e5-6c3be5a8b200)",
        "quoteid@odata.bind" : "/quotes(69b5e1ae-037f-e611-80ed-fc15b428dcdc)",
        "uomid@odata.bind" : "/uoms(73a5daea-6ddc-e311-a678-6c3be5a8c0e8)",
        "quantity": 1
    }

