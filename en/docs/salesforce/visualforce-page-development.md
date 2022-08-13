---
title: "Visualforce Page Development"
slug: "visualforce-page-development"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Basic page
A basic VisualForce page can be created like this:

    <apex:page>
      <h1>Hello, world!</h1>
    </apex:page>

## Using Standard Controllers
If your page is for displaying or editing information about a particular type of record, it may be helpful to use a standard controller to reduce the amount of boilerplate code you need to write.

By using a standard controller, your page will be displayed with an `?id=SALESFORCE_ID` parameter, and you automatically get access to all merge fields on the record.

Add a standard controller to your page by specifying the `standardController` attribute on `<apex:page>`:

    <apex:page standardController="Account">
      This is a page for {!Account.Name}
    </apex:page>

You also get the standard controller methods for free:

* `cancel()` - returns the `PageReference` for the cancel page (usually navigates back to a list view)
* `delete()` - deletes the record and returns the `PageReference` for the delete page
* `edit()` - returns the `PageReference` for the standard edit page
* `save()` - saves the record and returns the `PageReference` to the updated record
* `view()` - returns the `PageReference` for the standard view page

You can use them like this:

    <apex:page standardController="Account">
      Name: <apex:inputField value="{!Account.Name}" />
      <apex:commandButton value="Update record" action="{!save}" />
    </apex:page>

