---
title: "Script Type Overview"
slug: "script-type-overview"
draft: false
images: []
weight: 9829
type: docs
toc: true
---

You create SuiteScript customizations using an event-driven system. You define various types of Script records, each of which has its own unique set of events, and in your source file, you define functions that will be called to handle those events as they occur.

Scripts are one of the primary components with which you'll design and build your applications. The goal with this article is merely to become acquainted with the Script types and events available.

## The Client Script
The Client Script is one of the more commonly used, and complex, script types available to you. As its name implies, the Client Script runs in the browser, i.e. on the client side. It is the only script type that runs on the client side; all others will execute on the server side of NetSuite.

The primary use of the Client Script is for responding to user interactions with record forms within the NetSuite UI.

As soon as the user loads a record form in Edit mode, a `pageInit` event is fired that we can use to run code as the form is initialized, before the user can interact with it.

Whenever the user then changes any field on the form, a series of events will fire:

1. A `validateField` event fires that allows us to validate the value the user is trying to enter in the field. We can use this to either accept or prevent the change from taking place.
1. A `fieldChanged` event then fires that allows us to respond to the new value in the field.
1. Lastly, a `postSourcing` event fires after any and all dependent fields have also sourced in their values. This allows us to respond to the change *and* make sure we are working with all of the correct data.

This series of events fires no matter whether the user is changing a body field or a sublist field.

As the user does make changes to sublist lines, another series of events will be triggered:

1. A `lineInit` event is fired whenever the user initially selects a new or existing line, before they are able to make any changes to the fields on the line.
1. Whenever the user clicks the *Add* button to add a new line, a `validateLine` event is fired that allows us to verify that the entire line is valid and can be added to the record.
1. Whenever the user clicks the *Insert* button to add a new line above an existing one, a `validateInsert` event is fired, which works exactly like the `validateLine` event.
1. Similarly, whenever the user tries to remove a line, a `validateDelete` is fired that allows to either allow or deny the removal of the line.
1. [SuiteScript 1.0 only] Lastly, after the appropriate validation event succeeds, if the change to the line also effected a change to the total amount of a transaction, then a `recalc` event is fired that allows us to respond to the change in amount of our transaction.
1. [SuiteScript 2.0 only] Lastly, after the appropriate validation event succeeds, a `sublistChanged` event is fired to allow us to respond to the completed line change.

Finally, when the user clicks the *Save* button on the record, a `saveRecord` event is fired that allows us to validate whether the record is valid and can be saved. We can either prevent the save from occurring, or allow it to proceed with this event.

The Client script has by far the most events of any Script type, and the most complex relationship between those events.

## The Suitelet and Portlet Scripts
Often we will want to build custom UI pages in NetSuite; enter the Suitelet. The Suitelet script is designed for building internal, custom UI pages. Pages can be free-form HTML, or they can utilize NetSuite's UI Builder APIs to construct forms that follow NetSuite's look and feel.

When it is deployed, a Suitelet receives its own unique URL. The Suitelet then has a single `render` event that is called whenever that URL is hit with an HTTP `GET` or `POST` request. Typically, the response to the `GET` request would be to render the form itself, and then the form would `POST` back to itself for processing the form data.

We can also leverage Suitelets to build wizard-style UI progressions using NetSuite's "Assistant" UI components.

Portlets are extremely similar to Suitelets, except that they are specifically used to build custom dashboard widgets rather than full custom pages. Other than that, the two script types function very much alike.

## The User Event Script
Closely related to the Client Script is the User Event Script. The events of this Script type are again fired when a record is being loaded or saved, but it instead runs on the server side. As such, it cannot be used to respond immediately to field changes, but it also is not limited to only users interacting with the record on a form.

User Event scripts will execute no matter where the load or submit request is coming from, whether it's a user working in the UI, a third-party integration, or another internal Script making the request.

Whenever a process or user attempts to read a record out of the database, the User Event's `beforeLoad` event is triggered. We can use this to pre-process data, set default values, or manipulate the UI form before the user sees it.

Once a process or user attempts to submit a record to the database, whether it's the creation of a new record, editing of an existing record, or the deletion of a record, the following sequence occurs:

1. First, before the request actually makes its way to the database, a `beforeSubmit` event fires. We can use this event, for example, to clean up the record before it gets in the database.
1. The request is sent to the database, and the record is created/modified/deleted accordingly.
1. After the database processing is complete, an `afterSubmit` event fires. We can use this event, for example, to send out email notifications of changes, or to sync up with integrated third-party systems.

You can also watch [this series of videos](https://www.youtube.com/playlist?list=PLG2tK6Va2WUCseA3viGC2oYPkyA45wfA-) that help to visualize the events of this script type.

## The Scheduled and Map/Reduce Scripts
There are two types of scripts we can leverage for running background processing on a specific, regular interval; these are the *Scheduled* and the *Map/Reduce* scripts. Note that the *Map/Reduce* script type is only available in SuiteScript 2.0. The *Scheduled* script is available for both 1.0 and 2.0.

The Scheduled script only has a single `execute` event that gets triggered on whatever schedule you define. For example, you may want to run a nightly script that applies payments to invoices, or an hourly script that syncs data with an external system. When the time interval hits, NetSuite fires this `execute` event on your Scheduled script.

The Map/Reduce script works similarly, but once it is triggered, it breaks the processing into four distinct phases:

1. The `getInputData` phase is where you gather all of the input data you will need to complete the business process. You can use this phase to perform searches, read records, and package your data into a decipherable data structure.
1. NetSuite automatically passes the results of your `getInputData` phase to the second phase, called `map`. This phase is responsible for grouping your input data logically for processing. For instance, if you're applying payments to invoices, you may want to first group the invoices by Customer.
1. The results of the `map` phase are then passed to the `reduce` phase, which is where the actual processing takes place. This is where you would, keeping with our example, actually apply the Payments to the Invoices.
1. Lastly, a `summary` phase is invoked that contains data regarding the results of all your processing across the previous three phases. You can use this to generate reports or send out emails that processing is complete.

The major advantage of the Map/Reduce script is that NetSuite will automatically parallelize the processing for you across multiple queues, if available.

Both of these script types have an extremely large governance limit, so you can also use them for bulk processing or generally long-running background processes.

The shortest interval either of these script types can be configured to run is every 15 minutes.

Both of these script types can also be invoked on-demand by users or by other scripts, if necessary.

## The RESTlet
RESTlets allow us to build custom REST-based endpoints into NetSuite; thus, RESTlets form the backbone of nearly any integration into NetSuite.

RESTlets provide individual event handlers for four of the most commonly used HTTP request methods:

* `GET`
* `POST`
* `PUT`
* `DELETE`

When a RESTlet receives a request, it will route the request to the appropriate event handler function based on the HTTP request method used.

Authentication to a RESTlet can be done via user session, HTTP headers, or OAuth tokens.

## The Mass Update Script
Using the Mass Update script, we can build custom Mass Updates for users to perform. This functions just like a normal Mass Update, where the user selects the type of Mass Update, builds a search that returns the records to update, and then each search result is passed individually into the custom Mass Update script.

The script provides a single `each` event handler that receives the internal ID and record type of the record that is to be updated.

Mass Update scripts must be triggered manually by users through the standard Mass Update interface.

Mass Update scripts have a massively high governance limit and are intended for commonly used, custom bulk processing.

## The Workflow Action Script
Workflows can be somewhat limited in their functionality; for example, workflows cannot interact with line items. The Workflow Action script type is intended to be invoked by a Workflow to add scripting functionality to accomplish what the workflow itself cannot.

Workflow Actions have a single `onAction` event handler that will be invoked by the Workflow.

## The Bundle Installation Script
Lastly, we have the Bundle Installation script type, which provides several events that allow us to interact with the installation, update, and uninstallation of a particular bundle. This is a rarely-encountered script type, but important to be aware of nonetheless.

The Bundle Installation includes the following event handlers, which should be fairly self-explanatory:

* `beforeInstall`
* `afterInstall`
* `beforeUpdate`
* `afterUpdate`
* `beforeUninstall`


