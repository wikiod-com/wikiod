---
title: "User Event Before and After Submit events"
slug: "user-event-before-and-after-submit-events"
draft: false
images: []
weight: 9817
type: docs
toc: true
---

## Syntax
* beforeSubmit(type) // Before Submit, 1.0
* beforeSubmit(scriptContext) // Before Submit, 2.0
* afterSubmit(type) // After Submit, 1.0
* afterSubmit(scriptContext) // After Submit, 2.0

## Parameters
| Parameter | Details |
| ------ | ------ |
| *SuiteScript 2.0* | - |
| `scriptContext` | `{Object}` |
| `scriptContext.newRecord` | `{N/record.Record}` A reference to the record that is being read from the database. We can use it to modify the field values on the record |
| `scriptContext.oldRecord` | `{N/record.Record}` A read-only reference to the previous state of the record. We can use it to compare to the new values |
| `scriptContext.type` | `{UserEventType}` An enumeration of the type of write action being performed |
| *SuiteScript 1.0* | - |
| `type` | `{String}` The type of write action being performed |


# `beforeSubmit` and `afterSubmit`

These two events are triggered by any database write operation on a record. Any time a user, a script, a CSV import, or a web service request attempts to write a record to the database, the Submit events get fired.

Record actions that trigger *both* Submit events:

* Create
* Edit
* Delete
* XEdit (inline edit)
* Approve
* Reject
* Cancel
* Pack
* Ship

Record actions that trigger `beforeSubmit` only:

* Mark Complete
* Reassign (support cases)
* Edit Forecast

Record actions that trigger `afterSubmit` only:

* Dropship
* Special Order
* Order Items
* Pay Bills

# Typical Use Cases for `beforeSubmit`

* Validate record before it is committed to database
* Permission and restriction checks
* Last-minute changes before database commit
* Pull updates from external systems

# Typical Use Cases for `afterSubmit`

* Email notification of record changes
* Browser redirection
* Create/update dependent records
* Push changes to external systems

# User Events **do not** chain

Code written in User Events will not trigger any User Events on *other* records. For example, modifying the associated Customer record from the `beforeSubmit` of a Sales Order record *will not trigger* the Customer record's submit events.

NetSuite does this to avoid User Events triggering each other in an infinite loop. If you *do* need User Events to fire in a chained sequence, other script types (e.g. RESTlets, Suitelets, Scheduled Scripts) will need to be injected in between the events.

# Event Handlers return `void`

The return type of the Submit event handlers is `void`. Any data returned from our event handler has no effect on the system. We do not need to return anything from our handler function as we cannot actually do anything with its returned value.

# !! CAUTION !!

Be very cautious when comparing values between old and new records. Empty fields from the *old* record are returned as `null`, while empty fields from the *new* record are returned as an empty String. This means you cannot simply compare the old with the new, or you will get false positives. Any logic you write must handle the case where one is `null` and one is an empty String appropriately.

## Minimal: Log a message
<!-- language-all: lang-js -->

    // 1.0, Revealing Module pattern
    var myNamespace = myNamespace || {};
    
    myNamespace.example = (function () {
    
        /**
         * User Event 1.0 example detailing usage of the Submit events
         *
         * @appliedtorecord employee
         */
        var exports = {};
    
        function beforeSubmit(type) {
            nlapiLogExecution("DEBUG", "Before Submit", "action=" + type);
        }
    
        function afterSubmit(type) {
            nlapiLogExecution("DEBUG", "After Submit", "action=" + type);
        }
    
        exports.beforeSubmit = beforeSubmit;
        exports.afterSubmit = afterSubmit;
        return exports;
    })();

    // 2.0
    define(["N/log"], function (log) {
    
        /**
         * User Event 2.0 example showing usage of the Submit events
         *
         * @NApiVersion 2.x
         * @NModuleScope SameAccount
         * @NScriptType UserEventScript
         * @appliedtorecord employee
         */
        var exports = {};
    
        function beforeSubmit(scriptContext) {
            log.debug({
                "title": "Before Submit",
                "details": "action=" + scriptContext.type
            });
        }
    
        function afterSubmit(scriptContext) {
            log.debug({
                "title": "After Submit",
                "details": "action=" + scriptContext.type
            });
        }
    
        exports.beforeSubmit = beforeSubmit;
        exports.afterSubmit = afterSubmit;
        return exports;
    });

## Before Submit: Validate record before it is committed to database
<!-- language-all: lang-js -->

For this example, we want to make sure that any Employee who is marked as a *Project Resource* also has an appropriate *Labor Cost* defined.

    // 1.0, Revealing Module pattern
    var myNamespace = myNamespace || {};
    myNamespace.example = (function () {
    
        /**
         * User Event 1.0 example detailing usage of the Submit events
         *
         * @appliedtorecord employee
         */
        var exports = {};
    
        function beforeSubmit(type) {
            if (!isEmployeeValid(nlapiGetNewRecord())) {
                throw nlapiCreateError("STOIC_ERR_INVALID_DATA", "Employee data is not valid", true);
            }
        }
    
        function isEmployeeValid(employee) {
            return (!isProjectResource(employee) || hasValidLaborCost(employee));
        }
    
        function isProjectResource(employee) {
            return (employee.getFieldValue("isjobresource") === "T");
        }
    
        function hasValidLaborCost(employee) {
            var laborCost = parseFloat(employee.getFieldValue("laborcost"));
    
            return (Boolean(laborCost) && (laborCost > 0));
        }
    
        exports.beforeSubmit = beforeSubmit;
        return exports;
    })();

    // 2.0
    define(["N/error"], function (err) {
    
        var exports = {};
    
        /**
         * User Event 2.0 example detailing usage of the Submit events
         *
         * @NApiVersion 2.x
         * @NModuleScope SameAccount
         * @NScriptType UserEventScript
         * @appliedtorecord employee
         */
        function beforeSubmit(scriptContext) {
            if (!isEmployeeValid(scriptContext)) {
                throw err.create({
                    "name": "STOIC_ERR_INVALID_DATA",
                    "message": "Employee data is not valid",
                    "notifyOff": true
                });
            }
        }
    
        function isEmployeeValid(scriptContext) {
            return (!isProjectResource(scriptContext.newRecord) || hasValidLaborCost(scriptContext.newRecord));
        }
    
        function isProjectResource(employee) {
            return (employee.getValue({"fieldId" : "isjobresource"}));
        }
    
        function hasValidLaborCost(employee) {
            var laborCost = employee.getValue({"fieldId" : "laborcost"});
    
            return (Boolean(laborCost) && (laborCost > 0));
        }
    
        exports.beforeSubmit = beforeSubmit;
        return exports;
    });

Note that we pass references to the *new* record into our validation because we do not care what the values used to be; we are only concerned with the values that are about to be written to the database. In 2.0, we do that via the `scriptContext.newRecord` reference, and in 1.0 we call the global function `nlapiGetNewRecord`.

When the data being submitted is not valid, we create and throw an error. In a `beforeSubmit` event, in order to prevent the changes from being written to the database, your function must `throw` an Exception. Often developers try to `return false` from their function, expecting that to be enough, but that is not sufficient. Error objects are created in 2.0 using the `N/error` module, and in 1.0 using the global `nlapiCreateError` function; we then raise an Exception using our created error object with the `throw` keyword.
    

## After Submit: Determine whether a field was changed
<!-- language-all: lang-js -->

After the record gets stored in the database, we want to inspect what was changed on the record. We'll do this inspection by comparing values between the old and new record instances.

    // 1.0, Revealing Module pattern
    var myNamespace = myNamespace || {};
    myNamespace.example = (function () {
    
        /**
         * User Event 1.0 example detailing usage of the Submit events
         *
         * @appliedtorecord employee
         */
        var exports = {};
    
        function afterSubmit(type) {
            notifySupervisor();
        }
    
        function notifySupervisor() {
            // Old and New record instances are retrieved from global functions
            var employee = nlapiGetNewRecord();
            var prevEmployee = nlapiGetOldRecord();
    
            // If Employee Status didn't change, there's nothing to do
            if (!didStatusChange(employee, prevEmployee)) {
                return;
            }

            // Otherwise, continue with business logic...
        }
    
        function didStatusChange(employee, prevEmployee) {
            var status = employee.getFieldValue("employeestatus");
            var prevStatus = prevEmployee.getFieldValue("employeestatus");
    
            /* !! Caution !!
             * Empty fields from the Old record come back as `null`
             * Empty fields from the New record come back as an empty String
             * This means  you cannot simply compare the old and new
             */
            return ((prevStatus || status) && (status !== prevStatus));
        }

        exports.afterSubmit = afterSubmit;
        return exports;
    })();
    
    // 2.0
    define(["N/runtime"], function (runtime) {
    
        /**
         * User Event 2.0 example detailing usage of the Submit events
         *
         * @NApiVersion 2.x
         * @NModuleScope SameAccount
         * @NScriptType UserEventScript
         * @appliedtorecord employee
         */
        var exports = {};
    
        function afterSubmit(scriptContext) {
            notifySupervisor(scriptContext);
        }
    
        function notifySupervisor(scriptContext) {
            // Old and New records are simply properties on scriptContext
            var employee = scriptContext.newRecord;
            var prevEmployee = scriptContext.oldRecord;
    
            // If Employee Status didn't change, there's nothing to do
            if (!didStatusChange(employee, prevEmployee)) {
                return;
            }

            // Otherwise, continue with business logic...
        }
    
        function didStatusChange(employee, prevEmployee) {
            var status = employee.getValue({"fieldId" : "employeestatus"});
            var prevStatus = prevEmployee.getValue({"fieldId" : "employeestatus"});
    
            /* !! Caution !!
             * Empty fields from the Old record come back as `null`
             * Empty fields from the New record come back as an empty String
             * This means  you cannot simply compare the old and new
             */
            return ((prevStatus || status) && (status !== prevStatus));
        }

        exports.afterSubmit = afterSubmit;
        return exports;
    });

Be very cautious when comparing values between old and new records. Empty fields from the *old* record are returned as `null`, while empty fields from the *new* record are returned as an empty String. This means you cannot simply compare the old with the new, or you will get false positives. Any logic you write must handle the case where one is `null` and one is an empty String appropriately.


