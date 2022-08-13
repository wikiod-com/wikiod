---
title: "User Event Before Load event"
slug: "user-event-before-load-event"
draft: false
images: []
weight: 9896
type: docs
toc: true
---

## Parameters
| Parameter | Details |
| ------ | ------ |
| *SuiteScript 2.0* | - |
| `scriptContext` | {`Object`} |
| `scriptContext.newRecord` | {`N/record.Record`} A reference to the record being loaded from the database |
| `scriptContext.type` | {`UserEventType`} The action type that triggered this User Event |
| `scriptContext.form` | {`N/ui/serverWidget.Form`} A reference to the UI form that will be rendered
| *SuiteScript 1.0* | - |
| `type` | {`Object`} The action type that triggered this User Event |
| `form` | {`nlobjForm`} A reference to the UI form that will be rendered |
| `request` | {`nlobjRequest`} the HTTP GET request; only available when triggered by browser requests |

# `beforeLoad`

The `Before Load` event is triggered by any read operation on a record. Any time a user, a script, a CSV import, or a web service request attempts to read a record from the database, the `Before Load` event gets fired.

Record actions that trigger a `beforeLoad` event:

* Create
* Edit
* View / Load
* Copy
* Print
* Email
* QuickView

# Typical Use Cases for `beforeLoad`

* Modify the UI form before the user sees it
* Set default field values
* Data pre-processing

# User Events **do not** chain

Code written in User Events will not trigger any User Events on *other* records. For example, loading the associated Customer record from the `beforeLoad` of a Sales Order record *will not trigger* the Customer record's `beforeLoad`. Even if you are loading another Transaction record, its User Events will not be fired.

NetSuite does this to avoid User Events triggering each other in an infinite loop. If you *do* need User Events to fire in a chained sequence, other script types (e.g. RESTlets, Suitelets, Scheduled Scripts) will need to be injected in between the events.

# Event Handler returns `void`

The return type of the `beforeLoad` event handler is `void`. Any data returned from our event handler has no effect on the system. We do not need to return anything from our handler function as we cannot actually do anything with its returned value.

## Minimal: Log a message on Before Load
<!-- language-all: lang-js -->

    // 1.0
    function beforeLoad(type, form, request) {
        nlapiLogExecution("DEBUG", "Before Load", "type=" + type);
    }

    // 2.0
    /**
     * @NApiVersion 2.x
     * @NScriptType UserEventScript
     * @NModuleScope SameAccount
     */
    define(["N/log"], function (log) {
        function beforeLoad(context) {
            log.debug({
                "title": "Before Load",
                "details": "type=" + context.type
            });
        }

        return {
            "beforeLoad": beforeLoad
        };
    });

## Modifying the UI form
<!-- language-all: lang-js -->

    // 1.0
    // Revealing Module pattern, structures 1.0 similar to 2.0
    var myNamespace = myNamespace || {};
    myNamespace.example = (function () {

        /** @appliedtorecord employee */
        var exports = {};
        
        function beforeLoad(type, form, request) {
            showBonusEligibility(form);
        }
    
        function showBonusEligibility(form) {
            var field = form.addField("custpage_is_bonus_eligible",
                "checkbox", "Eligible for Bonus?");
            field.setDefaultValue(isEligibleForBonus(nlapiGetNewRecord()) ? "T" : "F");
        }
    
        function isEligibleForBonus(rec) {
            // Implement actual business rules for bonus eligibility here
            return true;
        }
        
        exports.beforeLoad = beforeLoad;
        return exports;
    })();

    // 2.0
    /**
     * @appliedtorecord employee
     * @NScriptType UserEventScript
     * @NApiVersion 2.x
     */
    define(["N/log", "N/ui/serverWidget"], function (log, ui) {
        var exports = {};
        
        function beforeLoad(context) {
            showBonusEligibility(context.form);
        }
        
        function showBonusEligibility(form) {
            var field = form.addField({
                "id": "custpage_is_bonus_eligible",
                "label": "Eligible for Bonus?",
                "type": ui.FieldType.CHECKBOX
            });
            field.defaultValue = (isEligibleForBonus() ? "T" : "F");
        }
    
        function isEligibleForBonus(rec) {
            // Implement actual business rules for bonus eligibility here
            return true;
        }
        
        exports.beforeLoad = beforeLoad;
        return exports;
    });

## Restrict execution based on the action that triggered the User Event
<!-- language-all: lang-js -->

    // 1.0
    // Utilize the type argument and raw Strings to filter your
    // execution by the action
    function beforeLoad(type, form, request) {
        // Don't do anything on APPROVE
        // Note that `type` is an Object, so we must use ==, not ===
        if (type == "approve") {
            return;
        }
        
        // Continue with normal business logic...
    }

    // 2.0
    /**
     * @appliedtorecord employee
     * @NScriptType UserEventScript
     * @NApiVersion 2.x
     */
    define([], function () {
        var exports = {};
        
        // Utilize context.type value and context.UserEventType enumeration
        // to filter your execution by the action
        function beforeLoad(context) {
            // Don't do anything on APPROVE
            if (context.type === context.UserEventType.APPROVE) {
                return;
            }
            
            // Continue with normal business logic...
        }
        
        exports.beforeLoad = beforeLoad;
        return exports;
    });

## Restrict execution based on the context that triggered the User Event
<!-- language-all: lang-js -->

In SuiteScript 1.0, we retrieve the current execution context using `nlapiGetContext().getExecutionContext()`, then we compare the result to the appropriate raw Strings.

    // 1.0 in Revealing Module pattern
    var myNamespace = myNamespace || {};
    myNamespace.example = (function () {
        var exports = {};
        
        function beforeLoad(type, form, request) {
            showBonusEligibility(form);
        }
        
        function showBonusEligibility(form) {
            // Doesn't make sense to modify UI form when the request
            // did not come from the UI
            var currentContext = nlapiGetContext().getExecutionContext();
            if (!wasTriggeredFromUi(currentContext)) {
                return;
            }
            
            // Continue with form modification...
        }
        
        function wasTriggeredFromUi(context) {
            // Current context must be compared to raw Strings
            return (context === "userinterface");
        }
        
        function isEligibleForBonus() {
            return true;
        }
        
        exports.beforeLoad = beforeLoad;
        return exports;
    })();

In SuiteScript 2.0, we get the current execution context by importing the `N/runtime` module and inspecting its `executionContext` property. We can then compare its value to the values of the `runtime.ContextType` enumeration rather than raw Strings.

    // 2.0
    /**
     * @NScriptType UserEventScript
     * @NApiVersion 2.x
     */
    define(["N/ui/serverWidget", "N/runtime"], function (ui, runtime) {
        var exports = {};
        
        function beforeLoad(scriptContext) {
            showBonusEligibility(scriptContext.form);
        }
        
        function showBonusEligibility(form) {
            // Doesn't make sense to modify the form if the 
            if (!wasTriggeredFromUi(runtime.executionContext)) {
                return;
            }
            
            // Continue with form modification...
        }
    
        function wasTriggeredFromUi(context) {
            // Context can be compared to enumeration from runtime module
            return (context === runtime.ContextType.USER_INTERFACE);
        }
        
        exports.beforeLoad = beforeLoad;
        return exports;
    });
    

