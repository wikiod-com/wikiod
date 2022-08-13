---
title: "RestLet - Retrieve Data (Basic)"
slug: "restlet---retrieve-data-basic"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

This sample shows the basic structure of a RESTlet script that is intended to be used to retrieve data from an external system. RESTlets are endpoints that are created to allow communication with external systems. 

## Retrieve Customer Name
    /**
     * requestdata - the data packet expected to be passed in by external system
     * JSON - data format exchange
     * stringify() convert javascript object into a string with JSON.stringify()
     * nlobjError - add in catch block to log exceptions
     */

    function GetCustomerData(requestdata)
    {
        var jsonString = JSON.stringify(requestdata);
        nlapiLogExecution('DEBUG', 'JSON', jsonString);
    
        try
        {
            var customer = requestdata.customer;
            nlapiLogExecution('DEBUG', 'customer', customer);
        }
        catch (err)
        {
            var errMessage = err;
            if(err instanceof nlobjError)
            {
                errMessage = errMessage + ' ' + err.getDetails() + ' ' + errMessage;
            }
            nlapiLogExecution('DEBUG', 'Error', errMessage);
        }
    }

