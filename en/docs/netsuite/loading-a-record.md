---
title: "Loading a record"
slug: "loading-a-record"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## SS 1.0
<!-- language: lang-js -->
    var recordType = 'customer'; // The type of record to load. The string internal id.
    var recordID = 100; // The specific record instances numeric internal id.
    var initializeValues = null;
    /* The first two parameters are required but the third -- 
    *  in this case the variable initializeValues -- is optional. */
    var loadedRecord = nlapiLoadRecord(recordType, recordID, initializeValues);

## SS 2.0
This example assumes that the record module is set to the variable RECORDMODULE, as shown below.

<!-- language: lang-js -->    
    require(['N/record'], function(RECORDMODULE){

        var recordType = RECORDMODULE.Type.SALES_ORDER; //The type of record to load.
        var recordID = 100; //The internal ID of the existing record instance in NetSuite.
        var isDynamic = true; //Determines whether to load the record in dynamic mode.
    
        var loadedRecord = RECORDMODULE.load({
            type: recordType, 
            id: recordID,
            isDynamic: isDynamic,
        });
    });

