---
title: "Create a record"
slug: "create-a-record"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Create new Task
<!-- language: lang-js -->
    var record = nlapiCreateRecord('task');
    record.setFieldValue('title', taskTitle);
    var id = nlapiSubmitRecord(record, true);

## Creating record in dynamic mode
 var record = nlapiCreateRecord('customrecord_ennveeitissuetracker', { recordmode: 'dynamic' });
       nlapiLogExecution('DEBUG', 'record', record);
         record.setFieldValue('custrecord_name1', name);
       record.setFieldValue('custrecord_empid', id);
         record.setFieldValue('custrecord_contactno', contactno);
         record.setFieldValue('custrecord_email', email);
        record.setFieldValue('custrecord_location', loc);
        record.setFieldValue('custrecord_incidentdate', incidentdate);
        record.setFieldValue('custrecord_issuedescription', desc);
       // record.setFieldValue('custrecord_reportedby', report);
        record.setFieldValue('custrecord_issuetype', issuetype);
        record.setFieldValue('custrecord_priority', priority);
    //    record.setFieldValue('custrecord_replacementprovided', repl);
        record.setFieldValue('custrecord_issuestatus', issuestatus);
       // record.setFieldValue('custrecord_resolvedby', resolvedby);
        record.setFieldValue('custrecord_remarks', remarks);
        record.setFieldValue('custrecord_resolvedby', resolvedby);
        record.setFieldValue('custrecord_updatedstatus', updatedstatus);
         var id = nlapiSubmitRecord(record,true);
         var recordId = nlapiGetRecordId();
         record = nlapiLoadRecord('customrecord_ennveeitissuetracker', id);

