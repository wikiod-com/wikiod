---
title: "Trigger Bulkification"
slug: "trigger-bulkification"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Bulkification
If you do row-by-row processing in Salesforce, you'll probably reach the governor limit quickly. This is especially true with triggers and things that fire when you don't expect them. One documented method of escaping the governor limit is bulkification.

**Note:** The following information is based on the official Salesforce docs.

Bulkifying Apex code means making sure that the code properly handles more than one record at a time. When a batch of records initiate Apex, a single instance of that Apex code is executed, but that instance needs to handle all of the records in that given batch. 

**Not Bulkified:**

    trigger accountTestTrggr on Account (before insert, before update) 
    {
    
       //This only handles the first record in the Trigger.new collection
       //But if more than 1 Account initiated this trigger, those additional records
       //will not be processed
       Account acct = Trigger.new[0];
       List<Contact> contacts = [select id, salutation, firstname, lastname, email 
                  from Contact where accountId =&nbsp;:acct.Id];
       
    }

**Bulkified:** 

    trigger accountTestTrggr on Account (before insert, before update) 
    {
        List<String> accountNames = new List<String>{};
       //Loop through all records in the Trigger.new collection
       for(Account a: Trigger.new){
          //Concatenate the Name and billingState into the Description field
          a.Description = a.Name + ':' + a.BillingState
       }
    }


