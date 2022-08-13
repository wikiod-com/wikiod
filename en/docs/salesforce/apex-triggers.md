---
title: "Apex Triggers"
slug: "apex-triggers"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Syntax
- trigger <_name_> on <_object-api-name_> (<_events_>) { // your trigger logic }

## Parameters
| parameter| description|
| ------ | ------ |
| name| Name of the trigger|
| object-api-name| Object on which the trigger will fire. Can be any standard or custom object.|
| events| Events which will fire the trigger. Are a combination of either `before`/`after` with any of `insert`/`update`/`delete`. There's also `after undelete` without `before` counterpart.|


## Basic trigger
    trigger AccountTrigger on Account (before insert) {
        System.debug('Account(s) are about to be inserted');
    }

## Trigger context variables
    trigger ContactTrigger on Contact (before insert, after insert,
                                       before update, after update,
                                       before delete, after delete,
                                       after undelete) {
        /** Before or After trigger execution**/
        //Returns true if trigger is before
        System.debug('Trigger:Time:Before : ' + Trigger.isBefore);
        //Returns true if trigger is after
        System.debug('Trigger:Time:After  : ' + Trigger.isAfter);
        
        /**DML Operation trigger execution **/
        //Returns true if trigger is insert
        System.debug('Trigger:DML:Insert  : ' + Trigger.isInsert);
        //Returns true if trigger is update
        System.debug('Trigger:DML:Update  : ' + Trigger.isUpdate);
        //Returns true if trigger is delete
        System.debug('Trigger:DML:Delete  : ' + Trigger.isDelete);
        //Returns true if trigger is undelete
        System.debug('Trigger:DML:Undelete: ' + Trigger.isUndelete);
        
        /** Records on Trigger execution **/
        //Returns data in state before DML. Records are read only
        //Not available for Insert Operation
        //Format: List<sObject>
        List<Contact> old_contacts = Trigger.old;
        System.debug('Trigger:Data:Old    : ' + old_contacts);
        //Returns data in state before DML. Records are read only
        //Not available for Insert Operation
        //Format: Map<Id, sObject>
        Map<Id, Contact> old_contacts_map = Trigger.oldMap;
        System.debug('Trigger:Data:OldMap : ' + old_contacts_map);
        //Returns data in state after DML.
        //Allowed for modifications in before context only
        //Not available for Delete Operation
        //Format: List<sObject>
        List<Contact> new_contacts = Trigger.new;
        System.debug('Trigger:Data:New    : ' + new_contacts);
        //Returns data in after before DML. 
        //Allowed for modifications in before context only
        //Not available for InsertOperation
        //Format: Map<Id, sObject>
        Map<Id, Contact> new_contacts_map = Trigger.newMap;
        System.debug('Trigger:Data:NewMap : ' + new_contacts_map);
        
        /** Another context variables **/
        //Returns amount of record in DML for trigger execution
        System.debug('Trigger:Size        :' + Trigger.size);
        //Returns true if the current context for the Apex code 
        //is a trigger, not VF, web service or anonymous apex
        System.debug('Trigger:isExecuting :' + Trigger.isExecuting);
    
        //Simple example how to use above context variables 
        //for different scenarios in combination
        if (Trigger.isBefore && Trigger.isUpdate) {
            // actions for before update
        } else if (Trigger.isAfter) {
            if (Trigger.isUpdate) {
                // actions for after update
            } else if (Trigger.isInsert) {
                // actions for after insert
            }
        }
    }

## Manipulating records that fired the trigger
    trigger MyTrigger on SomeObject__c (after insert, after update) {
        if (Trigger.isAfter && Trigger.isInsert) {
            System.debug('The following records were inserted: ');
            for (SomeObject__c o : Trigger.new) {
                System.debug(o.Name);
            }
        } else if (Trigger.isAfter && Trigger.isUpdate) {
            for (Id key : Trigger.newMap) {
                SomeObject__c theOldOne = Trigger.newMap.get(key);
                SomeObject__c theNewOne = Trigger.oldMap.get(key);
                if (theNewOne.Name != theOldOne.Name) {
                    System.debug('The name of ' + key + ' has been changed');
                }
            }
        }
    }

