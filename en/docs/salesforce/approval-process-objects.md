---
title: "Approval Process Objects"
slug: "approval-process-objects"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Approval Process is a very amazing feature in Salesforce to automate the business process. An approval process is a set of  the steps necessary for a particular record to be approved or rejected by approver or set of approvers.

A step can apply to all records included in the process, or just records that meet certain administrator-defined criteria. An approval process also specifies the actions to take when a record is approved, rejected, recalled, or first submitted for approval.

> ProcessDefinition and ProcessNode objects act as a template and store
> the master configurations for Approval Process itself.

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/tccW7.jpg

## ProcessDefinition
Represents the definition of a single approval process. Use this object to read the description of an approval process. The definition is read-only. We can not modify the record created in ProcessDefinition Object. But we can describe, query, search and retrieve the approval processes information.

**~ Query ~**

    SELECT CreatedById,CreatedDate,Description,DeveloperName,LastModifiedById,
    LastModifiedDate,LockType,Name,State,SystemModstamp,TableEnumOrId,Type,Id
    FROM ProcessDefinition

The records are created when we create a new approval process using Salesforce user interface of Approval Process.



## ProcessNode
Represents the Process Steps created for particular approval process(ProcessDefinition). This object is used to read the description of process step. In simple words ProcessNode records describes a step in a process definition. We can describe, query, search and retrieve the approval processes Steps.

**~ Query ~**

    SELECT Description,DeveloperName,Name,ProcessDefinitionId,SystemModstamp
    ,Id,FROM ProcessNode

As we can see ProcessDefinitionId field is acting like a foreign key which is referring ProcessDefinition Object or Table for which steps or process nodes are created. This object is also read only as ProcessDefinition Object.

## ProcessInstance
Represents an instance of a single, complete approval process. ProcessInstance record is created every time for particular object record which is submitted for approval. Its is also read-only object. We can describe, query and retrieve the approval processes Instance.

**~ Query ~**

    SELECT CompletedDate,CreatedById,CreatedDate,ElapsedTimeInDays,
    ElapsedTimeInHours,ElapsedTimeInMinutes,Id,IsDeleted,LastActorId,
    LastModifiedById,LastModifiedDate,ProcessDefinitionId,Status,
    SubmittedById,SystemModstamp,TargetObjectId FROM ProcessInstance

All ProcessInstance fields are automatically populated once the record is submitted for approval, with two exceptions fields: CompletedDate and LastActorId that are populated only after the approval process instance is complete. The ProcessDefinitionId field is the reference or foreign key ID of the ProcessDefinition Object.

## ProcessInstanceStep & ProcessInstanceWorkitem
Both objects ProcessInstanceStep & ProcessInstanceWorkItem are instances of process steps that are created for particular ProcessInstance. ProcessInstanceStep represents a step instance in an approval process (ProcessInstance) on which users has already acted and ProcessInstanceWorkItem represents a step instance in an approval process(ProcessInstance) on which is pending and users has to perform some action next on it. We can describe, query and retrieve the approval processes steps and workItems.

**~ Query ~**

    SELECT CreatedById,CreatedDate,ElapsedTimeInDays,ElapsedTimeInHours,
    ElapsedTimeInMinutes,Id,IsDeleted,OriginalActorId,ProcessInstanceId,
    ActorId,SystemModstamp FROM ProcessInstanceWorkitem 

    SELECT ActorId,Comments,CreatedById,CreatedDate,ElapsedTimeInDays,Id, 
    ElapsedTimeInHours,ElapsedTimeInMinutes,OriginalActorId,ProcessInstanceId
    ,StepNodeId,StepStatus,SystemModstamp FROM ProcessInstanceStep

 

## ProcessInstanceHistory*
The ***ProcessInstanceHistory*** is the object which is neither searchable nor queryable & this is the read-only object which shows all steps and pending approval requests associated with an approval process (ProcessInstance). <i>But we can use this object to replicate the related list functionality of the Salesforce user interface for approval processes which will be shown in my next blog post soon.</i> We can use ProcessInstanceHistory for a single read-only view of the both ProcessInstanceStep and ProcessInstanceWorkitem objects. We can query ProcessInstanceHistory by querying it in a nested soql query on the parent ProcessInstance object. The nested soql query references ***StepsAndWorkitems***, which is the child relationship name for ProcessInstanceHistory in the ProcessInstance object. This is very useful object to solve various business problems.

**~ Query ~**

    SELECT CompletedDate, CreatedById, CreatedDate,Id,IsDeleted,LastActorId,
    LastModifiedById,LastModifiedDate,ProcessDefinitionId,Status,SubmittedById
    ,SystemModstamp,TargetObjectId, (SELECT ID, ProcessNodeId, StepStatus,
    Comments,TargetObjectId,ActorId,CreatedById,IsDeleted,IsPending,
    OriginalActorId,ProcessInstanceId,RemindersSent,CreatedDate 
    FROM StepsAndWorkitems ) FROM ProcessInstance

