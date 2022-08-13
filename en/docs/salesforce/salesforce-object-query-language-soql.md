---
title: "Salesforce Object Query Language (SOQL)"
slug: "salesforce-object-query-language-soql"
draft: false
images: []
weight: 9956
type: docs
toc: true
---

## Syntax
- SELECT Id FROM Account 
- SELECT Id, Name FROM Account
- SELECT Id FROM Account WHERE Name = 'SomeAccountName'
- SELECT Id, Name, (SELECT Id, Name FROM Contacts) FROM Account
- SELECT Id, Name FROM Account WHERE Id = :apexVariableName

## Basic SOQL Query
    SELECT Id, Name FROM Account

This will return the Id and Name fields from the Account table. No filtering or sorting will be applied. 

## SOQL Query to Reference Parent Object's Fields
When object's are linked by a lookup or master-detail relationship, the parent records field's can be referenced from the child record or 'base object' in a query. This is also known as upwards traversal.

    SELECT FirstName, Account.Name, Account.Category__c FROM Contact

It's possible to traverse five records upwards.

    SELECT Account.Owner.Profile.CreatedBy.Name FROM Contact

When the base object is a custom lookup field, the __c in field's name Primary_Influencer__c, for example, will be changed to __r.

    SELECT Primary_Influencer__r.Nickname__c FROM Contact


SOQL Query to get child Records 

    SELECT Id, Name, (SELECT Id, FirstName, LastName FROM Contacts) FROM Account

## SOQL Query With Ordering
    SELECT Id, Name FROM User ORDER BY LastName

    SELECT Id, Name FROM Contact ORDER BY LastModifiedDate DESC

    SELECT Name, Title FROM User ORDER BY Title ASC NULLS FIRST

    SELECT Id FROM Contact ORDER BY LastName ASC NULLS LAST, FirstName ASC NULLS FIRST

## Using SOQL to Construct a Map
A very useful feature many people overlook is the ability to construct a Map using a SOQL query.

    Map<Id, Account> accounts = new Map<Id, Account>([SELECT Id, Name FROM Account]);
    System.debug(accounts);

When you run this code, `accounts` then contains a Map of your Account objects, keyed on Id. The output to the debug log would look similar to this:

    11:15:10:025 USER_DEBUG [13]|DEBUG|{
        XXXXXXXXXXXXXXXXXX=Account:{Id=XXXXXXXXXXXXXXXXXX, Name=Account 1}, 
        YYYYYYYYYYYYYYYYYY=Account:{Id=YYYYYYYYYYYYYYYYYY, Name=Account 2}, 
        ZZZZZZZZZZZZZZZZZZ=Account:{Id=ZZZZZZZZZZZZZZZZZZ, Name=Account 3}, 
        ...
    }

You are now able to look up the Account objects using their Id. Furthermore, if you want a collection of unique IDs, you can call the `keySet()` function of the Map class, like so:

    System.debug(accounts.keySet());

which looks something like this in the debug log:

    11:23:21:010 USER_DEBUG [15]|DEBUG|{XXXXXXXXXXXXXXXXXX, YYYYYYYYYYYYYYYYYY, ZZZZZZZZZZZZZZZZZZ, ...}

This is very useful when you need to query to get records and access them repeatedly in your code.

## SOQL Query With Filtering
    SELECT Name FROM User WHERE IsActive = true

This will return the name of all active Users. 

    SELECT Name, Phone FROM Contact WHERE CreatedDate >= 2016-01-01T00:00:00.000Z

This will return Contacts created on or after January 1st, 2016.

    SELECT Id, Name FROM Account LIMIT 100

This will return the first 100 Accounts from an unordered list.

    SELECT Id, Name, Phone FROM Lead WHERE Phone LIKE '(%) %-%'

This will return Leads with a phone number matching the specified format. "%" acts as a wild card character.

Using `LIKE '% %'` also enables a developer to replicate a `CONTAINS( )` formula.

    SELECT Email FROM Lead WHERE LeadSource LIKE '%Google%'

Will return Leads with a lead source that contains Google i.e. 'Google AdWords' & 'Google Natural Search'.

## Dynamic SOQL
You can execute a database query from a String rather than a regular SOQL expression:

```
String tableName = 'Account';
String queryString = 'SELECT Id FROM ' + tableName + ' WHERE CreatedDate >= YESTERDAY';
List<SObject> objects = Database.query(queryString);
```

Since dynamic SOQL queries are not compiled, their schema references are not validated, so it is preferable to use Apex variable interpolation using the `:variable` syntax where possible.

## SOQL Queries in Apex
To perform a query in Apex, surround the query with square brackets. The result can be assigned to a list, or to a single object.

    List<Account> allAccounts = [SELECT Id, Name FROM Account];
    Account oldestAccount = [SELECT Id, Name FROM Account ORDER BY CreatedDate LIMIT 1];

## Variable References in Apex SOQL Queries
To reference a variable in a query, add a colon (:) before the variable name.

    Datetime targetDate = Datetime.now().addDays(-7);
    List<Lead> recentLeads = [SELECT Id FROM Lead WHERE CreatedDate > :targetDate];

    string targetName = 'Unknown';
    List<Contact> incompleteContacts = [SELECT Id FROM Contact WHERE FirstName = :targetName];

## Potential Exceptions in Apex SOQL Queries
When assigning to a single object, a query that returns anything other than a single row will throw a `QueryException`.

    try {
        Account a = [SELECT Id FROM Account WHERE Name = 'Non-existent Account'];  
    } catch (QueryException e) {
        // List has no rows for assignment to SObject
    }

    try {
        Account a = [SELECT Id FROM Account];  
    } catch (QueryException e) {
        // List has more than 1 row for assignment to SObject
    }

Attempting to use a field that you did not include in the query will throw a `SObjectException`

    Account a = [SELECT Id FROM Account LIMIT 1];
    try {
        System.debug( a.Name );
    } catch (SObjectException e) {
        // SObject row was retrieved via SOQL without querying the requested field: Name
    }



## Using a Semi-Join
Selecting all accounts that have open opportunity records under them
```
SELECT Id, Name FROM Account WHERE AccountId IN 
    (SELECT Id FROM Opportunity WHERE IsClosed = false)
```

