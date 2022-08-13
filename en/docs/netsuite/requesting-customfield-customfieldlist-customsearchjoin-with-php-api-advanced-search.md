---
title: "Requesting customField, customFieldList & customSearchJoin with PHP API Advanced Search"
slug: "requesting-customfield-customfieldlist--customsearchjoin-with-php-api-advanced-search"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

These where some of the hardest things (and least talked about) to do with the PHP API advanced search (where you specify what fields). 

Im in the process of migrating to rest_suite github library that uses RESTLET, and get around the PHP API user concurrency limit of 1. 

But before i delete my old code im posting it here.
Example specs for these field can be found here: http://www.netsuite.com/help/helpcenter/en_US/srbrowser/Browser2016_1/schema/search/transactionsearchrow.html?mode=package

## customField & customFieldList Usage
    $service = new NetSuiteService();
    $search = new TransactionSearchAdvanced();
    $internalId = '123';//transaction internalId

    $search->criteria->basic->internalIdNumber->searchValue = $internalId;
    $search->criteria->basic->internalIdNumber->operator = "equalTo";

    $field = new SearchColumnSelectCustomField();
    $field->scriptId = 'custbody_os_freight_company';//this is specific to you & found in netsuite
    $search->columns->basic->customFieldList->customField[] = $field;
    
    $field = new SearchColumnStringCustomField();
    $field->scriptId = 'custbody_os_warehouse_instructions';//this is specific to you & found in netsuite
    $search->columns->basic->customFieldList->customField[] = $field;

    //and so on, you can keep adding to the customField array the custom fields you want
    
    $request = new SearchRequest();

    $request->searchRecord = $search;

    $searchResponse = $service->search($request);


## customSearchJoin Usage
    $service = new NetSuiteService();
    $search = new TransactionSearchAdvanced();
    $internalId = '123';//transaction internalId
    
    $search->criteria->basic->internalIdNumber->searchValue = $internalId;
    $search->criteria->basic->internalIdNumber->operator = "equalTo";
    
    $CustomSearchRowBasic = new CustomSearchRowBasic();
    $CustomSearchRowBasic->customizationRef->scriptId = 'custbody_os_entered_by';//this is specific to you & found in netsuite
    $CustomSearchRowBasic->searchRowBasic = new EmployeeSearchRowBasic();
    $CustomSearchRowBasic->searchRowBasic->entityId = new SearchColumnStringField();

    $search->columns->customSearchJoin[] = $CustomSearchRowBasic;
    //and so on, you can keep adding to the customSearchJoin array the custom fields you want         
    
    $request = new SearchRequest();
    
    $request->searchRecord = $search;
    
    $searchResponse = $service->search($request);

