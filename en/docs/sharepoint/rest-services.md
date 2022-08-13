---
title: "REST Services"
slug: "rest-services"
draft: false
images: []
weight: 9922
type: docs
toc: true
---

<h1>REST Service Endpoint URLs</h1>

The REST client access API was first introduced in SharePoint 2010, but was greatly expanded in SharePoint 2013. The REST API in [SharePoint 2010](https://msdn.microsoft.com/en-us/library/office/ff521587(v=office.14).aspx) is accessed through the ListData web service at the `/_vti_bin/ListData.svc` url. [SharePoint 2013](https://msdn.microsoft.com/en-us/library/office/fp142380.aspx) introduced the `/_api/lists/` and `/_api/web` endpoint URLs, which behave slightly differently.

The above endpoint URLs should be preceded by `http://server/site` where `server` represents the name of the server, and `site` represents the name of, or path to, the specific site.

| Example URL for... | SharePoint 2010 | SharePoint 2013
| ------ | ------ | ------ | 
| **Fetching a List:**  | `/_vti_bin/ListData.svc/ListName` | `/_api/lists('ListGuid')` |
| **Fetching an Item:**  | `/_vti_bin/ListData.svc/ListName(1)` | `/_api/lists('ListGuid')/items(1)` |
| **Fetching a Web:**  | (no equivalent) | `/_api/web`|

Despite the differences in accessing lists and list items, working with those results is very similar in both versions.

Note that the `ListData.svc` service is still available in SharePoint 2013 for backwards compatibility.

<h1>Sending REST Requests</h1>

A REST request can be submitted via a native JavaScript XMLHttpRequest or via the jQuery AJAX wrapper construct.

<h2>XMLHttpRequest Syntax</h2>

    var xhr = new XMLHttpRequest();
    xhr.open(verb, url, true);
    xhr.setRequestHeader("Content-Type","application/json");
    xhr.send(data);

<h2>jQuery AJAX Syntax</h2>

    $.ajax({
        method: verb,
        url: url,
        headers: { "Content-Type":"application/json" },
        data: data
    });

For more details on sending requests via AJAX, see [the JavaScript AJAX documentation](https://www.wikiod.com/javascript/ajax).

## Working with Lists
**Getting List Items**

This example shows how to retrieve all list items and iterate through them.  You can use the `top` parameter to request a certain number of results. You can also use the `select` parameter to select certain fields (`$select=id, Title, uri`).

**JavaScript**

    function GetListItems(){
        $.ajax({
            url: "../_api/web/lists/getbytitle('List Title')/items?$top=50"
            contentType: "application/json;odata=verbose",
            method: "GET",
            headers: { "accept": "application/json;odata=verbose" },
            success: function (data) {
                $.each(data.d.results, function(index,item){
                    //use item to access the individual list item
                    console.log(item.Id);
                });
            },
            error: function(error){
                console.log(error);
            }
        });
        }

**Getting an individual list item**

**JavaScript**

    function GetListItem(){
        $.ajax({
            url: "../_api/web/lists/getbytitle('List Title')/items(1)",
            contentType: "application/json;odata=verbose",
            method: "GET",
            headers: { "accept": "application/json;odata=verbose" },
            success: function (data) {
                console.log(data.d.Id);
            },
            error: function(error){
                console.log(error);
            }
        });
        }



## Get List Items with Lookup Columns
Sometimes, you may have a list structure that looks like this:

# Animal Listing Table
| Name | Type | Description |
| ------ | ------ | ----- |
| Title | String (Text) | Name of the animal |
| Age | Number | How old the animal is |
| Value | Currency | Value of the animal |
| Type | *Lookup (Animal Types Table)* | Lookup Field (Gives dropdown of choices from Animal Types Table) |

# Animal Types Table
| Name | Type | Description |
| ------ | ------ | ----- |
| Title | String (Text) | Name of the species/animal type (ex. Pig) |
| NumLegs | Number | Number of legs on the animal |

Probably not the most serious example but the problem here is still valid. When you use the usual request in order to retrieve values from the SharePoint list, for the `Type` of the animal, you will only get back a field called `TypeId` in the JSON response. In order to expand these items out in just a single AJAX call, some extra markup is required in the URL parameters.

This example applies to more than just lookup columns too. When you are using `People/Groups` columns, they are essentially just lookups as well, so you can pull items such as `Title`, `EMail`, and others easily.

# Example Code

*Important Note*: When you define the fields that you want to get back from the lookup columns, you must prefix the name of the field with the name of the lookup field in the original table. For example, if you want to get back the `NumLegs` attribute from the lookup column, you must type `Type/NumLegs`.

**JavaScript**

    // webUrl: The url of the site (ex. https://www.contoso.com/sites/animals)
    // listTitle: The name of the list you want to query
    // selectFields: the specific fields you want to get back
    // expandFields: the name of the fields that need to be pulled from lookup tables
    // callback: the name of the callback function on success
    function getItems(webUrl,listTitle,selectFields, expandFields, callback){
        var endpointUrl = webUrl + "/_api/web/lists/getbytitle('" + listTitle + "')/items";
        endpointUrl+= '?$select=' + selectFields.join(",");
        endpointUrl+= '&$expand=' + expandFields.join(",");
        return executeRequest(endpointUrl,'GET', callback);
    }

    function executeRequest(url,method,callback,headers,payload) 
    {
        if (typeof headers == 'undefined'){
            headers = {};
        }
        headers["Accept"] = "application/json;odata=verbose";
        if(method == "POST") {
            headers["X-RequestDigest"] = $("#__REQUESTDIGEST").val();
        }   

        var ajaxOptions = 
        {       
        url: url,   
        type: method,  
        contentType: "application/json;odata=verbose",
        headers: headers,
        success: function (data) { callback(data) }
        };
        if(method == "POST") {
        ajaxOptions.data = JSON.stringify(payload);
        }  

        return $.ajax(ajaxOptions);
    }

    // Setup the ajax request by setting all of the arguments to the getItems function
    function getAnimals() {
        var url = "https://www.contoso.com/sites/animals";
        var listTitle = "AnimalListing";
        
        var selectFields = [
            "Title",
            "Age",
            "Value",
            "Type/Title",
            "Type/NumLegs"
        ];

        var expandFields = [
            "Type/Title",
            "Type/NumLegs"
        ];

        getItems(url, listTitle, selectFields, expandFields, processAnimals);
    }

    // Callback function
    // data: returns the data given by SharePoint
    function processAnimals(data) {
        console.log(data);
        // Process data here
    }

    // Start the entire process
    getAnimals();


## Adding selections to a multivalue lookup field
This example assumes that your lookup column is named `MultiLookupColumnName` and that you want to set your multi-lookup field to lookup to the items with IDs 1 and 2.

**Using jQuery AJAX**
<!-- if version [eq 2010] -->
    var listName = "YourListName";
    var lookupList = "LookupListName";
    var idOfItemToUpdate = 1;
    var url = "/server/site/_vti_bin/ListData.svc/"+listName+"("+idOfItemToUpdate+")"; 
    var data = JSON.stringify({
        MultiLookupColumnName:[
            {__metadata:{uri:"http://yoursiteurl/_vti_bin/ListData.svc/"+lookupList+"(1)"}},
            {__metadata:{uri:"http://yoursiteurl/_vti_bin/ListData.svc/"+lookupList+"(2)"}}
        ]
    });
    $.ajax({
        method: 'POST',
        url: url,
        contentType: 'application/json',
        headers: {
          "X-HTTP-Method" : "MERGE",
          "If-Match" : "*"
        },
        data: data
    });

<!-- end version if -->
<!-- if version [gte 2013] --> 
    var listGuid = "id-of-list-to-update"; // use list GUID here
    var lookupGuid = "id-of-lookup-list"; // use lookup list GUID here
    var idOfItemToUpdate = 1;
    var url = "/server/site/_api/lists('"+ listGuid + "')/items("+ idOfItemToUpdate + ")";
    var data = JSON.stringify({
        MultiLookupColumnName:[
            {__metadata:{uri:"http://yoursiteurl/_api/lists('" + lookupGuid + "')/items(1)"}},
            {__metadata:{uri:"http://yoursiteurl/_api/lists('" + lookupGuid + "')/items(2)"}}
        ]
    });
    $.ajax({
        method: 'POST',
        url: url,
        contentType: 'application/json',
        headers: {
          "X-HTTP-Method" : "MERGE",
          "If-Match" : "*"
        },
        data: data
    });

<!-- end version if -->

**Using XMLHttpRequest**

<!-- if version [eq 2010] --> 
    var listName = "YourListName";
    var lookupList = "LookupListName";
    var idOfItemToUpdate = 1;
    var url = "/server/site/_vti_bin/ListData.svc/YourListName("+idOfItemToUpdate+")";
    var data = JSON.stringify({
        MultiLookupColumnName:[
           {__metadata:{uri:"http://yoursiteurl/_vti_bin/ListData.svc/"+lookupList+"(1)"}},
            {__metadata:{uri:"http://yoursiteurl/_vti_bin/ListData.svc/"+lookupList+"(2)"}}
        ]
    });
    var xhr = new XMLHttpRequest();
    xhr.open("POST",url,true);
    xhr.setRequestHeader("X-HTTP-Method", "MERGE");
    xhr.setRequestHeader("If-Match", "*");
    xhr.setRequestHeader("Content-Type","application/json");
    xhr.send(data);
<!-- end version if -->
<!-- if version [gte 2013] --> 
    var listGuid = "id-of-list-to-update";
    var lookupGuid = "id-of-lookup-list";
    var idOfItemToUpdate = 1;
    var url = "/server/site/_api/lists('"+ listGuid + "')/items("+ idOfItemToUpdate + ")"; 
    var data = JSON.stringify({
        MultiLookupColumnName:[
            {__metadata:{uri:"http://yoursiteurl/_api/lists('" + lookupGuid + "')/items(1)"}},
            {__metadata:{uri:"http://yoursiteurl/_api/lists('" + lookupGuid + "')/items(2)"}}
        ]
    });
    var xhr = new XMLHttpRequest();
    xhr.open("POST",url,true);
    xhr.setRequestHeader("X-HTTP-Method", "MERGE");
    xhr.setRequestHeader("If-Match", "*");
    xhr.setRequestHeader("Content-Type","application/json");
    xhr.send(data);
<!-- end version if -->

## Paging list items returned from a query
To simulate paging using REST you can do the following: 

1. Use the `$skip=n` parameter to skip the first `n` entries according to the `$orderby` parameter

2. Use the `$top=n` parameter to return the top `n` entries according to the `$orderby` and `$skip` parameters. 


    var endpointUrl = "/_api/lists('guid')/items"; // SP2010: "/_vti_bin/ListData.svc/ListName";
    $.getJSON(
        endpointUrl + "?$orderby=Id&$top=1000",
        function(data){
            processData(data); // you can do something with the results here
            var count = data.d.results.length;
            getNextBatch(count, processData, onComplete); // fetch next page 
        }
    );

    function getNextBatch(totalSoFar, processResults, onCompleteCallback){
        $.getJSON(
            endpointUrl + "?$orderby=Id&$skip="+totalSoFar+"&$top=1000",
            function(data){
                var count = data.d.results.length;
                if(count > 0){
                    processResults(data); // do something with results
                    getNextBatch(totalSoFar+count, callback); // fetch next page
                }else{
                    onCompleteCallback();
                }
            }
        );
    }


## Retrieve an ID of newly created item in SharePoint list
This example shows how to retrieve an ID of a newly created item using SharePoint REST API.

**Note :** 

**listName** - This variable contains name of you list.


**newItemBody** - This will be your request body for adding new item in list.

e.g. var newItemBody = 
{
     __metadata: { 'type': 'SP.Data.MyListNameItem' },
     Title: 'Some title value'
};


    function CreateListItemWithDetails(listName, newItemBody) {

        var item = newItemBody;
        return $.ajax({
            url: _spPageContextInfo.siteAbsoluteUrl + "/_api/web/lists/getbytitle('" + listName + "')/items",
            type: "POST",
            contentType: "application/json;odata=verbose",
            data: JSON.stringify(item),
            headers: {
                "Accept": "application/json;odata=verbose",
                "X-RequestDigest": $("#__REQUESTDIGEST").val(),
                "content-Type": "application/json;odata=verbose"
            }

        });
    }
    
    CreateListItemWithDetails(listName, newItemBody)
        .then(function(data){
            //success callback
            var NewlyCreatedItemId = data.d.ID;
        }, function(data){
            //failure callback
        });

## How to perform CRUD operations using SharePoint 2010 REST Interface
Create

In order to perform a Create operation via REST, you must perform the following actions:

Create an HTTP request using the `POST` verb.
Use the service URL of the list to which you want to add an entity as the target for the POST.
Set the content type to `application/json`.
Serialize the JSON objects that represent your new list items as a string, and add this value to the request body
JavaScript example:

    function createListItem(webUrl,listName, itemProperties, success, failure) {
    
        $.ajax({
            url: webUrl + "/_vti_bin/listdata.svc/" + listName,
            type: "POST",
            processData: false,
            contentType: "application/json;odata=verbose",
            data: JSON.stringify(itemProperties),
            headers: {
                "Accept": "application/json;odata=verbose"
            },
            success: function (data) {
                success(data.d);
            },
            error: function (data) {
                failure(data.responseJSON.error);
            }
        });
    }

Usage

    var taskProperties = {
        'TaskName': 'Order Approval',
        'AssignedToId': 12
    };
    
    createListItem('https://contoso.sharepoint.com/project/','Tasks',taskProperties,function(task){
        console.log('Task' + task.TaskName + ' has been created'); 
      },
      function(error){
        console.log(JSON.stringify(error));
      }
    );

Read

In order to perform a Read operation via REST, you must perform the following actions:

Create an HTTP request using the `GET` verb.
Use the service URL of the list item to which you want to add an entity as the target for the GET.
Set the content type to `application/json`.
JavaScript example:

    function getListItemById(webUrl,listName, itemId, success, failure) {
        var url = webUrl + "/_vti_bin/listdata.svc/" + listName + "(" + itemId + ")";
        $.ajax({
            url: url,
            method: "GET",
            headers: { "Accept": "application/json; odata=verbose" },
            success: function (data) {
                success(data.d);
            },
            error: function (data) {
                failure(data.responseJSON.error);
            }
        });
    }

Usage

    getListItemById('https://contoso.sharepoint.com/project/','Tasks',2,function(taskItem){
        console.log(taskItem.TaskName); 
      },
      function(error){
        console.log(JSON.stringify(error));
      }
    );

Update

To update an existing entity, you must perform the following actions:

Create an HTTP request using the `POST` verb.
Add an `X-HTTP-Method` header with a value of `MERGE`.
Use the service URL of the list item you want to update as the target for the POST
Add an `If-Match` header with a value of the entity’s original ETag, or *.
JavaScript example:

    function updateListItem(webUrl,listName,itemId,itemProperties,success, failure)
    {
       getListItemById(webUrl,listName,itemId,function(item){
    
          $.ajax({
             type: 'POST',
             url: item.__metadata.uri,
             contentType: 'application/json',
             processData: false,
             headers: {
                    "Accept": "application/json;odata=verbose",
                    "X-HTTP-Method": "MERGE",
                    "If-Match": item.__metadata.etag
             },
             data: Sys.Serialization.JavaScriptSerializer.serialize(itemProperties),
             success: function (data) {
                    success(data);
             },
             error: function (data) {
                    failure(data);
             }
          });
    
       },
       function(error){
           failure(error);
       });
    
    }

Usage

    var taskProperties = {
        'TaskName': 'Approval',
        'AssignedToId': 12  
    };


    updateListItem('https://contoso.sharepoint.com/project/','Tasks',2,taskProperties,function(item){
        console.log('Task has been updated'); 
      },
      function(error){
        console.log(JSON.stringify(error));
      }
    );

Delete

To delete an entity, you must perform the following actions:

Create an HTTP request using the `POST` verb.
Add an `X-HTTP-Method` header with a value of `DELETE`.
Use the service URL of the list item you want to update as the target for the POST
Add an `If-Match` header with a value of the entity’s original ETag.
JavaScript example:

    function deleteListItem(webUrl, listName, itemId, success, failure) {
        getListItemById(webUrl,listName,itemId,function(item){
            $.ajax({
                url: item.__metadata.uri,
                type: "POST",
                headers: {
                    "Accept": "application/json;odata=verbose",
                    "X-Http-Method": "DELETE",
                    "If-Match": item.__metadata.etag
                },
                success: function (data) {
                    success();
                },
                error: function (data) {
                    failure(data.responseJSON.error);
                }
            });
        },
       function (error) {
           failure(error);
       });
    }

Usage

    deleteListItem('https://contoso.sharepoint.com/project/','Tasks',3,function(){
        console.log('Task has been deleted'); 
      },
      function(error){
        console.log(JSON.stringify(error));
      }
    );

