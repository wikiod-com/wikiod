---
title: "Working with JavaScript Client Object Model (JSOM)"
slug: "working-with-javascript-client-object-model-jsom"
draft: false
images: []
weight: 9567
type: docs
toc: true
---

**Background**

The JavaScript Object Model was introduced in SharePoint 2010. It exposes on the client side many of the objects that were previously only accessible through server-side code or through dedicated web services.

**Embedding JavaScript in SharePoint Pages**

In SharePoint 2013 you can put your JavaScript in a Script Editor web part.

In SharePoint 2010 you can use the "content link" property of a Content Editor web part to link to an HTML file that contains your embedded script.

**Object Reference**

The constructors, methods, and properties of all objects found in the `SP` namespace are documented in the SharePoint 2013 client object model reference [here][1].

The SharePoint 2010 JavaScript client object model reference is available [here](https://msdn.microsoft.com/en-us/library/office/ee538253(v=office.14).aspx).

  [1]: https://msdn.microsoft.com/EN-US/library/office/jj246996.aspx

**JSOM's Asynchronous Programming Pattern**

When using the JavaScript client object model, code generally takes the following pattern:

 1. Obtain a `ClientContext` object.
 2. Use the `ClientContext` object to retrieve objects representing entities in the SharePoint object model, such as lists, folder, views.
 3. Queue up instructions to be performed against the objects. These instructions are not transmitted to the server yet.
 4. Use the `load` function to tell the `ClientContext` what information you want to receive back from the server.
 5. Invoke the `ClientContext` object's `executeQueryAsync` function to send the queued instructions to the server, passing two callback functions to run on success or failure.
 6. In the callback function, work with the results returned from the server.

**Alternatives**

Client-side alternatives to the JSOM include SharePoint's web services, [REST endpoints](https://www.wikiod.com/sharepoint/rest-services), and the [.NET client object model](https://www.wikiod.com/sharepoint/working-with-managed-client-side-object-model-csom).

## Getting library content types using the library name
    function getContentTypes(site_url,name_of_the_library){
        var ctx = new SP.ClientContext(site_url);
        var web = ctx.get_web();
        list = web.get_lists().getByTitle(name_of_the_library);

        // You can include any property of the SP.ContentType object (sp.js), for this example we are just getting the name
        ctx.load(list,'ContentTypes.Include(Name)');
        ctx.executeQueryAsync(onQuerySucceeded, onQueryFailed);
    }

    function onQuerySucceeded(sender, args) {
        // var list is the one that we used in function "getContentTypes"
        var contentTypesEnumerator = (list.get_contentTypes()).getEnumerator();

        while (contentTypesEnumerator.moveNext()) {
            var contentType = contentTypesEnumerator.get_current();
            alert(contentType.get_name());
        }
    }

    function onQueryFailed(sender, args) {
        alert('Request failed. ' + args.get_message() + '\n' + args.get_stackTrace());
    }

## Get List Items by CAML Query
<h1>Basic Example</h1>

Use the `set_viewXml` method of the SP.CamlQuery object to specify a CAML query to retrieve items.

    SP.SOD.executeOrDelayUntilScriptLoaded(showListItems,"core.js");

    function showListItems(){
        var clientContext = new SP.ClientContext();
        var list = clientContext.get_web().get_lists().getByTitle("List Title");
        var camlQuery = new SP.CamlQuery();
        camlQuery.set_viewXml(
            "<View><Query>" +
                "<Where>" +
                    "<Eq><FieldRef Name=\"Title\"/><Value Type=\"Text\">Value</Value></Eq>" + 
                "</Where>" +
                "<OrderBy><FieldRef Name=\"Modified\" Ascending=\"FALSE\"/></OrderBy>" +
            "</Query>"+
            //"<RowLimit>5000</RowLimit>" +
            "</View>");
        var items = list.getItems(camlQuery);
        clientContext.load(items);
        clientContext.executeQueryAsync(function(){
            var itemArray = [];
            var itemEnumerator = items.getEnumerator();
            while(itemEnumerator.moveNext()){
                var item = itemEnumerator.get_current();
                var id = item.get_item("ID");
                var title = item.get_item("Title");
                itemArray.push(id + ": " + title);
            }
            alert("ID: Title\n"+itemArray.join("\n"));
        },function(sender,args){alert(args.get_message());});
    }

<h1>Paging the results of a CAML query</h1>

You can take advantage of the `RowLimit` element in a CAML query to retrieve only a subset of results with each query. 

Use the `get_listItemCollectionPosition` method of a list item collection to retrieve the current position, then use that value as the parameter in an SP.CamlQuery object's `set_listItemCollectionPosition` method to retrieve the next batch of results.

    SP.SOD.executeOrDelayUntilScriptLoaded(showListItems,"sp.js");

    function showListItems(){
        var itemArray = [];
        var clientContext = new SP.ClientContext();
        var list = clientContext.get_web().get_lists().getByTitle("List Title");
        var viewXml = 
            "<View><Query>" +
                "<OrderBy><FieldRef Name=\"Modified\" Ascending=\"FALSE\"/></OrderBy>" +
            "</Query>"+
              "<RowLimit>1</RowLimit>" +
            "</View>";
        var camlQuery = new SP.CamlQuery();
        camlQuery.set_viewXml(viewXml);            
        var items = list.getItems(camlQuery);
        clientContext.load(items);
        clientContext.executeQueryAsync(loadResults,showError);

        function loadResults(){
            var resultsFound = false;            
            var itemEnumerator = items.getEnumerator();
            while(itemEnumerator.moveNext()){
                var item = itemEnumerator.get_current();
                var id = item.get_item("ID");
                var title = item.get_item("Title");
                itemArray.push(id + ": " + title);
            }
            var pos = items.get_listItemCollectionPosition();// <- get position
            if(pos !== null){ // <-- position is null when no more results are returned
                if(confirm("Results so far: \nID: Title\n"+itemArray.join("\n"))){
                    camlQuery = new SP.CamlQuery();
                    camlQuery.set_listItemCollectionPosition(pos);// <- set position for next batch
                    camlQuery.set_viewXml(viewXml);            
                    items = list.getItems(camlQuery);
                    clientContext.load(items);
                    clientContext.executeQueryAsync(loadResults,showError);
                }
            }else{
                alert("Total Results: \nID: Title\n"+itemArray.join("\n")); // <- display when no more results
            }
        }
        function showError(sender,args){
            alert(args.get_message());
        }
    }


## Delete an item in a list
    SP.SOD.executeOrDelayUntilScriptLoaded( function(){ deleteItem(1); }, "sp.js");

    function deleteItem(id){
        var clientContext = new SP.ClientContext();
        var list = clientContext.get_web().get_lists().getByTitle("List Title");
        var item = list.getItemById(id);
        item.deleteObject();
        clientContext.executeQueryAsync(function(){
            alert("Item #"+id+" deleted successfully!");
        },function(sender,args){alert(args.get_message());});
    }

## Creating Items or Folders
<h1>Creating List Items</h1>

    SP.SOD.executeOrDelayUntilScriptLoaded(createItem,"sp.js");

    function createItem(){
        var clientContext = new SP.ClientContext();
        var list = clientContext.get_web().get_lists().getByTitle("List Title");
        var newItem = list.addItem();
        newItem.set_item("Title","Example Title");
        newItem.update();
        clientContext.load(newItem); // only needed to retrieve info from newly created item
        clientContext.executeQueryAsync(function(){
            var itemId = newItem.get_item("ID");
            alert("Item #"+itemId+" Created Successfully!");
        },function(sender,args){
            alert(args.get_message());
        });
    }

The example above demonstrates that a list item is created by performing the following:

 1. Call the `addItem` method of a list object to get an item object
 2. Call the `set_item` method on the resulting list item object to set each field value as desired
 3. Call the `update` method on the list item object to indicate that the changes are to be committed
 4. Call the `executeQueryAsync` method of the client context object to execute the queued instructions

Note that you do **not** need to pass the new item object to the client context's `load` method to create the item. That step is only necessary if you wish to retrieve any of the item's field values from the server.
<h1>Creating Folders</h1>

Creating a folder is similar to adding an item to a list. The difference is that one must first create a `ListItemCreationInformation` object and set its `underlyingObjectType` property to `SP.FileSystemObjectType.folder`, and its `leafName` property to the desired name of the new folder. 

The object is then passed as a parameter in the `addItem` method on the library to create the folder.

    // ...
    var itemCreateInfo = new SP.ListItemCreationInformation();
    itemCreateInfo.set_underlyingObjectType(SP.FileSystemObjectType.folder);
    itemCreateInfo.set_leafName(folderName);
    var newItem = list.addItem(itemCreateInfo);
    // ...

To commit the change, invoke the `executeQueryAsync` method of the `ClientContext` object through which the library was accessed.

The full example below creates a folder with a name based on the current timestamp, then opens that folder in a modal dialog.

    SP.SOD.executeOrDelayUntilScriptLoaded(createFolder,"sp.js");

    function createFolder(){
        var now = new Date();
        var timeStamp = now.getYear() + "-" + (now.getMonth()+1) + "-" + now.getDate() 
            + "T" + now.getHours()+"_"+now.getMinutes()+" "+now.getSeconds()+"_"+now.getMilliseconds();
        var clientContext = new SP.ClientContext();
        var list = clientContext.get_web().get_lists().getByTitle("Library Title");
        var itemCreateInfo = new SP.ListItemCreationInformation();
        itemCreateInfo.set_underlyingObjectType(SP.FileSystemObjectType.folder);
        itemCreateInfo.set_leafName(timeStamp);
        var newItem = list.addItem(itemCreateInfo);
        newItem.update();
        clientContext.load(newItem);
        var rootFolder = list.get_rootFolder(); // Note: use a list's root folder to determine its server relative URL
        clientContext.load(rootFolder);
        clientContext.executeQueryAsync(function(){
            var itemId = newItem.get_item("ID");
            var name = newItem.get_item("FileLeafRef");
            SP.UI.ModalDialog.showModalDialog(
                { 
                    title: "Folder \""+name+"\" (#"+itemId+") Created Successfully!", 
                    url: rootFolder.get_serverRelativeUrl() + "/" + name
                }
            ); 
        },function(sender,args){alert(args.get_message());});
    }

## Get Current User Information
    SP.SOD.executeOrDelayUntilScriptLoaded(showUserInfo,"sp.js");

    function showUserInfo(){
        var clientContext = new SP.ClientContext();
        var user = clientContext.get_web().get_currentUser();
        clientContext.load(user);
        clientContext.executeQueryAsync(function(){
                var details = "ID: "+user.get_id()+"\n"+
                    "Title: "+user.get_title()+"\n"+
                    "Login: "+user.get_loginName()+"\n"+
                    "Email: "+user.get_email();
                alert(details);
            },function(sender,args){alert(args.get_message());})
    }

## Get a List Item by ID
<!-- language: lang-js -->
    SP.SOD.executeOrDelayUntilScriptLoaded(myFunction,"sp.js");

    function myFunction(){
        var clientContext = new SP.ClientContext();
        var list = clientContext.get_web().get_lists().getByTitle("List Title");
        var item = list.getItemById(1); // get item with ID == 1
        clientContext.load(item);
        clientContext.executeQueryAsync(
            function(){ // onSuccess
                var title = item.get_item("Title");
                alert(title);
            },
            function(sender,args){ // onError
                alert(args.get_message());
            }
        );
    }

