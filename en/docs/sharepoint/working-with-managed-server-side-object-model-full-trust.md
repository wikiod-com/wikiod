---
title: "Working with Managed Server Side Object Model (full-trust)"
slug: "working-with-managed-server-side-object-model-full-trust"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

<h1>Conceptual Hierarchy</h1>

In the SharePoint conceptual hierarchy, **site collections** contain **sites**, which in turn contain **lists**. A site collection (`SPSite`) has no explicit UI but always contains one root level site (accessible through the `RootWeb` property) and possibly additional subsites under that root site. A site or web (`SPWeb`) has a UI and contains lists/document libraries (`SPList`), pages with webparts, and items/documents (`SPListItem`).

<h1>Server-Side Caveats</h1>

 - To create an application that uses the SharePoint server-side object model, in your Visual Studio project you must add a reference to the Microsoft.SharePoint assembly which is listed under Framework Assemblies.
 - Applications using the Server Side Object Model (full-trust) can run only on a Windows Server that is hosting SharePoint.
 - You cannot connect to a SharePoint server other than the one the application is running on.

## Hello World (getting site title)
<!-- if version [gte 2013] -->
SharePoint 2013 and newer versions are 64-bit only and so the assembly/program needs to be also built for 64-bit processor. 

Right after your project is created it is necessary to swich the **Platform target** from **Any CPU** to **x64** otherwise error will occure.
<!-- end version if -->

    using System;
    using Microsoft.SharePoint;
    
    namespace StackOverflow
    {
        class Samples
        {
            static void Main()
            {    
                using (SPSite site = new SPSite("http://server/sites/siteCollection"))
                using (SPWeb web = site.OpenWeb())
                {  
                    Console.WriteLine("Title: {0} Description: {1}", web.Title, web.Description);
                }
            }
        }
    }

## Looping through entire SharePoint farm
Using PowerShell executed from a SharePoint Web Server:

<!-- language: powershell -->

    $wacoll = get-spwebapplication
    foreach($wa in $wacoll){
        if($wa.IsAdministrationWebApplication -eq $false){
            foreach($site in $wa.Sites){
                foreach($web in $site.AllWebs){
                    # your code here
                    $web.Dispose()
                }
                $site.Dispose()
            }
        }
    }

## Retrieve list items
    using (SPSite site = new SPSite("http://server/sites/siteCollection"))
    using (SPWeb web = site.OpenWeb())
    {
        SPList list = web.Lists["Some list"];

        // It is always better and faster to query list items with GetItems method with
        // empty SPQuery object than to use Items property
        SPListItemCollection items = list.GetItems(new SPQuery());
        foreach (SPListItem item in items)
        {
            // Do some operation with item
        }
    }

## Retrieve items using paging
    using (SPSite site = new SPSite("http://server/sites/siteCollection"))
    using (SPWeb web = site.OpenWeb())
    {
        SPList list = web.Lists["Some list"];
        SPQuery query = new SPQuery()
        {
            RowLimit = 100
        };

        do
        {
            SPListItemCollection items = list.GetItems(query);
            foreach (SPListItem item in items)
            {
                // Do some operation with item
            }

            // Assign current position to SPQuery object
            query.ListItemCollectionPosition = items.ListItemCollectionPosition;
        } while (query.ListItemCollectionPosition != null);
    }

## Get list by url
    using (SPSite site = new SPSite("http://server/sites/siteCollection"))
    using (SPWeb web = site.OpenWeb())
    {
        string listUrl = string.Format("{0}{1}", web.ServerRelativeUrl, "Lists/SomeList");
        SPList list = web.GetList(listUrl);
    }

## Creating a list item
When creating a new list item, its fields can be set using syntax similar to string arrays. Note that these fields are not created on the fly and are defined by the schema of the list. These fields (or columns) must exist on the server otherwise the create will fail. All list items will have the Title field. Some lists may have required fields that must be filled out before the item will be published in the list.

In this example, the list is using the Announcements template. In addition to the title field, the list includes the Body field that will display the contents of the announcement on the list.

    using (SPSite site = new SPSite("http://server/sites/siteCollection"))
    using (SPWeb web = site.OpenWeb())
    {
        SPList list = web.Lists["Announcements"];

        SPListItem item = list.AddItem();
        item[SPBuiltInFieldId.Title] = "My new item";
        item[SPBuiltInFieldId.Body] = "Hello World!";
        item.Update();
    }

