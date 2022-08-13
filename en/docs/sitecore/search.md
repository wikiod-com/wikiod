---
title: "Search"
slug: "search"
draft: false
images: []
weight: 9979
type: docs
toc: true
---


Sitecore search is built on top of Lucene, providing the ability to create very fast searching capabilities for your site. Instead of querying against a centralised database (such as Sitecore's SQL DB), it queries Lucene index files which are stored on the physical file system of the web-server. Sitecore provides a layer of abstraction over the Lucene.NET API including a LINQ provider which makes writing Lucene queries a simple & familiar process for .NET developers. Sitecore ships with some standard indexes configured which you can extend or define your own. You can also opt to use SOLR; a centralised & scalable platform built on top of Lucene.

## Create an inbound filter for the search
Inbound filter is applied when item is added to the search index and allows to specify whether item is included to the index or not.

Examples when inbound filter can be used - do not include standard values and previous versions of the item in the index.

Inbound filters are set in configuration:

    <indexing.filterIndex.inbound>
     <processor type="Sitecore.ContentSearch.Pipelines.IndexingFilters.ApplyInboundIndexFilter, Sitecore.ContentSearch"></processor>
    </indexing.filterIndex.inbound>  

Code implementation:

    public class ApplyInboundIndexVersionFilter : InboundIndexFilterProcessor   
    {        
        public override void Process(InboundIndexFilterArgs args)       
        { 
            var item = args.IndexableToIndex as SitecoreIndexableItem;
    
            if (!item.Item.Versions.IsLatestVersion())            
            {                  
                args.IsExcluded = true;            
            }
        }
    }

More examples and information could be found on http://www.sitecore.net/learn/blogs/technical-blogs/sitecore-7-development-team/posts/2013/04/sitecore-7-inbound-and-outbound-filter-pipelines.aspx




## Create an outbound filter for the search
Outbound filter can be used to filter the search results.

One of the examples of outbound filter usage is removing items which user doesn't have access to from search results.

Outbound filters are set in the configuration:

    <indexing.filterIndex.outbound>
     <processor type="Sitecore.ContentSearch.Pipelines.IndexingFilters.ApplyOutboundSecurityFilter, Sitecore.ContentSearch"></processor>
    </indexing.filterIndex.outbound>

Example of outbound filter implementation:

    public class ApplyOutboundIndexWorkflowFilter : OutboundIndexFilterProcessor   
    {        
        public override void Process(OutboundIndexFilterArgs args)       
        { 
            //You can use args.IsExcluded to remove items from the search results here
        }
    }

More examples and information could be found on http://www.sitecore.net/learn/blogs/technical-blogs/sitecore-7-development-team/posts/2013/04/sitecore-7-inbound-and-outbound-filter-pipelines.aspx

## Delete all previous versions of the item in the index when adding new version
By default Sitecore adds all versions of the item to the sitecore_master_index. The drawback of that is that if users are using workflows and adding lots of versions all of them will be added to the search results in the content editor.


Configuration:

    <event name="item:versionAdded" >
                  <handler type="FilterPatch.Library.ContentSearch.EventHandler, AssemblyName" method="Execute" />
           </event>

Handler implementation

    public class EventHandler
        {
            public void Execute(object sender, EventArgs eventArgs)
            {
                var item = Event.ExtractParameter(eventArgs, 0) as Item;
    
                //If item has less than 2 versions - then skip
                if(item.Versions.Count < 2)
                {
                    return;
                }
    
                var indexableItem = new SitecoreIndexableItem(item);
    
                var index = ContentSearchManager.GetIndex(indexableItem);
    
                using (var context = index.CreateDeleteContext())
                {
                    foreach(var version in item.Versions.GetVersions(true))
                    {
                        if(!version.Versions.IsLatestVersion())
                        {
                            var indexableItemVersion = new SitecoreIndexableItem(version);
                            context.Delete(indexableItemVersion.UniqueId);
                        }
                    }
                    context.Commit();                
                }
            }
        }



## Configuration
Sitecore ships with a set of standard indexes pre-configured which you can extend, or you can define your own. Of the pre-configured, `sitecore_master_index` & `sitecore_web_index` are of most interest for your site search. These are the predefined indexes for all of your Sitecore items in the tree of your master & web databases respectively, and are configured to store all of the standard fields of a Sitecore item that will be common among all templates. 

You can look at this configuration of the standard web index at this location: `<Your Site>\App_Config\Include\Sitecore.ContentSearch.Lucene.Index.Web.config`

The main areas of importance of an index's configuration are:

- *The Field Configuration* - What fields should be stored in the index, and how they should be stored.
- *The Strategy* - How and when should the index be updated.
- *The Crawler* - The location where the index can get it's Sitecore data

**Field Configuration**

Looking in the `sitecore_web_index` config, you can see the following reference: `<configuration ref="contentSearch/indexConfigurations/defaultLuceneIndexConfiguration" />`. This refers to a shared index configuration file found here: `<Your Site>\App_Config\Include\Sitecore.ContentSearch.Lucene.DefaultIndexConfiguration.config`. Here you can see all of the fields that are included in the standard config.

There are basically two ways to defined a field: either the field is sourced directly from a Sitecore item field, or it's a computed field. A computed field allows you write some code to do some calculations and store the result in the field. This code will get executed when the index is built/updated *not* when the index is queried. This is particularly useful if the field needs to store aggregated data, such as counts etc. 

Within the `<fieldMap>` element you'll see the elements `<fieldNames hint="raw:AddFieldByFieldName">` & `<fields hint="raw:AddComputedIndexField">`, which contain the directly sourced fields and the computed fields respectively.

**Strategy**

The strategy of your index determines when your index is updated. There are the following options to choose from:

 - **OnPublishEndAsynchronousStrategy (onPublishEndAsync)** - When an item is published, the index will be updated asynchronously. 
 - **SynchronousStrategy (syncMaster)** - When an item is saved, the index will be updated instantly and synchronously.
 - **IntervalAsynchronousStrategy (intervalAsyncCore/intervalAsyncMaster)** - Periodically check for item updates & update the index asynchronously
 - **ManualStrategy** - No automatic index updates. Indexes will only be updated manually (through the control panel, or programmatically)
 - **RebuildAfterFullPublishStrategy (rebuildAfterFullPublish)** - After a publish, the index will be rebuilt in full
 - **RemoteRebuildStrategy (remoteRebuild)** - This strategy is for multiple instances of Sitecore. For example, if a rebuild is called for from the content management server, then the remote content delivery servers will subscribed to this event and rebuild their own indexes.

By default, the master index is configured as `syncMaster`. This is because if you're in the experience editor saving items and rendering on the page displays the results of an index, you'll want to see changes you've made to items immediately in the results. The web index is configured as 'onPublishEndAsync', this is because the indexes of your web database only need updating when items are published from the master database to the web.

You can also combine multiple strategies. For example, if you have separate Sitecore instances for your content management (CM) & content delivery (CD), it would make sense to combine the `onPublishEndAsync` with `remoteRebuild`, so that CD indexes get updated when items are published as well as get rebuilt when a user triggers a rebuild from the control panel of the CM server.

You can choose your strategy using the following config:


    <strategies hint="list:AddStrategy">
        <strategy ref="contentSearch/indexConfigurations/indexUpdateStrategies/onPublishEndAsync" />
    </strategies>

**The Crawler**

This allows you to specify the location of the Sitecore data you want to index. The web index has the following default configuration:

    <locations hint="list:AddCrawler">
        <crawler type="Sitecore.ContentSearch.SitecoreItemCrawler, Sitecore.ContentSearch">
            <Database>web</Database>
            <Root>/sitecore</Root>
        </crawler>
    </locations> 

The two important bits are the `<Database>` and `<Root>` elements. The `<Root>` element allows you to specify the starting position in your Sitecore tree that the index should index. In reality, you're likely to have a 'Home' node under the content node that you'd point this at so that it only indexes actual content/pages rather than your templates etc.

