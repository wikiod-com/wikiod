---
title: "Azure Storage Options"
slug: "azure-storage-options"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Connecting to an Azure Storage Queue
Storage options in Azure provide a "REST" API (or, better, an HTTP API)

The Azure SDK offers clients for several languages. Let's see for example how to initialize one of the storage objects (a queue) using the C# client libraries.

All access to Azure Storage is done through a storage account. You can create a storage account in several ways: through the portal, through the Azure CLI, PowerShell, Azure Resource Manager (ARM), ...

In this example we suppose you already have one, and you have stored it in your `app.config` file.

    // Retrieve storage account from connection string.
    CloudStorageAccount storageAccount = CloudStorageAccount.Parse(
        CloudConfigurationManager.GetSetting("StorageConnectionString"));

Queues are reachable at the following URL:
`http://<storage account>.queue.core.windows.net/<queue>`

The client libraries will generate this URL for you; you just need to specify the queue name (which must be lowercase). The first step is to get a reference to a queue client, which will be used to manage your queues (queues are contained in the specified storage account).

    CloudQueueClient queueClient = storageAccount.CreateCloudQueueClient();

You use the client to get a reference to a queue.

    CloudQueue queue = queueClient.GetQueueReference("<queue>");

Now, using this `queue` proxy, you can direct any operation to your queue.
    
Typically, the first operation is to create the queue if it doesn't already exist

    queue.CreateIfNotExists();

Notice the name of the operation. Why "if not exists"? There are several reasons:
- you may be deploying multiple instances of "something" that will run this code ("something" is typically a [Compute Service](https://azure.microsoft.com/en-us/documentation/articles/cloud-services-choose-me/), like a Web Role or a Worker role, but it can be a Web App, a Fabric Service, some custom code in a VM...)
- your App may reboot at any time. Remember, this is a cloud environment where, especially for PaaS services, instances are ephemeral. You do not have the same degree of control over your app as you would have on your locally deployed app.

Even better, you should use the async version of the same API call:

    await queue.CreateIfNotExistsAsync();

We have used a queue in this example, but the example can be easily applied to the other storage objects (blobs, tables, and files).

Once you have created your storage object, you are ready to start using it.

