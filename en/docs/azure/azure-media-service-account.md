---
title: "Azure Media Service Account"
slug: "azure-media-service-account"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

A media service account is a Azure based account which gives you access to cloud based media services in Azure.
Stores metadata of the media files you create, instead saving the actual media content.
To work with media service account, you must have an associated storage account.
While creating a media service account, you can either select the storage account you already have or you can create a new one.
Since the media service account and storage account is treated separately, the content will be available in your storage account even if you delete your media service account
Please be noted that your storage account region must be same as your media service account region.

## Creating an asset in media service account
    public static string CreateBLOBContainer(string containerName)
            {
                try
                {
                    string result = string.Empty;
                    CloudMediaContext mediaContext;
                    mediaContext = new CloudMediaContext(mediaServicesAccountName, mediaServicesAccountKey);
                    IAsset asset = mediaContext.Assets.Create(containerName, AssetCreationOptions.None);
                    return asset.Uri.ToString();
                }
                catch (Exception ex)
                {
                    return ex.Message;
                }
            }

## Retrieving the items from the Asset
    private static void GetAllTheAssetsAndFiles(MediaServicesCredentials _medServCredentials)
           {
               try
               {
                   string result = string.Empty;
                   CloudMediaContext mediaContext;
                   mediaContext = new CloudMediaContext(_medServCredentials);
                   StringBuilder myBuilder = new StringBuilder();
                   foreach (var item in mediaContext.Assets)
                   {
                       myBuilder.AppendLine(Environment.NewLine);
                       myBuilder.AppendLine("--My Assets--");
                       myBuilder.AppendLine("Name: " + item.Name);
                       myBuilder.AppendLine("++++++++++++++++++++");
     
                       foreach (var subItem in item.AssetFiles)
                       {
                           myBuilder.AppendLine("File Name: "+subItem.Name);
                           myBuilder.AppendLine("Size: " + subItem.ContentFileSize);
                           myBuilder.AppendLine("++++++++++++++++++++++");
                       }
                   }
                   Console.WriteLine(myBuilder);
               }
               catch (Exception)
               {
                   throw;
               }
           }

