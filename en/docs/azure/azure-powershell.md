---
title: "Azure Powershell"
slug: "azure-powershell"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Login to Azure
**Classic (Service Management) mode:**

    Add-AzureAccount

This will authenticate you using Azure Active Directory, and PowerShell gets an access token that expires after about 12 hours. So you must repeat the authentication after 12 hours.

An alternative is to run the following cmdlet:

    Get-AzurePublishSettingsFile

This opens a browser window where you can download a publish settings file. This file contains a certificate that allows PowerShell to authenticate. Then you can import your publish settings file using the following:

    Import-AzurePublishSettingsFile

Remember that the publish settings file contains a certificate with effectively admin privileges in your subscription. Keep it secure or delete it after using it.

**Resource manager**

In Resource Manager side, we can only use Azure Active Directory authentication with the 12 hour access tokens. There are two alternative commands currently that you can use:

    Login-AzureRmAccount
    Add-AzureRmAccount

## Selecting subscription
When you have multiple subscriptions under your Azure account; it's important that you are selecting the one you wish to operate on (and use this as default); to avoid accidents happening to resources on the wrong subscription. 

**Classic mode**

    Set-AzureSubscription
    Select-AzureSubscription 

**Resource manager**

    Select-AzureRmSubscription

**Subscription information**

The commands above ask you to specify information (e.g., the Subscription ID) to identify the subscription you want to switch to. To list this information for the subscriptions you have access to, run this command:

    Get-AzureSubscription


## Classic mode vs ARM mode
When working with Azure using PowerShell there are 2 different ways you should be aware of and you will see a lot of the Microsoft documentation referring to both of them:

**"Classic mode (Service Management)"** 

This is the old way of operating Azure and managing Azure. There is still some services in Azure that can only be managed using the classic mode, even though more and more services are moving towards the new ARM mode. 

To list modules installed on your machines operating under classic mode you can do the following:

    Get-Module -ListAvailable Azure.*

**"Resource manager (ARM) "** 

This is the new way of managing Azure (Based on the REST API's provided). Most of the services you could manage in Azure from Powershell classic mode can now be managed using the new mode with some exceptions. This should be the preferred way of managing your Azure resources unless you have certain services that are not supported under ARM mode yet.

To list the modules installed on your machine containing commands to operate in ARM mode are under you can do the following : 

    Get-Module -ListAvailable AzureRM*



## Get the Current Azure PowerShell Version
To determine the version of Azure PowerShell that you have installed, run the following:

    Get-Module -ListAvailable -Name Azure -Refresh

This command returns the installed version even when you haven't loaded the Azure PowerShell module in your current PowerShell session.


## Manipulate Azure Assets
Azure Cmdlets let you perform some of the same actions on Azure assets through PowerShell that you would using C# code or the Azure portal.

For example, these steps let you download the contents of an Azure blob into a local directory:

    New-Item -Path .\myblob -ItemType Directory
    $context = New-AzureStorageContext -StorageAccountName MyAccountName -StorageAccountKey {key from the Azure portal}
    $blob = Get-AzureStorageBlob -Container MyContainerName -Context $context
    $blob | Get-AzureStorageBlobContent -Destination .\myblob\
    

## Managing Traffic Managers
With Azure PowerShell you can get certain functionality currently unavailable on [Azure Portal](https://portal.azure.com), like:

 - Reconfigure all Traffic Manager's endpoints at once
 - Address other services via Azure `ResourceId` instead of domain name, so you don't need to set Location manually for Azure Endpoints

## Prerequisites

To start you need to [login](https://www.wikiod.com/azure/azure-powershell#Login to Azure) and [select RM subscription](https://www.wikiod.com/azure/azure-powershell#Selecting subscription).

## Get TrafficManager profile
Operations with Traffic Managers via PowerShell are done in three steps:

 1. Get TM profile:<br>`$profile = Get-AzureRmTrafficManagerProfile -ResourceGroupName my-resource-group -Name my-traffic-manager`<br>Or create new as [in this article](https://docs.microsoft.com/en-us/powershell/resourcemanager/azurerm.trafficmanager/v2.2.0/new-azurermtrafficmanagerprofile).
 2. Explore and modify TM profile<br>Check `$profile` fields and `$profile.Endpoints` to see each endpoint's configuration.
 3. Save changes via `Set-AzureRmTrafficManagerProfile -TrafficManagerProfile $profile`.

## Change endpoints
All current endpoints are stored in `$profile.Endpoints` list, so you can alter them directly by index<br>`$profile.Endpoints[0].Weight = 100`<br>or by name<br> `$profile.Endpoints | ?{ $_.Name -eq 'my-endpoint' } | %{ $_.Weight = 100 }`

To clear all endpoints use<br>
`$profile.Endpoints.Clear()`

To delete particular endpoint use<br>
`Remove-AzureRmTrafficManagerEndpointConfig -TrafficManagerProfile $profile -EndpointName 'my-endpoint'`

To add new endpoint use<br>
`Add-AzureRmTrafficManagerEndpointConfig -TrafficManagerProfile $profile -EndpointName "my-endpoint" -Type AzureEndpoints -TargetResourceId "/subscriptions/00000000-0000-0000-0000-000000000000/resourceGroups/my-resource-group/providers/Microsoft.ClassicCompute/domainNames/my-azure-service" -EndpointStatus Enabled -Weight 100`

As you can see, in the last case we've addressed our azure service via ResourceId rather than domain name.

## Keep in mind

Your changes to TM and it's endpoints are not applied until you'll invoke `Set-AzureRmTrafficManagerProfile -TrafficManagerProfile $profile`. That allows you to fully reconfigure TM in one operation.

Traffic Manager is an implementation of [DNS](https://en.wikipedia.org/wiki/Domain_Name_System) and IP address given to clients has some time to live (aka TTL, you can see it's duration in seconds in the `$profile.Ttl` field). So, after you've reconfigured TM some clients will continue to use old endpoints they cached until that TTL expire.

