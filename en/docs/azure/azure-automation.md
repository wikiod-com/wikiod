---
title: "Azure-Automation"
slug: "azure-automation"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Parameters
| Parameter Name | Description |
| -------------- | ------------|
| resourceGroupName | The Azure Resource group where the storage account sits |
| connectionName | The Azure Run As connection (service pricipal) that was created when the automation account was created |
| StorageAccountName | The name of the Azure Storage account |
| ContainerName | The blob container name |
| DaysOld | The number of days a blob can be before it is deleted |

Ensure you have access to Azure Active Directory so, on Automation Account creation, Azure creates a RunAs account for you. This will save you a lot of trouble.

## Delete Blobs in Blob storage older than a number of days
Here is an example of an Azure Powershell automation runbook that deletes any blobs in an Azure storage container that are older than a number of days.

This may be useful for removing old SQL backups to save cost and space. 

It takes a number of parameters which are self explanatory.

**Note: I have left some commented out code to help with debugging.**

It uses a service principal that Azure can set up for you automatically when you create your automation account. You need to have Azure Active Directory access. See pic:

<img src="http://i.stack.imgur.com/9nNXJ.png" width="200" height="475">

    <#
    .DESCRIPTION
        Removes all blobs older than a number of days back using the Run As Account (Service Principal)

    .NOTES
        AUTHOR: Russ
        LASTEDIT: Oct 03, 2016   #>

    param(
        [parameter(Mandatory=$true)]
        [String]$resourceGroupName,

        [parameter(Mandatory=$true)]
        [String]$connectionName,

        # StorageAccount name for content deletion.
        [Parameter(Mandatory = $true)] 
        [String]$StorageAccountName,

        # StorageContainer name for content deletion.
        [Parameter(Mandatory = $true)] 
        [String]$ContainerName,

        [Parameter(Mandatory = $true)]
        [Int32]$DaysOld

    )
    $VerbosePreference = "Continue";
    try
    {
    # Get the connection "AzureRunAsConnection "
    $servicePrincipalConnection=Get-AutomationConnection -Name $connectionName         

    "Logging in to Azure..."
    Add-AzureRmAccount `
        -ServicePrincipal `
        -TenantId $servicePrincipalConnection.TenantId `
        -ApplicationId $servicePrincipalConnection.ApplicationId `
        -CertificateThumbprint $servicePrincipalConnection.CertificateThumbprint 
    catch {
    if (!$servicePrincipalConnection)
    {
        $ErrorMessage = "Connection $connectionName not found."
        throw $ErrorMessage
    } else{
        Write-Error -Message $_.Exception
        throw $_.Exception
    }
    $keys = Get-AzureRMStorageAccountKey -ResourceGroupName $resourceGroupName -AccountName $StorageAccountName
    # get the storage account key
    Write-Host "The storage key is: "$StorageAccountKey;
    # get the context
    $StorageAccountContext = New-AzureStorageContext -storageAccountName $StorageAccountName -StorageAccountKey $keys.Key1 #.Value;
    $StorageAccountContext;
    $existingContainer = Get-AzureStorageContainer -Context $StorageAccountContext -Name $ContainerName;
    #$existingContainer;
    if (!$existingContainer)
    {
     "Could not find storage container";
    } 
    else 
    {
    $containerName = $existingContainer.Name;
    Write-Verbose ("Found {0} storage container" -f $containerName);
    $blobs = Get-AzureStorageBlob -Container $containerName -Context $StorageAccountContext;
    $blobsremoved = 0;

    if ($blobs -ne $null)
    {    
        foreach ($blob in $blobs)
        {
            $lastModified = $blob.LastModified
            if ($lastModified -ne $null)
            {
                #Write-Verbose ("Now is: {0} and LastModified is:{1}" –f [DateTime]::Now, [DateTime]$lastModified);
                #Write-Verbose ("lastModified: {0}" –f $lastModified);
                #Write-Verbose ("Now: {0}" –f [DateTime]::Now);
                $blobDays = ([DateTime]::Now - $lastModified.DateTime)  #[DateTime]

                Write-Verbose ("Blob {0} has been in storage for {1} days" –f $blob.Name, $blobDays);

                Write-Verbose ("blobDays.Days: {0}" –f $blobDays.Hours);
                Write-Verbose ("DaysOld: {0}" –f $DaysOld);

                if ($blobDays.Days -ge $DaysOld)
                {
                    Write-Verbose ("Removing Blob: {0}" –f $blob.Name);

                    Remove-AzureStorageBlob -Blob $blob.Name -Container $containerName -Context $StorageAccountContext;
                    $blobsremoved += 1;
                }
                else {
                    Write-Verbose ("Not removing blob as it is not old enough.");
                }
            }
        }
    }

    Write-Verbose ("{0} blobs removed from container {1}." –f $blobsremoved, $containerName);
    }

It you use the test pane you can enter the required parameters and run it.

[![enter image description here][2]][2]

As you can see, when I ran it, it didn't find any blobs that were old enough to delete.


  [1]: http://i.stack.imgur.com/9nNXJ.png
  [2]: http://i.stack.imgur.com/3C3gl.png

## Index maintenance
If you care about your indices (yes, that's the plural of index), you should maintain them, especially if you do frequent inserts or deletes that affect them.
Azure Automation provides an almost ready runbook that you can use and schedule to perform automated index rebuilding. Here's what you have to do:
First, import the runbook:
![](https://dotnetfalconcontent.blob.core.windows.net/azure-automation-job-for-index-maintenance/step02.png)

After the runbook is imported, you have to go into edit mode and press publish on the runbook, and it becomes active. In edit mode, you can also check out the source code of the runbook.
![](https://dotnetfalconcontent.blob.core.windows.net/azure-automation-job-for-index-maintenance/step03.png)

Then you have to add a credential to the runbook, which can be used to connect to the database (basically it is just a key-value pair, where the value is a username and a password, and the key can be used from the script to reference this particular credential). This has to be a user-password pair that can authenticate to the database, and the user should have rights to access database state and run the ALTER INDEX statement:
![](https://dotnetfalconcontent.blob.core.windows.net/azure-automation-job-for-index-maintenance/step04.png)
And finally schedule the runbook with the specified parameters. You can also test the runbook and start it immediately (but you have to specify the parameters in this case too):

![](https://dotnetfalconcontent.blob.core.windows.net/azure-automation-job-for-index-maintenance/step05.png)

Now unfortunately, I have found that there are two problems with this default runbook.

First, it only handles tables that are in the default schema. That's not always enough, so go ahead and find the line:

    SELECT  t.name AS TableName, t.OBJECT_ID FROM sys.tables t

And change it to this:

    SELECT  '['+SCHEMA_NAME(t.schema_id)+'].['+t.name+']' AS TableName, t.OBJECT_ID FROM sys.tables t

Also, the script cannot handle special characters anywhere in the connection string (like ",' or =). To handle these, you can use the proper connection string escaping, or better yet, use the connectionstring builder. Whereever you see a connection string created in the script (there should be two places), change it to use the connectionstring builder:

    $connStringBuilder = New-Object System.Data.SqlClient.SqlConnectionStringBuilder
    $connStringBuilder["Server"] = "tcp:$using:SqlServer,$using:SqlServerPort"
    $connStringBuilder["Database" ] = "$using:Database"
    $connStringBuilder["User ID"] = "$using:SqlUsername"
    $connStringBuilder["Password"] = "$using:SqlPass"
    $connStringBuilder["Trusted_Connection"] = $False
    $connStringBuilder["Encrypt"] = $True
    $connStringBuilder["Connection Timeout"] = "30"
    $connString = $connStringBuilder.ConnectionString                
    $Conn = New-Object System.Data.SqlClient.SqlConnection($connString)

You can also change the query that actually calculates fragmentation a bit. This is the query:

    SELECT a.object_id, avg_fragmentation_in_percent
        FROM sys.dm_db_index_physical_stats (
               DB_ID(N'$Database')
             , OBJECT_ID(0)
             , NULL
             , NULL
             , NULL) AS a
        JOIN sys.indexes AS b 
        ON a.object_id = b.object_id AND a.index_id = b.index_id;

If you change the last parameter of sys.dm_db_index_physical_stats from NULL to 'DETAILED', you get a much better estimation of how fragmented the indices are.

I have uploaded this version to Github as well:
https://github.com/conwid/IndexRebuildScript

