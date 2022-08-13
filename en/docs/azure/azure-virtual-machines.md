---
title: "Azure Virtual Machines"
slug: "azure-virtual-machines"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Create Azure VM by classic ASM API
    # 1. Login Azure by admin account
    Add-AzureAccount
    #
    # 2. Select subscription name
    $subscriptionName = Get-AzureSubscription | Select -ExpandProperty SubscriptionName
    #
    # 3. Create storage account
    $storageAccountName = $VMName 
    # here we use VMName to play the storage account name and create it, you can choose your name or use existed one to replace the storage account creation operation
    New-AzureStorageAccount -StorageAccountName $storageAccountName -Location $Location | Out-Null
    #
    # 4. Select subscription name and storage account name for current context
    Select-AzureSubscription -SubscriptionName $subscriptionName -Current | Out-Null
    Set-AzureSubscription -SubscriptionName $subscriptionName -CurrentStorageAccountName $storageAccountName | Out-Null
    #
    # 5. Select a VM image name
    $label = $VMLabelPattern
    # take care, please ensure the VM image location resides to the same location of your storage account and service below
    $imageName = Get-AzureVMImage | where { $_.Label -like $label } | sort PublishedDate -Descending | select -ExpandProperty ImageName -First 1
    #
    # 6. Create cloud service
    $svcName = $VMName
    # here we use VMName to play the service name and create it, you can choose your name or use existed one to replace the service creation operation
    New-AzureService -ServiceName $svcName -Location $Location | Out-Null
    #
    # 7. Build command set
    $vmConfig = New-AzureVMConfig -Name $VMName -InstanceSize $VMSize -ImageName $imageName
    #
    # 8. Set local admin of this vm
    $cred=Get-Credential -Message "Type the name and password of the local administrator account."
    $vmConfig | Add-AzureProvisioningConfig -Windows -AdminUsername $cred.Username -Password $cred.GetNetworkCredential().Password
    #
    # 9. Execute the final cmdlet to create the VM
    New-AzureVM -ServiceName $svcName -VMs $vmConfig | Out-Null

For more information, please see [How to create Azure Virtual Machine (VM) by Powershell using classic ASM API][1]


  [1]: https://gallery.technet.microsoft.com/How-to-create-Azure-VM-by-b894d750

