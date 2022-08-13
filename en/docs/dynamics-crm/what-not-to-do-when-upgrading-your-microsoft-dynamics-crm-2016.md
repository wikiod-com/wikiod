---
title: "What Not to do when upgrading your Microsoft Dynamics CRM 2016"
slug: "what-not-to-do-when-upgrading-your-microsoft-dynamics-crm-2016"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Microsoft Dynamics CRM has evolved drastically over the past few years. There have been many updates and versions released, with a range of new features and improvements at each stage along the path. During the upgrade of a Dynamics CRM environment, there are a few points to bear in mind to ensure a hassle- free upgrade process. 

## Let’s look at some mandatory items to check off before diving into a full-scale upgrade process

***DB backup***

Database backup is a must before starting any Dynamics CRM upgrade process. This is mandatory as to always have the option to rollback in case of any major roadblock.

***Wrong Estimation***

DO NOT underestimate the work involved in a CRM Upgrade process.

Audit your current Microsoft Dynamics CRM and identify third-party solutions in use. For these third party solutions, check their developer website for compatibility with your intended upgrade CRM version. Download the new solution and keep it ready to test after the migration process has been completed.

***CRM Organization***

DO NOT upgrade the CRM blindly.

As a first step, we need to prepare the CRM organisation to upgrade Microsoft Dynamics CRM.

*Few tips to prepare an organisation*

Make sure that your CRM environment satisfies the software and hardware component requirements

While upgrading CRM 4, delete records from listed table. (The system will try to delete all the records from the below tables when we migrate to CRM 2011, in case it fails then we have to manually delete the entries from these tables.)
Records in the below-listed tables will result in poor performance of the system.

AsyncOperationBase 

WorkflowWaitSubscriptionBase
 
BulkDeleteFailureBase    

WorkflowLogBase 

DuplicateRecordBase 

WorkflowWaitSubscriptionBase

Install the latest rollup before upgrading CRM. For example, in order to upgrade to CRM 2013, the CRM 2011 Server must either be in Update Rollup 6, Update Rollup 14 or a later rollup before an upgrade can be considered. Else, while upgrading the Dynamics CRM environment, an error will be thrown.

For upgrading to MS CRM 2013, use custom code validation tool to check for unsupported client side codes (JavaScript) that will not work following the upgrade. Also, use the legacy feature check tool to detect any server extensions that use the 2007 endpoint or Microsoft Dynamics CRM 4.0 features.

Each new version of Microsoft Dynamics CRM introduces more powerful functionalities, but along with the allure of the latest and greatest version of software, comes the concern of how the upgrade process will impact your business.



## Will the benefits of upgrading outweigh the headaches? Is an upgrade worth it?
Now, here are some common pitfalls which might come along your way when upgrading your Dynamics CRM system.

*The organisation database selected for the import is a different version than the organisation database that is currently deployedCRM Organization Database*

[![enter image description here][1]][1]

To fix this issue, we need to install either UR6 or above in MS CRM 2011 environment while upgrading to MS CRM 2013. Following this, the upgrade to Dynamics CRM 2016 can be completed without concerns

*Organisation cannot be upgraded.*

[![enter image description here][2]][2]

To fix this issue, we need to install SP1 for MS CRM 2013. Following this, the upgrade to Dynamics CRM 2016 can be completed without concerns.

*Uninstallation of Microsoft Dynamics CRM connector for SQL Server reporting services*

[![enter image description here][3]][3]

To fix this issue, uninstall Microsoft Dynamics CRM 2013 reporting extensions from Installed programs in Control panel before proceeding with the upgrade to Dynamics CRM 2016.

*The SQL Server Reporting Service account is a local user and is not supported when upgrading from CRM 2013 to CRM 2015*

[![enter image description here][4]][4]

*If the Microsoft SQL Server 2012 Reporting Service was installed via default settings – then the service account is set to “ReportServer” – to resolve the issue, open the Reporting Services Configuration Manager and update the Service Account to anything else such as “Local System”. Following this, we won’t face any concerns while we upgrading to MS CRM 2016.*

[![enter image description here][5]][5]

Some of our learning have been simple and logical, but many might forgo them.

While upgrading, don’t go with as it is upgraded, there are many features that can cater to the organisation’s current requirements which are implemented through customization. These can be modelled out of the box to fit the requirements with later versions.

Also, consider the communication/integration components with which the legacy or any existing system is connected with Microsoft Dynamics CRM. These are important links which should not be affected during an upgrade process, and when affected, can greatly impact productivity


  [1]: https://i.stack.imgur.com/fhnOd.jpg
  [2]: https://i.stack.imgur.com/QMx6k.jpg
  [3]: https://i.stack.imgur.com/e1c6T.png
  [4]: https://i.stack.imgur.com/NoVRT.jpg
  [5]: https://i.stack.imgur.com/aclK9.jpg

