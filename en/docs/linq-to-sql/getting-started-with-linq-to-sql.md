---
title: "Getting started with linq-to-sql"
slug: "getting-started-with-linq-to-sql"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Installing linq-to-sql:

Right-click the **App_Code** folder and then click **Add New Item**. The Add New Item dialog box is displayed. Under Visual Studio installed templates, select the LINQ to SQL Classes template and rename the file **Tasks.dbml**. Click Add.

OR:

Under **VS2102**, in your solution explorer, right click on your project, and select **"Add/New Element"**.
Then you find : C# -> Data -> **"Linq to Sql classes"** (depending on your Visual studio version) or you can select "Entity Data Model". An assistant should appear to guide you through the rest of the process.

In your project classes you should then use your **dataModel**, and use Linq to make your requests.

