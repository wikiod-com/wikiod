---
title: "Getting started with azure-webjobs"
slug: "getting-started-with-azure-webjobs"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Creating a WebJob in the Azure Portal
 1. In the **Web App** blade of the [Azure Portal][1], click **All settings** > **WebJobs** to show the WebJobs blade:

[![WebJobs blade][2]][2]

 2. Click **Add**. The **Add WebJob** dialog appears.

[![Add WebJob blade][3]][3]

 3. Under **Name**, provide a name for the WebJob. The name must start with a letter or a number and cannot contain any special characters other than "-" and "_".
 4. In the **How to Run** box, choose your preferred option **Continuous** or **Triggered** (the trigger can be using a cron schedule or a WebHook).
 5. In the **File Upload** box, click the folder icon and browse to the zip file that contains your script. The zip file should contain your executable (.exe .cmd .bat .sh .php .py .js) as well as any supporting files needed to run the program or script.
 6. Check **Create** to upload the script to your web app.
The name you specified for the WebJob appears in the list on the WebJobs blade.


  [1]: https://portal.azure.com/
  [2]: http://i.stack.imgur.com/2r22H.png
  [3]: http://i.stack.imgur.com/50mRZ.png

