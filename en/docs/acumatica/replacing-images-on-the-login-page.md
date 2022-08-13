---
title: "Replacing Images on the Login Page"
slug: "replacing-images-on-the-login-page"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

In this topic you will learn how to replacing standard Acumatica images on the login page. The demonstrated approach will make sure to keep your custom images on login page after the upgrade to a newer version and restore original images, provided by Acumatica, if at some point your customization appears unpublished.

## Using customization to replace images on the login page
To create a customization package replacing images on the login page, follow the steps below on your **local Acumatica instance**:

 1. Create a new folder in the Acumatica instance folder. For this example I added a folder called ***IconsCustomized*** in my local **LoginImages** instance:

    [![enter image description here][1]][1]

 2. Add your custom images in this folder. For the sake of this example, I used images from Acumatica 4.2 login page:

    [![enter image description here][2]][2]

    Keep in mind, ***to replace all images on the login page***, you have to add at least as many custom images in your **IconsCustomized** folder as the number of the `login_bg*.*` files originally present in the **Icons** folder of your Acumatica website. It's perfectly fine to use same image or images multiple times (by naming the files differently), if the number of your custom images is less then what was originally provided by Acumatica.

 3. Now login to your Acumatica application, create new customization project called ***LoginPageImages*** and open it in Customization Manager.

 4. In Customization Manager, navigate to the **Files** section and click the **Add New Record** button to open the **Add Files** dialog:

    [![enter image description here][3]][3]

 5. In the Add Files dialog, select all files from your **IconsCustomized** folder and click **Save**:

    [![enter image description here][4]][4]

    ***Now you have the custom login page images in the customization project, but you still need to edit the path so they correctly replace the standard images.***

 6. In Customization Manager, select **Edit Project XML** from the **File** menu:

    [![enter image description here][5]][5]

 7. For all the **File** tags, generated for your custom images, charge the **AppRelativePath** attribute to ***AppRelativePath="Icons\..."*** and set the **SystemFile** attribute to ***True*** for those images, that currently present in the **Icons** folder, then click the **Save to Database** button when done:

    [![enter image description here][6]][6]

    While publishing customization, Acumatica will automatically backup files currently present in the website folder, which are replaced by files from the customization with **SystemFile** attribute set ***True***.

 8. If you now proceed with publishing the customization, it's very likely for **Some files have been modified in the file system.** error message to show up. To prevent this quite frightening message from appearing, open you project in Customization Manager, navigate to the **Files** section and click **Detect Modified Files** to open the **Modified Files Detected** dialog, then click the **Discard All Changes** button:

    [![enter image description here][7]][7]

 9. Now you can go ahead and publish the customization to enjoy your custom images on the login page:

    [![enter image description here][8]][8]


  [1]: https://i.stack.imgur.com/VZoI2m.png
  [2]: https://i.stack.imgur.com/RWy0Am.png
  [3]: https://i.stack.imgur.com/1AUEXm.png
  [4]: https://i.stack.imgur.com/CqJrwm.png
  [5]: https://i.stack.imgur.com/RYu7Om.png
  [6]: https://i.stack.imgur.com/tDvSc.png
  [7]: https://i.stack.imgur.com/FszDEm.png
  [8]: https://i.stack.imgur.com/mrj0N.jpg

