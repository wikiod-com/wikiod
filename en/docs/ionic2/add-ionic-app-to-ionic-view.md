---
title: "Add ionic app to ionic view"
slug: "add-ionic-app-to-ionic-view"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

ionic view is a mobile app which you have to install in your mobile, so that you can view your app without creating .apk files. By sharing your app id, others can also view your app in their mobile using ionic view.

Site: https://view.ionic.io/

## Steps to add your app to ionic view

The following steps need to be done in the [app.ionic.io][1]

1. Create an account or login into your ionic account 


2. Click "New App" in the Dashboard and give name for your app

        I named my app as 'MyIonicApp'


3. In the overview section of this newly created app, there will be a ID below the app name.

        MyIonicApp ID is 4c5051c1 


Below steps are done in **Node.js** command prompt
1. Login into your ionic account by running

       $ ionic login


2. Root your app folder.


3. To upload your app to ionic view, first you have to link your app with the ID you created in ionic site. Run the following command to link,
    
       $ ionic link [your-app-id]

    For MyIoincApp, the command will be, 
   
       $ ionic link 4c5051c1

   The above command will update the app id in the MyIonicApp's config file.


4. Once the linking is done, upload the app by executing 

       $ ionic upload


**Note**

Once the upload is successful, open ionic view in your mobile to view the app.

Others can view your app, by submitting the app id under 'Preview an app' section in ionic view. 


  [1]: https://apps.ionic.io/login

