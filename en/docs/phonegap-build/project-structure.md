---
title: "Project Structure"
slug: "project-structure"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

This section provides details of how to organize the files and folders for a phonegap-build project, along with a brief description of each file and folder. It must focus on the files & folders that are uploaded to the phonegap-build service.

It should be updated whenever there is any change in the phonegap-build service to reflect the current requirements. However, if the current phonegap-build service supports some variations, they should be all listed.


## Folders & Files Tree
The minimum required files and folders for the phonegap-build project are:

<pre>
&boxh; www
   &boxvr;&boxh; res
   &boxv;   &boxvr;&boxh; icon
   &boxv;   &boxv;   &boxvr;&boxh; android
   &boxv;   &boxv;   &boxvr;&boxh; ios
   &boxv;   &boxv;   &boxvr;&boxh; windows-phone
   &boxv;   &boxv;   &boxur;&boxh; ...
   &boxv;   &boxvr;&boxh; splash
   &boxv;   &boxv;   &boxvr;&boxh; android
   &boxv;   &boxv;   &boxvr;&boxh; ios
   &boxv;   &boxv;   &boxvr;&boxh; windows-phone
   &boxv;   &boxv;   &boxur;&boxh; ...
   &boxv;   &boxur;&boxh; .pgbomit
   &boxvr;&boxh; index.html
   &boxvr;&boxh; config.xml
   &boxvr;&boxh; icon.png
   &boxur;&boxh; splash.png
</pre>

Place the `index.html` file in the root `www` folder; it will be the first screen in your app. The other HTML pages don't have to be in the root. Similarly, it's entirely up to you where to put your images, CSS, and script files because you will be referencing them in `index.html` and other HTML files as you usually do in any website project.

Also place the `config.xml` file in the root `www` folder. This file holds all the settings for your app.

Additionally, place the default icon `icon.png` and splash-screen `splash.png` images in the root `www` folder. Then in the `res` folder, you can place all your icons and splash-screens for the different platforms and screen resolutions. It is recommended (but not mandatory) to organize them into folders like the illustration above, a folder for the icons and another for the splash-screens, and in each of them a folder for each platform you want to support, and finally in each folder an image for each screen resolution. The names of these files can be anything you want because you will be referencing them in the `config.xml` file.

It is recommended to place an empty file named `.pgbomit` in the `res` folder. This file tells the phonegap-build service to only include the files related to the platform being built. If you don't have a `.pgbomit`, the phonegap-build service will include all the images in every platform, which increases the file size of your apps unnecessarily.

All other files & folders (e.g. the plugins that you use in your app) must be placed outside the `www` folder, because they must **not** be uploaded to the phonegap-build service. The phonegap-build service will add all the required plugins listed in your `config.xml` file.

## Uploading Project to the Build Service
When you're ready to upload your project to the phonegap-build service, zip the content of the `www` folder and upload the zip file to the phonegap-build service.

It is recommended to only zip the content of the `www` folder, and not the `www` folder itself. However, this is just a recommendation and zipping the `www` folder will work too.

The name of the zip file doesn't matter. However, it is a good practice to use the name of your app followed by the version. Example: `MyFirstApp.01.05.21.zip`.

