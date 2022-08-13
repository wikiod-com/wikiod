---
title: "How to customize platform specific www folder in cordova"
slug: "how-to-customize-platform-specific-www-folder-in-cordova"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## create css/js specific to a platform (android/ios)
Let say you want to create css/js file specific to a platform. For that you have to create a **merges** folder in root folder of you cordova porject. In merges folder create directory for each platform (android/ios..). then in specific platform folder create a css/js folder and put your css/js file specific to platform folder. That's it, once you run **cordova build**  command, all js/css files corresponding to each platform would be placed in respected platform folder 

**Note:** Make sure your root www/index.html will have same css/js defined. For that make sure you have same filename corresponding to each platform in merges folder.  

```sh
//let say you are in CordovaMergesExample folder
cd CordovaMergesExample 

//create test folder with com.test id and TestApp as name
cordova create test com.test TestApp

//add platform android and ios 
cordova platform add android ios


----------


//create merges/android/css/override.css and merges/ios/css/override.css 


----------

//In root www/index.html add this stylesheet
<link rel="stylesheet" type="text/css" href="css/override.css" />


----------


cordova build

---> cordova build engine automatically identify the platform in merges folder and add files in respective folders. Check platforms/android/assets/www/css and platforms/ios/www/css
```

