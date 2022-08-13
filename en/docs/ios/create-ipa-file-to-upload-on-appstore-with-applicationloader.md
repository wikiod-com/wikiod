---
title: "Create .ipa File to upload on appstore with Applicationloader"
slug: "create-ipa-file-to-upload-on-appstore-with-applicationloader"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## create .ipa file to upload app to appstore with Application Loader
   If you want to upload .ipa file to itunesconnect **without integrating developer account in Xcode** and you want to use **application loader**. then you can **generate .ipa with iTunes** . 

**Step 1 :-** Select device inplace of simulator.

[![enter image description here][1]][1]

**Step 2 :-** Go to Product -> select Archive

[![enter image description here][2]][2]

**Step 3 :-** After complited process right click to your Archive -> and select show in Finder

[![enter image description here][3]][3] 

**Step 4 :-** when you click on show in finder you will redirect to Archive folder, looks like this 

[![enter image description here][4]][4]

**Step 5 :-** Right click on .xarchive file -> select Show in finder option.

[![enter image description here][5]][5]

**Step 6 :-** Go to Product Folder -> Application Folder -> You will find yourprojectname.app

[![enter image description here][6]][6]

**Step 7 :-** Now to convert .app to .ipa just drag and drop into itunes . check below image ,

[![enter image description here][7]][7]

**Step 8 :-** Now put this .ipa file in safe place and use when upload with application loader .

**Note :-** if you want to know how to upload app with application loader then check this ,

[Upload app with application Loader][8]


**EDIT :-** 

**WARNING :-**  Don't make .ipa with  changing extension from .aap to .zip and .zip to .ipa.

I have seen in many answer that , they have suggest compress .app file and then change the extension from .zip to .ipa . It is not working now . By this method you will get Error like , 

> IPA is invalid, it does not include a payload directory.


  [1]: http://i.stack.imgur.com/Xq6iC.png
  [2]: http://i.stack.imgur.com/9h28M.png
  [3]: http://i.stack.imgur.com/9jofK.png
  [4]: http://i.stack.imgur.com/RlIU8.png
  [5]: http://i.stack.imgur.com/hhtc1.png
  [6]: http://i.stack.imgur.com/KHoiu.png
  [7]: http://i.stack.imgur.com/udTTp.gif
  [8]: http://help.apple.com/itc/apploader/

