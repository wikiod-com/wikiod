---
title: "DriveApp - getFileById(id)"
slug: "driveapp---getfilebyidid"
draft: false
images: []
weight: 9828
type: docs
toc: true
---

It is also possible to get a file by the file's URL.  The ID of a file is in the url, so using the ID instead of the entire URL means that the parameter is shorter.  Storing the URL rather than the ID takes up more space.

## Get a file from Google Drive using the file ID
    function getGoogleDriveFileById(id) {
      var file;
      
      file = DriveApp.getFileById(id);//Returns a file - The "id" must be a string
      
      //One way to manually get a file ID
      // - Open the file from Google Drive
      // - The file ID is in the URL in the browsers address bar
      //https://docs.google.com/spreadsheets/d/File_ID_is_here/edit#gid=0
    };

