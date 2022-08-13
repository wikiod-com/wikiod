---
title: "DriveApp Service"
slug: "driveapp-service"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

Google Mime types can not be used for the third parameter of Mime Types. Using a Google Mime Type will result in an error that states:

Cannot use "DriveApp.createFile()" to create Google MIME types. Please use Advanced Drive Service

MimeType.GOOGLE_APPS_SCRIPT

MimeType.GOOGLE_DOCS

MimeType.GOOGLE_DRAWINGS

MimeType.GOOGLE_FORMS

MimeType.GOOGLE_SHEETS

MimeType.GOOGLE_SLIDES

## Create a new folder in Google root drive
    function createNewFolderInGoogleDrive() {
      var folderName,newFolder;//Declare variable names
      
      folderName = "Test Folder " + new Date().toString().slice(0,15);//Create a new folder name with date on end
      newFolder = DriveApp.createFolder(folderName);//Create a new folder in the root drive
    };

## Create new file in Google Drive of a certain Mime type
    function createGoogleDriveFileOfMimeType() {
      var content,fileName,newFile;//Declare variable names
      
      fileName = "Test File " + new Date().toString().slice(0,15);//Create a new file name with date on end
      content = "This is the file Content";
    
      newFile = DriveApp.createFile(fileName,content,MimeType.JAVASCRIPT);//Create a new file in the root folder
    };

## Create a new text file in Google root drive folder
    function createGoogleDriveTextFile() {
      var content,fileName,newFile;//Declare variable names
      
      fileName = "Test Doc " + new Date().toString().slice(0,15);//Create a new file name with date on end
      content = "This is the file Content";
    
      newFile = DriveApp.createFile(fileName,content);//Create a new text file in the root folder
    };

## Create a new file in Google Drive from a blob


    function createGoogleDriveFileWithBlob() {
      var blob,character,data,fileName,i,L,max,min,newFile,randomNmbr;//Declare variable names
      
      fileName = "Test Blob " + new Date().toString().slice(0,15);//Create a new file name with date on end
      
      L = 500;//Define how many times to loop
      data = "";
      max = 126;
      min = 55;
    
      for (i=0;i<L;i+=1) {//Loop to create data
        randomNmbr = Math.floor(Math.random()*(max-min+1)+min);//Create a random number
        //Logger.log('randomNmbr: ' + randomNmbr);
        character = String.fromCharCode(randomNmbr);
        
        //Logger.log('character: ' + character);//Print the character to the Logs
        data = data + character;
      };
    
      blob = Utilities.newBlob(data, MimeType.PLAIN_TEXT, fileName);//Create a blob with random characters
    
      newFile = DriveApp.createFile(blob);//Create a new file from a blob
      
      newFile.setName(fileName);//Set the file name of the new file
    };

## Get all folders - put folders into a continuation token - then retrieve from token
    function processGoogleDriveFolders() {
      var arrayAllFolderNames,continuationToken,folders,foldersFromToken,thisFolder;//Declare variable names
      
      arrayAllFolderNames = [];//Create an empty array and assign it to this variable name
      
      folders = DriveApp.getFolders();//Get all folders from Google Drive in this account
      continuationToken = folders.getContinuationToken();//Get the continuation token
    
      Utilities.sleep(18000);//Pause the code for 3 seconds
      
      foldersFromToken = DriveApp.continueFolderIterator(continuationToken);//Get the original folders stored in the token
      folders = null;//Delete the folders that were stored in the original variable, to prove that the continuation token is working
      
      while (foldersFromToken.hasNext()) {//If there is a next folder, then continue looping
        thisFolder = foldersFromToken.next();//Get the next folder
        arrayAllFolderNames.push(thisFolder.getName());//Get the name of the next folder
      };
      
      Logger.log(arrayAllFolderNames);//print the folder names to the Logs
    };

## Get all files - put them into a continuation token - then retrieve them
    function processGoogleDriveFiles() {
      var arrayAllFileNames,continuationToken,files,filesFromToken,fileIterator,thisFile;//Declare variable names
      
      arrayAllFileNames = [];//Create an empty array and assign it to this variable name
      
      files = DriveApp.getFiles();//Get all files from Google Drive in this account
      continuationToken = files.getContinuationToken();//Get the continuation token
      
      Utilities.sleep(18000);//Pause the code for 3 seconds
      
      filesFromToken = DriveApp.continueFileIterator(continuationToken);//Get the original files stored in the token
      files = null;//Delete the files that were stored in the original variable, to prove that the continuation token is working
      
      while (filesFromToken.hasNext()) {//If there is a next file, then continue looping
        thisFile = filesFromToken.next();//Get the next file
        arrayAllFileNames.push(thisFile.getName());//Get the name of the next file
      };
      
      Logger.log(arrayAllFileNames);  
    };

