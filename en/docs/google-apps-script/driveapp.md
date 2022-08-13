---
title: "DriveApp"
slug: "driveapp"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Create a new folder in a Google Drive root
<!-- language: lang-js -->

    function createNewFolderInGoogleDrive(folderName) {
      return DriveApp.createFolder(folderName);
    }

Use function `createNewFolderInGoogleDrive` to create folder named `Test folder` in a Google Drive root:
<!-- language: lang-js -->

     var newFolder = createNewFolderInGoogleDrive('Test folder');

`newFolder` has [Class Folder][1] type:
<!-- language: lang-js -->

    // output id of new folder to log
    Logger.log(newFolder.getId());



  [1]: https://developers.google.com/apps-script/reference/drive/folder

## Create new file in Google Drive of a certain Mime type
<!-- language: lang-js -->

     function createGoogleDriveFileOfMimeType() {
      var content,fileName,newFile;//Declare variable names
      
      fileName = "Test File " + new Date().toString().slice(0,15);//Create a new file name with date on end
      content = "This is the file Content";
    
      newFile = DriveApp.createFile(fileName,content,MimeType.JAVASCRIPT);//Create a new file in the root folder
    };



## Create a new text file in Google Drive root folder
<!-- language: lang-js -->

     function createGoogleDriveTextFile() {
      var content,fileName,newFile;//Declare variable names
      
      fileName = "Test Doc " + new Date().toString().slice(0,15);//Create a new file name with date on end
      content = "This is the file Content";
    
      newFile = DriveApp.createFile(fileName,content);//Create a new text file in the root folder
    };



## Create a new file in Google drive from a blob
<!-- language: lang-js -->

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
<!-- language: lang-js -->

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
<!-- language: lang-js -->

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



## Add a folder to the root drive
<!-- language: lang-js -->

    function DriveAppAddFolder(child) {//Adds file to the root drive in Google Drive
      var body,returnedFolder;//Declare variable names
    
      if (!child) {
        body = "There is no folder";
        MailApp.sendEmail(Session.getEffectiveUser().getEmail(), "", "Error Adding Folder!", body)
        return;
      };
        
      returnedFolder = DriveApp.addFolder(child);//Add a folder to the root drive
      
      Logger.log('returnedFolder: ' + returnedFolder);//Print the folder results to the Logs
    };
    
    
    function createNewFolderInGoogleDrive() {
      var folder,newFolderName,timeStamp,dateTimeAsString;
      
      timeStamp = new Date();//Create a new date
      dateTimeAsString = timeStamp.toString().slice(0,15);
      
      newFolderName = 'Test Folder Name ' + dateTimeAsString;//Create new folder name with date/time appended to name
      
      folder = DriveApp.createFolder(newFolderName);//Create a new folder
      DriveAppAddFolder(folder);//Call a function and pass a folder to the function
    };



## Create a new text file and add it to the root folder
<!-- language: lang-js -->

     function DriveAppAddFile(child) {//Adds file to the root drive in Google Drive
      var body,returnedFolder;//Declare variable names
    
      if (!child) {
        body = "There is no file";
        MailApp.sendEmail(Session.getEffectiveUser().getEmail(), "", "Error Adding File!", body)
        return;
      };
        
      returnedFolder = DriveApp.addFile(child);
      
      Logger.log('returnedFolder: ' + returnedFolder);
    };
    
    
    function createNewFileInGoogleDrive() {
      var content,file,newFileName,timeStamp,dateTimeAsString;
      
      timeStamp = new Date();//Create a new date
      dateTimeAsString = timeStamp.toString().slice(0,15);
      
      content = "This is test file content, created at: " + dateTimeAsString;//Create content for new file
      newFileName = 'Test File ' + dateTimeAsString;//Create new file name with date/time appended to name
      
      file = DriveApp.createFile(newFileName, content);//Create a new file
      DriveAppAddFile(file);//Call a function and pass a file to the function
    };

## Get all Files in a Drive Folder
<!-- language: lang-js -->

    function onOpen() {

      // Add a custom menu to run the script
      var ss = SpreadsheetApp.getActiveSpreadsheet();
      var searchMenuEntries = [ {name: "Run", functionName: "search"}];
      ss.addMenu("Get Files", searchMenuEntries);
    }
    
    function getFiles() {

      // Get the active spreadsheet and the active sheet
      var ss = SpreadsheetApp.getActiveSpreadsheet();
      var ssid = ss.getId();
      
      // Look in the same folder the sheet exists in. For example, if this template is in
      // My Drive, it will return all of the files in My Drive.
      var ssparents = DriveApp.getFileById(ssid).getParents();
      var sheet = ss.getActiveSheet();
    
      // Set up the spreadsheet to display the results
      var headers = [["Last Updated", "File Owner", "File Name", "File URL"]];
      sheet.getRange("A1:D").clear();
      sheet.getRange("A1:D1").setValues(headers);
      
      // Loop through all the files and add the values to the spreadsheet.
      var folder = ssparents.next();
      var files = folder.getFiles();
      var i=1;
      while(files.hasNext()) {
        var file = files.next();
        if(ss.getId() == file.getId()){ 
          continue; 
        }
        sheet.getRange(i+1, 1, 1, 4).setValues([[file.getLastUpdated(),file.getOwner().getName(),file.getName(), file.getUrl()]]);
        i++;
      }
    }

