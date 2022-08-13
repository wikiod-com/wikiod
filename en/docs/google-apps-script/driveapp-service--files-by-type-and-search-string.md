---
title: "DriveApp Service - Files by type and search string"
slug: "driveapp-service---files-by-type-and-search-string"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Parameters
| Parameter Name | Use For |
| ------ | ------ |
| searchString | the string to be found in the file name |

## Get files by file type with matching string in file name
Get all Google Forms with the word "Untitled" in the file name.

<!-- language: lang-js -->

    function mainSearchFunction(searchStr) {
      var fileInfo,arrayFileIDs,arrayFileNames,arrayOfIndexNumbers,
          allFileIDsWithStringInName,i,searchStr,thisID;//Declare variables
      
      if (!searchStr) {
        searchStr = "Untitled";//Assign a string value to the variable
      };

      fileInfo = getFilesOfType();//Run a function that returns files information
      arrayFileNames = fileInfo[1];//Get the array of file names
      arrayOfIndexNumbers = searchFileNamesForString(arrayFileNames,searchStr);

      //Logger.log('searchStr: ' + searchStr)
      //Logger.log(arrayOfIndexNumbers)
      
      allFileIDsWithStringInName = [];
      arrayFileIDs = fileInfo[0];
    
      for (i=0;i<arrayOfIndexNumbers.length;i+=1) {
        thisID = arrayFileIDs[arrayOfIndexNumbers[i]];
        allFileIDsWithStringInName.push(thisID);
      };
      
      Logger.log(allFileIDsWithStringInName)
    };

    function getFilesOfType() {
      var allFormFiles,arrFileName,arrFileID,arrFileUrls,thisFile;
      
      allFormFiles = DriveApp.getFilesByType(MimeType.GOOGLE_FORMS);
      arrFileName = [];
      arrFileID = [];
      arrFileUrls = [];
      
      while (allFormFiles.hasNext()) {
        thisFile=allFormFiles.next();
        arrFileName.push(thisFile.getName());
        arrFileID.push(thisFile.getId());
        arrFileUrls.push(thisFile.getUrl());
      };
    
      //Logger.log(arrFileName)
      return [arrFileID,arrFileName];
    };


    function searchFileNamesForString(arrayFileNames,searchStr) {
      var arrayIndexNumbers,i,L,thisName;
      
      arrayIndexNumbers = [];

      L = arrayFileNames.length;

      for (i=0;i<L;i+=1){
        thisName = arrayFileNames[i];
        Logger.log(thisName);
        Logger.log('thisName.indexOf(searchStr): ' + thisName.indexOf(searchStr));
        
        if (thisName.indexOf(searchStr) !== -1) {
          arrayIndexNumbers.push(i);
        };
      };

      return arrayIndexNumbers;
    };

