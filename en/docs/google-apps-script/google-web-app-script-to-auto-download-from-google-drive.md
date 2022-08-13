---
title: "Google Web App Script To Auto Download From Google Drive"
slug: "google-web-app-script-to-auto-download-from-google-drive"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

This Simple Google App Web Script (Standalone) allows Google Drive to be repeated polled for files to be downloaded to the user's local PC.

Shows how to use one app script to provide the function of both the:

1. User interface (a simple one in this example)
2. The file download page.

For a fuller explanation of how it works read the Example "How it works".


The Web Script must be published in order to work.

Pops ups must be enabled for https://script.google.com

## forms.html
    <!DOCTYPE html>
    <html>
      <head>
        <base target="_top">
        <script>
        
        setInterval(
        function () 
        { 
          document.getElementById('messages').innerHTML = 'Event Timer Fetching';
          google.script.run
            .withSuccessHandler(onSuccess)
            .withFailureHandler(onFailure)
            .fetchFromGoogleDrive();
        }, 60000);
        
        function callFetchGoogleDrive() {
          document.getElementById('messages').innerHTML = 'Fetching';
          google.script.run
            .withSuccessHandler(onSuccess)
            .withFailureHandler(onFailure)
            .fetchFromGoogleDrive();   
        }
          
        function onSuccess(sHref) 
        {
          if(new String(sHref).valueOf() == new String("").valueOf())
          {
            document.getElementById('messages').innerHTML = 'Nothing to download';
          }
          else
          {
            document.getElementById('messages').innerHTML = 'Success';
            document.getElementById('HiddenClick').href = sHref;
            document.getElementById('HiddenClick').click(); // Must enable Pop Ups for https://script.google.com
          }
        }
        
        function onFailure(error) 
        {
          document.getElementById('messages').innerHTML = error.message;
        }
        
        </script>
      </head>
      <body>
        <div id="messages">Waiting to DownLoad!</div>
        <div>
          <a id="HiddenClick" href="" target="_blank" onclick="google.script.host.close" style="visibility: hidden;">Hidden Click</a>
        </div>
        <div>
          <button type="button" onclick='callFetchGoogleDrive();' id="Fetch">Fetch Now!</button> 
        </div>
      </body>
    </html>

## code.gs
    function doGet(e){
       var serveFile = e.parameter.servefile;
       var id = e.parameter.id;
       
       if(serveFile)
       {
         return downloadFile(id); // and Hyde
       }
       
      return HtmlService.createHtmlOutputFromFile('form.html'); // Jekyll
    }
    
    function fetchFromGoogleDrive() { // Jekyll
      var fileslist = DriveApp.searchFiles("your search criteria goes here + and trashed = false"); // the 'and trashed = false' prevents the same file being download more than once
      
      if (fileslist.hasNext()) {
        var afile = fileslist.next();
        var html = ScriptApp.getService().getUrl()+"?servefile=true&id="+afile.getId();
        return html;
      }
      else
      {
        return '';
      }
    }
       
    function downloadFile(id){ // and Hyde  
      try
      {
        var afile = DriveApp.getFileById(id);
      
        var aname = afile.getName();
        var acontent = afile.getAs('text/plain').getDataAsString();
        
        var output = ContentService.createTextOutput();
        output.setMimeType(ContentService.MimeType.CSV);
        output.setContent(acontent);
        output.downloadAsFile(aname);
        afile.setTrashed(true);
        return output;
      }
      catch (e) {
        return ContentService.createTextOutput('Nothing To Download')
      }
    }

## How it works
Google Drive (Standalone) Web App to automatically download (Poll) files from Drive to user's local PC (Download Folder).

DriveApp provides mechanisms for searching and downloading files.  However the download mechanism has some serious limitations due to the client/server architecture Google Apps inherited. (No fault of Google)

The server side DriveApp does not provide a direct function to download to the local PC because the server has no concept of where the client is and downloading the file to the server itself would be meaningless.

The server side code needs a mechanism to provide the client side code the file data or a link to file.  Both these mechanisms are provided but the data from the former is limited to being used by the client side code directly.  The client has no mechanism of saving the data, once obtained, to the local PC.  So it can be used to display the data on the web page itself.

The second mechanism allows the url of the script (itself) or the url of the Drive file to be passed back.  The Drive file url is not very useful as it cannot be directly used in the client browser to download the file. Placing this url in anchor (and clicking it) only results in a web page that opens but does not actually do anything (except possibly view the file online).

That leaves the script url.  However the script url only provides the script and not the file.

In order to initiate a download the file from the Drive service must be returned from the doGet / doPost function of the server side script using ContentService createTextOutput exactly as shown in the Google online guides.  However this implies that there can be no other UI element on the web page generated by the results returned by doGet/doPost.

This leaves us with a very unattractive solution.  A blank web page with no user UI elements that downloads a page, closes and that requires manually opening when ever another download is required.

Obviously another hosting web page could provide the UI and the link to the Web App Download script to resolve this issue.

This script uses a Dr Jekyll and Mr Hyde approach to resolve this issue.

If the script is opened with no parameters to the GET (doGet) then it defaults to displaying a form.  This will be the condition when the published app is first opened by a user. The form provided in this example is extremely simple.

If the script is opened with the parameter servefile=true then the script behaves as a Drive file download.

The client side javascript contains a polling mechanism (event timer setInterval) that periodically calls server side script to check for the availability of another file to download.

When the server side script is executed if it finds any Drive file that matches the search criteria* it returns the url of the script itself appended with the parameters:

?servefile=true&id=the_id_of_the_google_drive_file

(* The search criteria in this simple example is hard coded into the server side script.  It could easily be passed from the client to the server if required.)

This information is returned as a string to the client via the recognised withSuccessHandler mechanism.

The client java script then updates the HREF of a hidden anchor with this returned information and then clicks the anchor automatically.

This causes another invocation of the app/script to be launched.  When the new invocation of the app launches the doGet will detect the servefile parameter and instead of returning the user interface it will return the file, to the browser. The file returned will be that identified by the provided ID parameter that was previously returned by the search described above.

Given that the file with the provided ID still exists it will be downloaded and the new invocation of the app will close leaving the first invocation to repeat this process.

A button is provided on the simple interface if the user/tester becomes impatient with waiting for the timer but it is not required and can otherwise be removed.

The simple form can of course be extended to provide a richer user interface if required.  Such as providing the file search criteria.

