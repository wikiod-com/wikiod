---
title: "Google sheets MailApp"
slug: "google-sheets-mailapp"
draft: false
images: []
weight: 9961
type: docs
toc: true
---

This service allows users to send emails with complete control over the content of the email. Unlike GmailApp, MailApp's sole purpose is sending email. MailApp cannot access a user's Gmail inbox.

Changes to scripts written using GmailApp are more likely to trigger a re-authorization request from a user than MailApp scripts.

## A basic MailApp Example
MailApp is the api from Google App Script that can be used to send mail

<!-- language: lang-js -->
    
    function sendEmails() {

      var subject = "A subject for your new app!";
      var message = "And this is the very first message"
      var recipientEmail = "abc@example.com";

      MailApp.sendEmail(recipientEmail, subject, message);
    }  

The MailApp Class is limited to [quotas][1] based on your Google Account:

- Consumer user (ie, personal Gmail account): 100 recipients/day
- Google Apps (legacy) customer: 100 recipients/day
- GSuite (basic/Gov/Edu/Business): 1500 recipients/day

You can check your email quota within `MailApp`

<!-- language: lang-js -->

    function checkQuota() {
      Logger.log(MailApp.getRemainingDailyQuota());
    }


  [1]: https://developers.google.com/apps-script/guides/services/quotas

## Sending HTML content in mail
In the above example, if we want to send out HTML content as message in the email, then create a HTML file by going to **File -> New -> HTML file**

Now you can see a HTML file besides your gs file as follows :

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/rPy6s.png 

Now, update the *getMessage()* method from above example as follows :

<!-- language: lang-js -->

    function getMessage(name, amount) {
      var htmlOutput = HtmlService.createHtmlOutputFromFile('Message'); // Message is the name of the HTML file
      
      var message = htmlOutput.getContent()
      message = message.replace("%name", name);
      message = message.replace("%amount", amount);
      
      return message;
    }

The call to *MailApp* api needs to be changed as well
<!-- language: lang-js -->
 
    MailApp.sendEmail(recipientEmail, subject, message, {htmlBody : message});

So the whole code will be as follows : 

<!-- language: lang-js -->

    function getDataSheet() {
    
      sheet = SpreadsheetApp.getActiveSheet();
    
      startRow = 2;  // First row of data to process
      numRows = 100;   // Number of rows to process
      startCol = 1;  //First column of data to process
      numCols = 15;    // Number of columns to process 
      
      var dataRange = sheet.getRange(startRow, startCol, numRows, numCols);
    
      // Fetch values for each row in the Range.
      var data = dataRange.getValues();  
    
      return data;  
    }
    
    function getMessage(name, amount) {
      var htmlOutput = HtmlService.createHtmlOutputFromFile('Message');
      
      var message = htmlOutput.getContent()
      message = message.replace("%name", name);
      message = message.replace("%amount", amount);
      
      return message;
    }
    
    function sendEmail() {
      
      var emailSent = "Yes";
      var reimbursed = "Yes";
      var emailCol = 5;
          
      var data = getDataSheet();
      
      for (var i = 0; i < data.length; i++) {
        
        var row = data[i];
        
        var isReimbursed = row[3];
        var isEmailSent = row[4];
        var name = row[0];
        var amount = row[2];
        
        if(isReimbursed == reimbursed && isEmailSent != emailSent) {
          
          var subject = "Reimbursement details";
          var message = getMessage(name, amount);
          
          var recipientEmail = row[1];
          
          MailApp.sendEmail(recipientEmail, subject, message, {htmlBody : message});
          
          sheet.getRange(startRow + i, emailCol).setValue(emailSent);
        }
      }
    }

## Access data from sheet
<!-- language: lang-js -->

    function getSheetData() {
    
      var sheet = SpreadsheetApp.getActiveSheet();

      var startRow = 2;  // First row of data to process
      var numRows = 100;   // Number of rows to process
      var startCol = 1;  //First column of data to process
      var numCols = 15;    // Number of columns to process 
      
      var dataRange = sheet.getRange(startRow, startCol, numRows, numCols);
    
      // Fetch values for each row in the Range.
      var data = dataRange.getValues();  
    
      return data;  
    }

You can also modify the above function as follows to get data range dynamic from the content present in sheet:

<!-- language: lang-js -->

    function getDataSheet() {

      sheet = SpreadsheetApp.getActiveSheet();
       
       //Get data range based on content
      var dataRange = sheet.getDataRange();
  
      // Fetch values for each row in the Range.
      var data = dataRange.getValues();  

      return data;  
    }

## Use Sheet data to send email
Given - A have sheet of employees who have requested for reimbursement. 

Requirement - We should sent out an email to the employee when their reimbursement is processed

So, the sheet is as follows:

[![Name | Email Address | Reimbursement amount | Is Reimbursement Processed | Email sent][1]][1]

The function for sending out an email is as follows:

<!-- language: lang-js -->

    function getDataSheet() {
    
      sheet = SpreadsheetApp.getActiveSheet();
    
      startRow = 2;  // First row of data to process
      numRows = 100;   // Number of rows to process
      startCol = 1;  //First column of data to process
      numCols = 15;    // Number of columns to process 
      
      var dataRange = sheet.getRange(startRow, startCol, numRows, numCols);
    
      // Fetch values for each row in the Range.
      var data = dataRange.getValues();  
    
      return data;  
    }    

    function getMessage(name, amount) {
      return "Hello " + name + ", Your reimbursement for amount " + amount + " is processed successfully";
    }
    
    function sendEmail() {
      
      var emailSent = "Yes";
      var reimbursed = "Yes";
      var emailCol = 5;
          
      var data = getDataSheet();
      
      for (var i = 0; i < data.length; i++) {
        
        var row = data[i];
        
        var isReimbursed = row[3];
        var isEmailSent = row[4];
        var name = row[0];
        var amount = row[2];
        
        if(isReimbursed == reimbursed && isEmailSent != emailSent) {
          
          var subject = "Reimbursement details";
          var message = getMessage(name, amount);
          
          var recipientEmail = row[1];
          
          MailApp.sendEmail(recipientEmail, subject, message);
          
          //Sheet range starts from index 1 and data range starts from index 0
          sheet.getRange(1 + i, emailCol).setValue(emailSent);
        }
      }
    }

[![Email sent for Ramesh is Yes][2]][2]


  [1]: http://i.stack.imgur.com/QhYZq.png
  [2]: http://i.stack.imgur.com/l4GzJ.png

