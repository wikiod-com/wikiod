---
title: "GmailApp"
slug: "gmailapp"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

See also the official [API reference][1] for the GmailApp for further details on the available methods.

  [1]: https://developers.google.com/apps-script/reference/gmail/gmail-app

## Get CSV file attached to a mail
Assume that we have a system that sends out daily reports over email in the form of attached CSV files and that we want to access these.

<!-- language: lang-js -->

    function getCsvFromGmail() {
      // Get the newest Gmail thread based on sender and subject
      var gmailThread = GmailApp.search("from:noreply@example.com subject:\"My daily report\"", 0, 1)[0];
      
      // Get the attachments of the latest mail in the thread.
      var attachments = gmailThread.getMessages()[gmailThread.getMessageCount() - 1].getAttachments();
      
      // Get and and parse the CSV from the first attachment
      var csv = Utilities.parseCsv(attachments[0].getDataAsString());
      return csv;
    }

