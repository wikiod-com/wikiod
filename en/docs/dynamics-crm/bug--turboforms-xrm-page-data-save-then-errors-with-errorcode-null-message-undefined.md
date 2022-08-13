---
title: "Bug - TurboForms Xrm.Page.data.save().then() Errors With ErrorCode Null, Message Undefined"
slug: "bug---turboforms-xrmpagedatasavethen-errors-with-errorcode-null-message-undefined"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## onChange() Calls save(), From Field That Is Invalid
Steps to Reproduce (Turbo Forms, CRM 2015.1, --> CRM 2016.2)
------------------

 1. Form (with or without other required fields) has one field that is empty and required.
 2. Lose focus on the empty field ("title" in this example), this triggers the Field Notification Icon:
[![enter image description here][1]][1]
 3. Wire up onChange Handler for empty field to call save:

<!-- language: lang-js -->
    function forceSaveOnChangeOfTitle(){
        Xrm.Page.data.save().then(
            function () {}, 
            function (error, message) {console.error("Error: " + error + " Message: " + message);}
        );
    }
 4. Enter a Value in empty field.

**Result:**

 - Save fails.  Calls failure callback with Error Number of "null", and Message of "undefined".
 - Field Notification disappears, but required message is still displayed in the bottom right:
[![enter image description here][2]][2]


**Known Workarounds:**
------------------

Set the value of the attribute to itself:

<!-- language: lang-js -->
    function forceSaveOnChangeOfTitle(){
        var title = Xrm.Page.getAttribute("title");
        title.setValue(title.getValue());
        Xrm.Page.data.save().then(
            function () {}, 
            function (error, message) {console.error("Error: " + error + " Message: " + message);}
        );
    }

Use 1ms Timeout with:

<!-- language: lang-js -->
    function forceSaveOnChangeOfTitle(){
        setTimeout(function() {
            Xrm.Page.data.save().then(
                function () {}, 
                function (error, message) {console.error("Error: " + error + " Message: " + message);}
            );
        }, 1);
    }


  [1]: http://i.stack.imgur.com/KbSOE.jpg
  [2]: http://i.stack.imgur.com/KxblB.jpg

