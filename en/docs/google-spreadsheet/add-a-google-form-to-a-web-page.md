---
title: "Add a Google Form to a web page"
slug: "add-a-google-form-to-a-web-page"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Google Spreadsheets has a powerful add on called [Google Forms](https://www.google.com.au/forms/about/) that allows a web developer to add simple forms easily to web sites in order to collect data from users.

This article discusses the way to embed these into a web application.

I've also created a [Youtube video](https://youtu.be/ODnjdf25SQY) with a running commentary, screenshots and so on.

The examples above are adapted from a fully functional site and this article assumes a reasonable existing knowledge of HTML/Javascript/CSS in order to use these code snippets.

## Build a Google form
Log into a Google Account and click New > More > Google Forms. 

Build the form fields required using the editor.

If the form was built with an account that is part of an organisation then click on the cog and unselect the option that only members can complete the form.

Set the form to save the responses to a spreadsheet by clicking on the Responses tab, and click the spreadsheet icon. The popup provides the option to save this form data to a new or existing spreadsheet. By selecting existing it allows multiple forms per spreadsheet. Follow the prompts to complete this task. This is a good time to save some test data to make sure it is all working.

Optionally the web app may wish to set some pre-filled responses in the fields. If that is the case go back to the form and click on the three dots dropdown menu, then click `Get pre-filled link`. This will load the form in a special mode where fields can be completed without submitting the data. When completing the fields use the label name as the prefilled value. Then save the URL which will have parameters similar to `entry.123=labelname1&entry.456=labelname2`. Save a copy of that URL for later.

## Embed the Google form
This is done by adding a button, dialog box and iframe as explained below.

The examples below use [MDL](https://getmdl.io/started/) for look and feel because it is used by Google forms and so it makes the additional elements look fairly seamless.

Dialog boxes may require a [polyfill](https://cdnjs.com/libraries/dialog-polyfill) if you plan to support older browsers.

## Add Button and Dialog to html
```
<button id="googleFormButton" class="mdl-button mdl-js-button mdl-button--raised">
  Load Form
</button>
```

```
<dialog id="googleFormsDialog" class="mdl-dialog">
  <!-- <h4 class="mdl-dialog__title">Google Form</h4> -->
  <div id="googleformparent" class="mdl-dialog__content">
    <div id="googleFormsDialogIFrameLoading">Loading...</div>
    <!-- IFrame element googleFormsDialogIFrame is added dynamically due to google forms popup issue. -->
  </div>
  <div class="mdl-dialog__actions">
    <button id="dialogclose" type="button" class="mdl-button">Close</button>
  </div>
</dialog>
```

## Add an event listener to the button.
The value of GOOGLE-FORM-PREFILLED-URL should look something like this: 
https://docs.google.com/forms/.../?usp=pp_url&entry.1739003583=labelname1

    jQuery('#googleFormButton').click(showGoogleForm)
    jQuery('#googleFormButton').attr('googleFormsURL', 'GOOGLE-FORM-PREFILLED-URL')


## Manage the dialog box and google form iframe
Add a new function called showGoogleForm and adapt the follow code to suit. Note for simplicity this example does not contain any error checking which should be added in a production environment.

The url should look something like this: 
https://docs.google.com/forms/.../?usp=pp_url&entry.1739103583=labelname1

```
  var showGoogleForm = function (e) {
    var url = e.currentTarget.googleFormsURL
    url = url.replace('labelname1', 'Some prefilled value')
    url = url.replace('labelname2', 'Another prefilled value')

    // Add the iFrame dynamically to avoid popup issue
    jQuery('<iframe id="#googleform" src="" width="100%" height="100%" frameborder="0" marginheight="0" marginwidth="0">Loading...</iframe>').appendTo('#googleformparent')    

    // Set the prefilled url as the iFrame source
    jQuery('#googleform').attr('src', url)

    // Remove the iframe element when the user closes the dialog to avoid the popup if the user did not submit the form.
    jQuery('#dialogclose').click(function(e) {
        jQuery('#googleform').remove()        
    })

  }
```

