---
title: "Apps Script Web Apps"
slug: "apps-script-web-apps"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

This is an example form web app, the client-side bit shows some basic UX design, such as a disabled submit button when the form is submitting, or an error message if it fails...etc

The Apps Script bit is very basic. It contains just the code necessary to serve up the html, and to validate the field.

Here is a link to this example app in action: [Example Apps Script Form][1] 

**Note:** You must be signed into a Google account. 

The Apps Script file structure is as so:

 - Code.gs
 - index.html
 - Stylesheet.html
 - JavaScript.html


  [1]: https://script.google.com/macros/s/AKfycbxFaza451zjpq-VjEwHFa3NSolaA6uu3M8_ViV5O2fTZUhGI0E/exec

## Web App Form
**Apps Script:**

<!-- language: lang-js -->

    //Triggered when the page is navigated to, serves up HTML
    function doGet(){
      var template = HtmlService.createTemplateFromFile('index'); 
      return template.evaluate()
          .setTitle('Example App')
          .setSandboxMode(HtmlService.SandboxMode.IFRAME);
    }
    
    //Called from the client with form data, basic validation for blank values
    function formSubmit(formData){
      for(var field in formData){
        if(formData[field] == ''){
          return {success: false, message: field + ' Cannot be blank'}
        }
      }
      return {success: true, message: 'Sucessfully submitted!'};
    }

**HTML**

<!-- language: lang-html -->

    <!DOCTYPE html>
    <html>
    
        <head>
            <base target="_top">
            <link href="https://ssl.gstatic.com/docs/script/css/add-ons1.css" rel="stylesheet">
            <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.1.4/jquery.min.js" type="text/javascript"></script>
        </head>
    
        <body>
            <div id="mainForm">
                <h1>Example Form</h1>
                <form>
                    <div>
                        <div class="inline form-group">
                            <label for="name">Name</label>
                            <input id="nameInput" style="width: 150px;" type="text">
                        </div>
                    </div>
                    <div>
                        <div class="inline form-group">
                            <label for="city">City</label>
                            <input id="cityInput" style="width: 150px;" type="text">
                        </div>
                        <div class="inline form-group">
                            <label for="state">State</label>
                            <input id="stateInput" style="width: 40px;" type="text">
                        </div>
                        <div class="inline form-group">
                            <label for="zip-code">Zip code</label>
                            <input id="zip-codeInput" style="width: 65px;" type="number">
                        </div>
                    </div>
                    <div class="block form-group">
                        <label for="typeSelect">Type</label>
                        <select id="typeSelect">
                            <option value="">
                            </option>
                            <option value="Type 1 ">
                                Type 1
                            </option>
                            <option value="Type 2 ">
                                Type 2
                            </option>
                            <option value="Type 3 ">
                                Type 3
                            </option>
                            <option value="Type 4 ">
                                Type 4
                            </option>
                        </select>
                    </div>
                    <button class="action" id="submitButton" type="button">Submit</button>
                    <button class="clear" id="clearFormButton" type="button">Clear Form</button>
                </form>
                <div class="hidden error message">
                    <div class="title">Error:</div>
                    <div class="message"></div>
                </div>
                <div class="hidden success message">
                    <div class="title">Message:</div>
                    <div class="message">Sucessfully submitted</div>
                </div>            
            </div>
            <?!= HtmlService.createHtmlOutputFromFile('JavaScript').getContent(); ?>
            <?!= HtmlService.createHtmlOutputFromFile('Stylesheet').getContent(); ?>
        </body>
    
    </html>

**CSS**

<!-- language: lang-css-->

    <style>
    .hidden {
        display: none;
    }
    
    .form-group {
        margin: 2px 0px;
    }
    
    #submitButton {
        margin: 4px 0px;
    }
    
    body {
        margin-left: 50px;
    }
    
    .message {
       padding: 2px;
       width: 50%;    
    }
    
    .message > * {  
       display: inline-block;
    }
    
    .message .title {
        font-weight: 700;
        font-size: 1.1em;     
    }
    
    .success.message {
        border: 1px solid #5c9a18;
        background: #e4ffe4;
        color: #2a8e2a;
    }
    
    .error.message {
        background: #f9cece;
        border: 1px solid #7d2929;
    }
    
    .error.message .title {
        color: #863030;
    }
    
    button.clear {
        background: -moz-linear-gradient(top, #dd6e39, #d17636);
        background: -ms-linear-gradient(top, #dd6e39, #d17636);
        background: -o-linear-gradient(top, #dd6e39, #d17636);
        background: -webkit-linear-gradient(top, #dd6e39, #d17636);
        background: linear-gradient(top, #dd6e39, #d17636);
        border: 1px solid transparent;
        color: #fff;
        text-shadow: 0 1px rgba(0, 0, 0, .1);
    }
    
    button.clear:hover {
        background: -moz-linear-gradient(top, #ca602e, #bd6527);
        background: -ms-linear-gradient(top, #ca602e, #bd6527);
        background: -o-linear-gradient(top, #ca602e, #bd6527);
        background: -webkit-linear-gradient(top, #ca602e, #bd6527);
        background: linear-gradient(top, #ca602e, #bd6527);
        border: 1px solid transparent;
        color: #fff;
        text-shadow: 0 1px rgba(0, 0, 0, .1);
    }
    </style>


**JavaScript**

<!-- language: lang-js -->

    <script>
    var inputs = [
      'nameInput',
      'cityInput',
      'stateInput',
      'zip-codeInput',
      'typeSelect'  
    ];
    
    $(function(){
      var pageApp = new formApp();
      $('#submitButton').on('click', pageApp.submitForm);
      $('#clearFormButton').on('click', pageApp.clearForm);
    });
    
    var formApp = function(){
      var self = this;
      
      //Clears form input fields, removes message, enables submit
      self.clearForm = function(){
        for(var i = 0; i < inputs.length; i++){
            $('#'+inputs[i]).val('');
        }
        toggleSubmitButton(false);
        setErrorMessage(false);
        setSuccessMessage(false);
      }
      
      //Submits the form to apps script
      self.submitForm = function(){
        toggleSubmitButton(true);
        setSuccessMessage(false);
        setErrorMessage(false);
        
        google.script.run
            .withSuccessHandler(self.sucessfullySubmitted)
            .withFailureHandler(self.failedToSubmit)
            .formSubmit(self.getFormData());    
      };
      
      //Retrieves the form data absed on the input fields
      self.getFormData = function(){
        var output = {};
        for(var i = 0; i < inputs.length; i++){
            output[inputs[i]] = $('#'+inputs[i]).val();
        }
        console.log(output)
        return output;  
      }
      
      //When the apps script sucessfully returns
      self.sucessfullySubmitted = function(value){
        if(value.success){
          setSuccessMessage(true, value.message);
        } else {
          setErrorMessage(true, value.message);
          toggleSubmitButton(false);
        }
      }
      
      //When the apps script threw an error
      self.failedToSubmit = function(value){
        toggleSubmitButton(false);
        setErrorMessage(true, value.message);
      }
    }
    
    //Disables/enables the submit button
    function toggleSubmitButton(disabled){
      $('#submitButton').prop('disabled', disabled);
    }
    
    //Sets the general message box's message and enables or disabled the error box
    function setSuccessMessage(show, message){
      if(show){
        $('.success.message').removeClass('hidden');
        $('.success.message .message').text(message);
      } else {
        $('.success.message').addClass('hidden');
        $('.success.message .message').text('');
      }
    }
    
    //Sets the error message box's message and enables or disabled the error box
    function setErrorMessage(show, message){
      if(show){
        $('.error.message').removeClass('hidden');
        $('.error.message .message').text(message);
      } else {
        $('.error.message').addClass('hidden');
        $('.error.message .message').text('');
      }
    }
    
    function getFormData(){
      var output = {};
      for(var i = 0; i < inputs.length; i++){
          output[inputs[i]] = $('#'+inputs[i]).val();
      }
      return output;
    }
    </script>



