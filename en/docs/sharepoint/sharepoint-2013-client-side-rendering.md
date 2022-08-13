---
title: "SharePoint 2013 Client Side Rendering"
slug: "sharepoint-2013-client-side-rendering"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

Client Side Rendering (CSR) is a new concept that is introduced in SharePoint 2013. It provides you with a mechanism that allow you to use your own output render for a set of controls that are hosted in a SharePoint page (list views, list forms and search results).

Client Site Rendering is simply when the data is transformed using the client rather than the server. This means using client-side technologies, such as HTML and JavaScript rather than having to write XSLT.


## Apply validations on New/Edit Item Form using CSR
Suppose we have a SharePoint list and it has four fields viz. Title, Full Name, Email, Mobile Number etc. Now if you want to apply custom validation in New/Edit Item form, you can easily do it with CSR code. 
The below mentioned can validate following conditions in forms:

 - Blank values in fields
 - Email id format check with regular expression
 - Mobile Number format Check with regular expression
 - Full Name field should not contain numeric values


**Step : 1** Create a JS file, say `CSRValidations.js` and copy paste following code in the JS file


            (function () { 
         
            // Create object that have the context information about the field that we want to change it's output render  
            var fieldContext = {}; 
            fieldContext.Templates = {}; 
            fieldContext.Templates.Fields = { 
                // Apply the new rendering for Email field on New and Edit Forms 
                "Title": { 
                    "NewForm": titleFieldTemplate, 
                    "EditForm":  titleFieldTemplate
                },
                "Full_x0020_Name": { 
                    "NewForm": fullNameFieldTemplate, 
                    "EditForm":  fullNameFieldTemplate
                },
                "Email": { 
                    "NewForm": emailFieldTemplate, 
                    "EditForm":  emailFieldTemplate 
                },
                "Mobile_x0020_Phone": { 
                    "NewForm": mobilePhoneFieldTemplate, 
                    "EditForm":  mobilePhoneFieldTemplate
                }
            }; 
         
            SPClientTemplates.TemplateManager.RegisterTemplateOverrides(fieldContext); 
         
        })(); 
         
        // This function provides the rendering logic 
        function emailFieldTemplate(ctx) { 
         
            var formCtx = SPClientTemplates.Utility.GetFormContextForCurrentField(ctx); 
         
            // Register a callback just before submit. 
            formCtx.registerGetValueCallback(formCtx.fieldName, function () { 
                return document.getElementById('inpEmail').value; 
            }); 
         
            //Create container for various validations 
            var validators = new SPClientForms.ClientValidation.ValidatorSet(); 
            validators.RegisterValidator(new emailValidator()); 
         
            // Validation failure handler. 
            formCtx.registerValidationErrorCallback(formCtx.fieldName, emailOnError); 
         
            formCtx.registerClientValidator(formCtx.fieldName, validators); 
         
            return "<span dir='none'><input type='text' value='" + formCtx.fieldValue + "'  maxlength='255' id='inpEmail' class='ms-long'> \ <br><span id='spnEmailError' class='ms-formvalidation ms-csrformvalidation'></span></span>"; 
        } 
         
        // Custom validation object to validate email format 
        emailValidator = function () { 
            emailValidator.prototype.Validate = function (value) { 
                var isError = false; 
                var errorMessage = ""; 
         
                //Email format Regex expression 
                //var emailRejex = /\S+@\S+\.\S+/; 
                var emailRejex = /^(([^<>()[\]HYPERLINK "\\.,;:\s@\"\\.,;:\s@\"]+(\.[^<>()[\]HYPERLINK "\\.,;:\s@\"\\.,;:\s@\"]+)*)|(\".+\"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
        
                if (value.trim() == "") { 
                    isError = true; 
                    errorMessage = "You must specify a value for this required field.";
                }else if (!emailRejex.test(value) && value.trim()) {
                    isError = true;
                    errorMessage = "Please enter valid email address";
                } 
         
                //Send error message to error callback function (emailOnError) 
                return new SPClientForms.ClientValidation.ValidationResult(isError, errorMessage); 
            }; 
        }; 
         
        // Add error message to spnError element under the input field element 
        function emailOnError(error) { 
            document.getElementById("spnEmailError").innerHTML = "<span role='alert'>" + error.errorMessage + "</span>"; 
        } 
        
        // This function provides the rendering logic 
        function titleFieldTemplate(ctx) { 
         
            var formCtx = SPClientTemplates.Utility.GetFormContextForCurrentField(ctx);
            // Register a callback just before submit.             
            
            formCtx.registerGetValueCallback(formCtx.fieldName, function () { 
                return document.getElementById('inpTitle').value; 
            }); 
         
            //Create container for various validations 
            var validators = new SPClientForms.ClientValidation.ValidatorSet(); 
            validators.RegisterValidator(new titleValidator()); 
         
            // Validation failure handler. 
            formCtx.registerValidationErrorCallback(formCtx.fieldName, titleOnError); 
         
            formCtx.registerClientValidator(formCtx.fieldName, validators); 
         
            return "<span dir='none'><input type='text' value='" + formCtx.fieldValue + "'  maxlength='255' id='inpTitle' class='ms-long'> \ <br><span id='spnTitleError' class='ms-formvalidation ms-csrformvalidation'></span></span>"; 
        } 
         
        // Custom validation object to validate title format 
        titleValidator = function () { 
            titleValidator.prototype.Validate = function (value) { 
                var isError = false; 
                var errorMessage = ""; 
        
                if (value.trim() == "") { 
                    isError = true; 
                    errorMessage = "You must specify a value for this required field.";
                }
         
                //Send error message to error callback function (titleOnError) 
                return new SPClientForms.ClientValidation.ValidationResult(isError, errorMessage); 
            }; 
        }; 
         
        // Add error message to spnError element under the input field element 
        function titleOnError(error) { 
            document.getElementById("spnTitleError").innerHTML = "<span role='alert'>" + error.errorMessage + "</span>"; 
        } 
        
        // This function provides the rendering logic 
        function mobilePhoneFieldTemplate(ctx) { 
         
            var formCtx = SPClientTemplates.Utility.GetFormContextForCurrentField(ctx); 
         
            // Register a callback just before submit. 
            formCtx.registerGetValueCallback(formCtx.fieldName, function () { 
                return document.getElementById('inpMobilePhone').value; 
            }); 
         
            //Create container for various validations 
            var validators = new SPClientForms.ClientValidation.ValidatorSet(); 
            validators.RegisterValidator(new mobilePhoneValidator()); 
         
            // Validation failure handler. 
            formCtx.registerValidationErrorCallback(formCtx.fieldName, mobilePhoneOnError); 
         
            formCtx.registerClientValidator(formCtx.fieldName, validators); 
         
            return "<span dir='none'><input type='text' value='" + formCtx.fieldValue + "'  maxlength='255' id='inpMobilePhone' class='ms-long'> \ <br><span id='spnMobilePhoneError' class='ms-formvalidation ms-csrformvalidation'></span></span>"; 
        } 
         
        // Custom validation object to validate mobilePhone format 
        mobilePhoneValidator = function () { 
            mobilePhoneValidator.prototype.Validate = function (value) { 
                var isError = false; 
                var errorMessage = ""; 
         
                //MobilePhone format Regex expression 
                //var mobilePhoneRejex = /\S+@\S+\.\S+/; 
                var mobilePhoneRejex = /^[0-9]+$/;
        
                if (value.trim() == "") { 
                    isError = true; 
                    errorMessage = "You must specify a value for this required field.";
                }else if (!mobilePhoneRejex.test(value) && value.trim()) {
                    isError = true;
                    errorMessage = "Please enter valid mobile phone number";
                } 
         
                //Send error message to error callback function (mobilePhoneOnError) 
                return new SPClientForms.ClientValidation.ValidationResult(isError, errorMessage); 
            }; 
        }; 
         
        // Add error message to spnError element under the input field element 
        function mobilePhoneOnError(error) { 
            document.getElementById("spnMobilePhoneError").innerHTML = "<span role='alert'>" + error.errorMessage + "</span>"; 
        } 
        
        // This function provides the rendering logic 
        function fullNameFieldTemplate(ctx) { 
         
            var formCtx = SPClientTemplates.Utility.GetFormContextForCurrentField(ctx); 
         
            // Register a callback just before submit. 
            formCtx.registerGetValueCallback(formCtx.fieldName, function () { 
                return document.getElementById('inpFullName').value; 
            }); 
         
            //Create container for various validations 
            var validators = new SPClientForms.ClientValidation.ValidatorSet(); 
            validators.RegisterValidator(new fullNameValidator()); 
         
            // Validation failure handler. 
            formCtx.registerValidationErrorCallback(formCtx.fieldName, fullNameOnError); 
         
            formCtx.registerClientValidator(formCtx.fieldName, validators); 
         
            return "<span dir='none'><input type='text' value='" + formCtx.fieldValue + "'  maxlength='255' id='inpFullName' class='ms-long'> \ <br><span id='spnFullNameError' class='ms-formvalidation ms-csrformvalidation'></span></span>"; 
        } 
         
        // Custom validation object to validate fullName format 
        fullNameValidator = function () { 
            fullNameValidator.prototype.Validate = function (value) { 
                var isError = false; 
                var errorMessage = ""; 
         
                //FullName format Regex expression 
                var fullNameRejex = /^[a-z ,.'-]+$/i;
        
                if (value.trim() == "") { 
                    isError = true; 
                    errorMessage = "You must specify a value for this required field.";
                }else if (!fullNameRejex.test(value) && value.trim()) {
                    isError = true;
                    errorMessage = "Please enter valid name";
                } 
         
                //Send error message to error callback function (fullNameOnError) 
                return new SPClientForms.ClientValidation.ValidationResult(isError, errorMessage); 
            }; 
        }; 
         
        // Add error message to spnError element under the input field element 
        function fullNameOnError(error) { 
            document.getElementById("spnFullNameError").innerHTML = "<span role='alert'>" + error.errorMessage + "</span>"; 
        } 
        

**Step : 2** Open New Item Form in browser. Edit page and edit web part.

**Step : 3** In Web part properties, Go to Miscellaneous --> JS link --> paste the path of your js file (e.g. ~sitecollection/SiteAssets/CSRValidations.js)

**Step : 4** Save Web part properties and page.


## Change hyperlink of fields/columns inside the list view using CSR
Below example shows how to change the hyperlink for "**ID**" and "**Title(LinkTitle)**" field inside the list view using CSR. 


**Step1 : Create a JS file and paste below code**

    (function () {
        
        function registerRenderer() {
            var ctxForm = {};
            ctxForm.Templates = {};
    
            ctxForm.Templates = {
                Fields : {
                    'LinkTitle': { //------ Change Hyperlink of LinkTitle
                        View : function (ctx) {
                            var url = String.format('{0}?ID={1}', "/sites/Lists/testlist/EditItem.aspx", ctx.CurrentItem.ID);
                            return String.format('<a href="{0}" onclick="EditItem2(event, \'{0}\');return false;">{1}</a>', url, ctx.CurrentItem.Title);
                        }
                    },
                    'ID' : { //------ Change Hyperlink from ID field
                        View : function (ctx) {
                            var url = String.format('{0}?ID={1}', "/IssueTracker/Lists/testlist/DisplayItem.aspx", ctx.CurrentItem.ID);
                            return String.format('<a href="{0}" onclick="EditItem2(event, \'{0}\');return false;">{1}</a>', url, ctx.CurrentItem.ID);
                        }
                    },
    
                }
            };
            SPClientTemplates.TemplateManager.RegisterTemplateOverrides(ctxForm);
        }
        ExecuteOrDelayUntilScriptLoaded(registerRenderer, 'clienttemplates.js');
    
    })();

**Step 2 : GoTo web part properties of List View and add JS Link reference to this newly created js file (e.g. ~sitecollection/SiteAssets/CSRCodeFile.js)**

(Note : Refer your JSlink in this format only. "~sitecollection/YourJSfFilePath".)

Step 3 : Appy and Done

## Hide column from SharePoint list view using CSR.
This example shows how to hide a "Date" field from the SharePoint list view using CSR.

    (function () {
        
        function RemoveFields(ctx) {
            var fieldName = "Date"; // here Date is field or column name to be hide
            var header = document.querySelectorAll("[displayname=" + fieldName + "]")[0].parentNode;
            var index = [].slice.call(header.parentNode.children).indexOf(header) + 1;
            header.style.display = "none";
            for (var i = 0, cells = document.querySelectorAll("td:nth-child(" + index + ")"); i < cells.length; i++) {
                cells[i].style.display = "none";
            }
        }
    
        function registerRenderer() {
            var ctxForm = {};
            ctxForm.Templates = {};
            ctxForm.OnPostRender = RemoveFields;
            SPClientTemplates.TemplateManager.RegisterTemplateOverrides(ctxForm);
        }
        ExecuteOrDelayUntilScriptLoaded(registerRenderer, 'clienttemplates.js');
    
    })();



## Change column display name in list view using CSR
There are cases when you need to change Display Name of column in a list view

e.g. Column Name showing in the view is "IsApprovalNeeded" and you want to appear as "Is Approval Needed?". 

You can, of course change the display name of a column by changing the column title in list settings, but if you want to keep it as it is in the list settings and only modify it on the page preview then you can do it by using CSR(Client-Side-Rendering).

Here is the code...

    (function () {    
    
        function preTaskFormRenderer(renderCtx) {
           modifyColumns(renderCtx);       
        }
    
        function modifyColumns(renderCtx)
        {
          var arrayLength= renderCtx.ListSchema.Field.length;
            for (var i=0; i < arrayLength;i++)
            {
               if(renderCtx.ListSchema.Field[i].DisplayName == 'IsApprovalNeeded')
                 {
                   var newTitle= "Is Approval Needed?";
                   var linkTitleField = renderCtx.ListSchema.Field[i];
                   linkTitleField.DisplayName = newTitle;
                 }
             }
        }
        
        function registerRenderer()
        {
          var ctxForm = {};
          ctxForm.Templates = {};
          ctxForm.OnPreRender = preTaskFormRenderer;
          SPClientTemplates.TemplateManager.RegisterTemplateOverrides(ctxForm);
        }

        ExecuteOrDelayUntilScriptLoaded(registerRenderer, 'clienttemplates.js');
    
    })(); 

