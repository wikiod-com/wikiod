---
title: "Working with Modal Dialog Boxes with JavaScript"
slug: "working-with-modal-dialog-boxes-with-javascript"
draft: false
images: []
weight: 9854
type: docs
toc: true
---

## Syntax
- var options = SP.UI.$create_DialogOptions();

- var modalDialog = SP.UI.ModalDialog.showModalDialog(options);

## Parameters
| options Property  | Description  |
| ------ | ------ |
| title  | A string that contains the title of the dialog |
| url | A string that contains the URL of the page that appears in the dialog. Either **url** or **html** must be specified. **url** takes precedence over **html**.|
| html | An HTML element to display within the dialog. |
| x | The x-offset of the dialog as an integer value. |
| y | The y-offset of the dialog as an integer value. |
| width | The width of the dialog as an integer value. If unspecified and **autosize** is **false** the width is set to 768px |
| height | The height of the dialog as an integer value. If unspecified and **autosize** is **false** the height is set to 576px |
| allowMaximize | A Boolean value specifying whether the **Maximize** button should be shown. |
| showMaximized | A Boolean value specifying whether the dialog opens maximized. |
| showClose | A Boolean value specifying whether the **Close** button appears on the dialog. |
| autoSize | A Boolean value that specifies whether the dialog platform handles dialog sizing automatically. |
| dialogReturnValueCallback | A function pointer that specifies the return callback function. Function takes two parameters: a *dialogResult* of type SP.UI.DialogResult Enumeration, and a *returnValue* object that contains any data returned by the dialog. |
| args | An object that contains data that are passed to the dialog. |

The `SP.UI.ModalDialog` namespace was introduced to the [JavaScript Object Model ][1]with SharePoint 2010, and is available in subsequent SharePoint versions 2013, Office365, and 2016.

Additional reference materials: 
 - [MSDN Reference for SP.UI.ModalDialog.showModalDialog(options)](https://msdn.microsoft.com/en-us/library/office/ff410058%28v=office.14%29.aspx?f=255&MSPPError=-2147217396)
 - [MSDN Reference for SP.UI.DialogResult Enumeration](https://msdn.microsoft.com/en-us/library/office/ff409060%28v=office.14%29.aspx?f=255&MSPPError=-2147217396)


  [1]: https://www.wikiod.com/sharepoint/working-with-javascript-client-object-model-jsom

## Perform an Action when a Dialog Box is Closed
    SP.SOD.executeOrDelayUntilScriptLoaded(showDialog,"sp.js");

    function showDialog(){
        var options = SP.UI.$create_DialogOptions();
        options.url = "/mySite/lists/myList/NewForm.aspx";
        options.dialogReturnValueCallback = myCallBackFunction;
        SP.UI.ModalDialog.showModalDialog(options);    
        function myCallBackFunction(result,data){
            switch(result){
                case SP.UI.DialogResult.invalid: 
                    alert("The dialog result was invalid"); 
                    break;
                case SP.UI.DialogResult.cancel: 
                    alert("You clicked cancel or close"); 
                    break;
                case SP.UI.DialogResult.OK: 
                    alert("You clicked OK, creating an item in the list."); 
                    break;
            }
        }
    }

## Show an Existing Page in a Dialog
    SP.SOD.executeOrDelayUntilScriptLoaded(showDialog,"sp.js");

    function showDialog(){
        SP.UI.ModalDialog.showModalDialog(
            { url: "/org/it/web/wik/Lists/ExampleCode/DispForm.aspx?ID=6" }
        );
    }

## Show a Custom Dialog
    SP.SOD.executeOrDelayUntilScriptLoaded(showDialog,"sp.js");

    function showDialog(){
        var dialogOptions = SP.UI.$create_DialogOptions();
        dialogOptions.title = "Your Title Here!";
        var dummyElement = document.createElement("div");
        dummyElement.style.textAlign = "center";
        dummyElement.appendChild(document.createElement("br"));
        dummyElement.appendChild(document.createTextNode("Some beautifully crafted text."));
        dummyElement.appendChild(document.createElement("br"));
        dialogOptions.html = dummyElement;
        SP.UI.ModalDialog.showModalDialog(dialogOptions);    
    }

