---
title: "Web Forms For Marketeers (WFFM)"
slug: "web-forms-for-marketeers-wffm"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Web Forms For Marketeers is the popular Sitecore module that allows to create forms in Sitecore and customize and extend their behavior using save actions.

More information about the module can be found in 
https://dev.sitecore.net/Downloads/Web_Forms_For_Marketers.aspx


## Submit the WFFM form programmatically
This example shows how to submit the WFFM form in code.

    var controlResults = new List<ControlResult>();
    controlResults.Add(new ControlResult(Pdf_Request_Form.Name.ItemID.ToString(), "Name", name, string.Empty));
    controlResults.Add(new ControlResult(Pdf_Request_Form.Email.ItemID.ToString(), "Email", email, string.Empty));    
    FormDataHandler.ProcessData(Pdf_Request_Form.ItemID, controlResults.ToArray(), new IActionDefinition[] {}, DependenciesManager.ActionExecutor);

