---
title: "Call Actions using Web API"
slug: "call-actions-using-web-api"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Examples how to call bound and unbound actions.

Note that in a bound function the full function name includes the namespace Microsoft.Dynamics.CRM. Functions that aren't bound must not use the full name.

## Call actions using Web API
    function exampleCloseIncident(id, status){
        var parameters = {};
        var incidentresolution = {};
        incidentresolution["incidentid@odata.bind"] = "/incidents(" + id + ")";
        incidentresolution["@odata.type"] = "Microsoft.Dynamics.CRM.incidentresolution";
        parameters.IncidentResolution = incidentresolution;
        parameters.Status = status;
        
        callUnboundAction("CloseIncident", parameters, true, function(result){
            Xrm.Utility.alertDialog("Incident closed");
        });
    }
    
    function exampleQualifyLead(id){
        var payload = {
            "CreateAccount": createAccount,
            "CreateContact": createContact,
            "CreateOpportunity": false,
            "Status":3
        };
    
        callBoundAction("leads", id, "Microsoft.Dynamics.CRM.QualifyLead", payload, true, function(result){
            Xrm.Utility.alertDialog("Lead qualified");
        });
    }
    
    function callUnboundAction(actionname, payload, async, successCallback, errorCallback) {
        var req = new XMLHttpRequest();
        req.open("POST", encodeURI(getWebAPIPath() + actionname), async);
        req.setRequestHeader("OData-MaxVersion", "4.0");
        req.setRequestHeader("OData-Version", "4.0");
        req.setRequestHeader("Accept", "application/json");
        req.setRequestHeader("Content-Type", "application/json; charset=utf-8");
        req.onreadystatechange = function () {
            if (this.readyState === 4) {
                req.onreadystatechange = null;
                if (this.status == 200 || this.status == 204) {
                    if (this.status == 200) {
                        var result = JSON.parse(this.response);
                    }
    
                    if (successCallback) {
                        successCallback(result);
                    }
                } else {
                    
                    if(errorCallback) {
                        errorCallback(this);
                    }
                    else{
                        Xrm.Utility.alertDialog(this.statusText);
                    }                
                }
            }
        };
        
        if (payload) {
            req.send(JSON.stringify(payload));
        }
        else {
            req.send();
        }
    }
    
    function callBoundAction(entitysetname, id, actionname, payload, async, successCallback, errorCallback) {
        var req = new XMLHttpRequest();
        req.open("POST", encodeURI(getWebAPIPath() + entitysetname + "(" + id + ")/" + actionname), async);
        req.setRequestHeader("OData-MaxVersion", "4.0");
        req.setRequestHeader("OData-Version", "4.0");
        req.setRequestHeader("Accept", "application/json");
        req.setRequestHeader("Content-Type", "application/json; charset=utf-8");
        req.onreadystatechange = function () {
            if (this.readyState === 4) {
                req.onreadystatechange = null;
                if (this.status == 200 || this.status == 204) {
                    if (this.status == 200) {
                        var result = JSON.parse(this.response);
                    }
    
                    if (successCallback) {
                        successCallback(result);
                    }
                } else {
                    
                    if(errorCallback) {
                        errorCallback(this);
                    }
                    else{
                        Xrm.Utility.alertDialog(this.statusText);
                    }  
                }
            }
        };
    
        if (payload) {
            req.send(JSON.stringify(payload));
        }
        else {
            req.send();
        }      
    }
    
    function getClientUrl() {
        //Get the organization URL
        if (typeof GetGlobalContext == "function" &&
            typeof GetGlobalContext().getClientUrl == "function") {
            return GetGlobalContext().getClientUrl();
        }
        else {
            //If GetGlobalContext is not defined check for Xrm.Page.context;
            if (typeof Xrm != "undefined" &&
                typeof Xrm.Page != "undefined" &&
                typeof Xrm.Page.context != "undefined" &&
                typeof Xrm.Page.context.getClientUrl == "function") {
                try {
                    return Xrm.Page.context.getClientUrl();
                } catch (e) {
                    throw new Error("Xrm.Page.context.getClientUrl is not available.");
                }
            }
            else { throw new Error("Context is not available."); }
        }
    }
    
    function getWebAPIPath() {
        return getClientUrl() + "/api/data/v8.2/";
    }

