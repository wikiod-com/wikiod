---
title: "Creating REST APIs in coldfusion"
slug: "creating-rest-apis-in-coldfusion"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

REST APIs are interesting when data should be accessed from everywhere including different languages (server and client side). That requires separation from data and processing. 

## Creating backend
    <cfcomponent displayname="myAPI" output="false">
        <cffunction name="init" access="public" output="no">
            <!--- do some basic stuff --->
            <cfreturn this>
        </cffunction>

        <cffunction name="welcome">
            <cfreturn "Hello World!">
        </cffunction>
    </cfcomponent>

## The interface
    <cfscript>
        api_request = GetHttpRequestData();
        api = createObject("component","myAPI").init();
    </cfscript>

    <cfif api_request.method is 'GET'>
        <cfoutput>#api.welcome()#</cfoutput>
    <cfelseif api_request.method is 'POST'>
        <cfheader statuscode="500" statustext="Internal Server Error" />
    </cfif>

