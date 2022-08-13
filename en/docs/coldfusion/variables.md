---
title: "Variables"
slug: "variables"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Parameters
|Attribute|Description|
|---------|-----------|
|name|(Required) Name of the parameter/variable.|
|default|(Optional) Value to set parameter to if it does not exist.|
|max|(Optional) The maximum valid value; used only for range validation.|
|min|(Optional) The minimum valid value; used only for range validation.|
|pattern|(Optional) A JavaScript regular expression that the parameter must match; used only for regex or regular_expression validation.|
|type|(Optional) The valid format for the data.|

## Using cfparam
The `<cfparam>` tag creates a variable if it does not already exist. You can assign a default value using the `default` attribute. This can be used if you want to create a variable, but don't want to overwrite it if it has been previously created elsewhere.

Here the variable hasn't been set previously, so it will be assigned with the `<cfparam>` tag.

    <cfparam name="firstName" default="Justin">
    <cfoutput>
        Hello #firstName#
    </cfoutput>

Here the variable has already been assigned using the `<cfset>` tag, so this value will override the default value in the `<cfparam>` tag.

    <cfset firstname="Justin">
    
    <cfparam name="firstName" default="Barney">
    <cfoutput>
        Hello #firstName#
    </cfoutput>

## Checking if a Variable Exists
You can check if a variable has been defined in a scope by using ColdFusion's built in `StructKeyExists()` function. This can be used inside a `<cfif>` tag to prevent error messages in the event you attempt to refer to a variable that does not exist. You can also use this function to determine whether a user has performed a certain action or not. The syntax for the function is 

    StructKeyExists(structure, "key")

The following example checks if the variable `firstName` exists in the `variables` scope.

    <cfif StructKeyExists(variables, "firstName")>
        Hello #variables.firstname#!
    <cfelse>
        Hello stranger!
    </cfif>

Alternatively, you may use the function: 

    isDefined("scopeName.varName")

To avoid ambiguity, it is recommended to declare the scope. For example, If you have a variable in the scope `test`

    <cfset test.name = "Tracy" />

and you test for `name` in the global scope, you will get a result of `true`.


    isDefined("name") <!--- true --->
    isDefined("x.name") <!--- false--->
    isDefined("test.name") <!--- true --->

## Using cfset
You can set a ColdFusion variable using the `<cfset>` tag. To output the variable, you need to surround the variable name with hash `#` symbols and enclose it within `<cfoutput>` tags.

    <cfset variablename="World!">
    <cfoutput>
        Hello #variablename#
    </cfoutput>

## Setting a variable scope
It is a common practice to set application variables to an object scope. This keeps them easy to identify and distinguish from variables in other scopes. 

The Variables scope in a CFC is private to the CFC. When you set variables in this scope, they cannot be seen by pages that invoke the CFC.

    <cfparam name="variables.firstName" default="Timmy">
    <cfset variables.firstName="Justin">

Scopes shared with the calling page include: Form, URL, Request, CGI, Cookie, Client, Session, Application, Server, and Flash. Variables in these scopes are also available to all pages that are included by a CFC.

*CFC:*

    <cfset url.sessionId="23b5ly17">

    <cfinclude template="check_session.cfm">

*check_session.cfm*

    <cfif url.sessionId eq "23b5ly17">
        <p>Welcome back!</p>
    </cfif>

