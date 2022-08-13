---
title: "Global Variables on Visualforce pages"
slug: "global-variables-on-visualforce-pages"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## $Resource
Use the `$Resource` variable to reference static resources.

```
<img src="{!$Resource.myImage}" />
```

Can be used in conjunction with the `URLFOR` function to reference files inside of a zipped static resource:

```
<apex:includeScript value="{!URLFOR($Resource.myResources, 'js/app.js')}" />
```

## $Label
The `$Label` variable can be used to display text defined in your custom labels.

```
<apex:outputText value="{!$Label.Welcome_Message}" />
```

You can do formatting with labels as well. Suppose you have a custom label named `Welcome_Message` defined as

    Welcome to our site, {0}!

You could use the label to display a formatted message:

```
<apex:outputText value="{!$Label.Welcome_Message}" rendered="{!NOT(ISBLANK($User.ContactId))}">
  <apex:param value="{!$User.Contact.FirstName}" />
</apex:outputText>

## $User
The `$User` variable gives you access to all the standard and custom fields on the `User` object for the currently logged in user.

```
You are logged in as a user from {!$User.CompanyName}.
```

