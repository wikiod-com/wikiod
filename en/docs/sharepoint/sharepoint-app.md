---
title: "SharePoint App"
slug: "sharepoint-app"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

SharePoint Hosted App

Reference required from site:
http://www.letsharepoint.com/what-is-user-information-list-in-sharepoint-2013/

## SharePoint 2013: Access User Profile Service Data using JSOM in SharePoint 2013 ​

SharePoint 2013: Access User Profile Service Data using JSOM
 in SharePoint 2013
​

In this article, we will learn to manage or access User Profile Service(UPS) Application using JSOM (Javascript Object Model) and create a basic App. Before we start, lets go through basic UPS terminology first.

User Profile – It has all the information of people in an organization in an organized manner. It displays all properties like AccountName, FirstName, LastName, WorkEmail etc. related to a user.

User Profile Service Application – It is considered as centralized location to store all user profiles and also allows the administrators to configure or manage profiles, profile synchronization, My Site, Social Tags etc. It can also pull information from Directory Services like Active Directory.

My Site – A personalized site for individual user to manage their information and store documents, links etc. It provides rich networking and social features by enabling users to share information about themselves or their activities. My Site is accessible by clicking on User Name on top right corner of SharePoint page.

Manage and Access User Profile Data

Since we are going to work using JSOM, we can perform only 'Read ' operations with an exception that Profile picture can be changed using JSOM (or CSOM or REST)

*Server Side Code allows Read/Write both operations.

Retrieve User Profile Properties using JSOM

Lets create a SharePoint Hosted App and retrieve user information in that app –

Launch Visual Studio 2013 and select "App for SharePoint 2013" from New Project.
After selecting above project type, you are presented with a window to connect to SharePoint site and select type of app to be deployed (see below screenshot)
Here I have provided mu SharePoint Online developer site URL and selected SharePoint Hosted App. Click Finish.

3.)    After the project is created, you will see a set of folders/filers added in Solution Explorer added by default to the project.



4.)    If you open "Default.aspx" page, you will find some JavaScript libraries already added to page.


Here we need to add one more library to start working with User Profiles

<script type="text/javascript" src="/_layouts/15/sp.UserProfiles.js"></script

5.) Now open App.js file , there you will find some code already added to display current user name-

Here we will add below code to extract User Profile Properties for a User and code will look like this.

In this code I have provided my id to retrieve few details.

Hit F5 to execute the code and it will deployed to SharePoint site provided initially. But you see it will give message "Error: Access Denied. You do not have permission to perform this action or access this resource."

Reason: You have to set App access permissions for User Profile Service. In solution open AppManifest.XML file and set the permissions using dropdowns as per below screenshot.

Few things to remember :-         

1.)    Format of username to be passed will vary based on environment, like-

For SharePoint Online or on premise forms authentication, it will be –

i:0#.f|membership|user@domain.com

For SharePoint 2013 On Premise windows authentication, it will be –

i:0#.w|domain\user

For SharePoint 2013 On Premise SAML based authentication, it will be –

?i:05:t|adfs with roles|user@domain.com

2.) Set App Access permission in Appmanifest file​

