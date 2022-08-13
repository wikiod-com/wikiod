---
title: "Azure Active Directory B2C"
slug: "azure-active-directory-b2c"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

Azure AD B2C is a cloud identity management solution for your web and mobile applications. It is a highly available global service that scales to hundreds of millions of identities.

## Azure AD B2C - Angularjs sample (Web and Mobile) app

This sample demonstrates the use of AD B2C for securing an AngularJS based web and mobile app.

Refer https://github.com/NewtonJoshua/Azure-ADB2C-Angularjs-sample

### Azure AD B2C

[Azure AD B2C](https://azure.microsoft.com/en-us/services/active-directory-b2c/) is a cloud identity management solution for your web and mobile applications. It is a highly available global service that scales to hundreds of millions of identities.



### Web app - Hello.js
Web app implementation uses [Hello.js](http://adodson.com/hello.js/) that performs identity management with Azure AD B2C . Hello.js is a client-side JavaScript SDK for authenticating with OAuth2 web services and querying REST APIs.

### Mobile app - ADAL plugin
Mobile app implementation uses [ADAL Cordova Plugin Patch For B2C](https://github.com/jospete/azure-activedirectory-library-for-cordova). This is a chopped version of Active Directory Authentication Library (ADAL) plugin for Apache Cordova apps, [cordova-plugin-ms-adal](https://github.com/AzureAD/azure-activedirectory-library-for-cordova) that works with Azure AD B2C. The original cordova-plugin-ms-adal plugin provides easy to use authentication functionality for your Apache Cordova apps by taking advantage of Active Directory.

### Decode JWT
jwtHelper of [angular-jwt](https://github.com/auth0/angular-jwt) will take care of helping you decode the token (JWT) and check its expiration date. JSON Web Tokens are an open, industry standard [RFC 7519](https://tools.ietf.org/html/rfc7519) method for representing claims securely between two parties.

## 1. Project set up:
1. Clone or download this repository 

    `git clone https://github.com/NewtonJoshua/Azure-ADB2C-Angularjs-sample.git`
2. Install dependencies

    `npm install`

    `bower install`

## 2. AD set up:
ADAL-B2C configuration

1. [Create an Azure AD B2C Directory](https://docs.microsoft.com/en-us/azure/active-directory-b2c/active-directory-b2c-get-started)

    Note the **_Domain name_**, it'll be used as the _tenantName_.
2. [Register your application](https://docs.microsoft.com/en-us/azure/active-directory-b2c/active-directory-b2c-app-registration)
    Follow the instructions to create an application and enable both Web App and Native client. Refer [Register a web application](https://docs.microsoft.com/en-us/azure/active-directory-b2c/active-directory-b2c-app-registration#register-a-web-application) and [Register a mobile/native application](https://docs.microsoft.com/en-us/azure/active-directory-b2c/active-directory-b2c-app-registration#register-a-mobilenative-application)

    Enter the _Reply URL_ as http://localhost:8100 or any port from wher you'll be serving your app.

    In _Application Claims_, select Email Addresses too.

    Note the **_Application ID_** . It'll be used as the _clientId_.
3. [Create a sign-up or sign-in policy](https://docs.microsoft.com/en-us/azure/active-directory-b2c/active-directory-b2c-reference-policies#create-a-sign-up-or-sign-in-policy)

    Note the name of the policy. It'll be used as _policy_.
4. [Create a password reset policy](https://docs.microsoft.com/en-us/azure/active-directory-b2c/active-directory-b2c-reference-policies#create-a-password-reset-policy)

    Note the name of the policy. It'll be used as the _password-reset-policy_ 

## 3. AD settings:

In [settings.value.js](https://github.com/NewtonJoshua/Azure-ADB2C-Angularjs-sample/blob/master/www/js/settings.value.js), enter the following values

* tenantName: Domain name from step 2.1
* clientId: Application ID from step 2.2
* policy: policy name from step 2.3

## 4. Run this sample:
### Web App:
From your shell or command line run

`ionic serve`

### Mobile App:

1. Add platforms

  `cordova platform add android`

  `cordova platform add ios`

2. Generate icon and splash screen resources

  `ionic cordova resources`

3. Build the App
 
  `cordova build`

For more details on building the apps refer the [Cordova](https://cordova.apache.org/) [documentions](https://cordova.apache.org/docs/en/latest/guide/overview/index.html), [Android Platform Guide](https://cordova.apache.org/docs/en/latest/guide/platforms/android/index.html) and [iOS Platform Guide](https://cordova.apache.org/docs/en/latest/guide/platforms/ios/)

## 5. Customize the Azure AD B2C user interface

The Azure AD B2C login screen can be customized to suit our branding. Refer [Customizing the UI](https://docs.microsoft.com/en-us/azure/active-directory-b2c/active-directory-b2c-reference-ui-customization)

In this sample we have two customized UI screens,
* AD B2C Sign in ansd Sign up page: adCustomPages/unified.html
* AD B2C Password reset page: adCustomPages/resetpassword.html

In adCustomPages/unified.html, at line 442 and 445, enter your tenantName, password-reset-policy and clientId

The pages should be uploaded in a blob and their url should be referred in the Azure AD B2C policies.
  * _Create a storage account_ as mentioned in [Upload the sample content to Azure Blob Storage](https://docs.microsoft.com/en-us/azure/active-directory-b2c/active-directory-b2c-reference-ui-customization-helper-tool#upload-the-sample-content-to-azure-blob-storage)
  * Upload the sample AD Pages in the container and note down their url.
  * For the created Blob service Storage account create a CORS rule with '*' as ALLOWED ORIGINS. Select all in ALLOWED METHODS. Enter * for ALLOWED HEADERS and EXPOSED HEADERS as well.
  * [Customize your policy](https://docs.microsoft.com/en-us/azure/active-directory-b2c/active-directory-b2c-reference-ui-customization-helper-tool#customize-your-policy)
  
Now in your application you can see the customized UI.

## Implementation:

If you have to build an application based on this sample remember to install the required dependencies.

### Web App
Dependencies:

`bower install ng-hello --save`

`bower install angular-jwt --save`

refer [hello.service.js](https://github.com/NewtonJoshua/Azure-ADB2C-Angularjs-sample/blob/master/www/js/hello.service.js)

### Mobile App
Dependencies:

`cordova plugin add https://github.com/jospete/azure-activedirectory-library-for-cordova --save`

`bower install angular-jwt --save`

refer [adal.service.js](https://github.com/NewtonJoshua/Azure-ADB2C-Angularjs-sample/blob/master/www/js/adal.service.js)


## Related documents:

1. Overview:
https://docs.microsoft.com/en-us/azure/active-directory-b2c/active-directory-b2c-overview

2. Azure AD - Help secure AngularJS single-page apps by using Azure AD
https://docs.microsoft.com/en-us/azure/active-directory/develop/active-directory-devquickstarts-angular

3. Azure AD B2C: Single-page app sign-in by using OAuth 2.0 implicit flow
https://docs.microsoft.com/en-us/azure/active-directory-b2c/active-directory-b2c-reference-spa



