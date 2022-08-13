---
title: "Getting started with azure-active-directory"
slug: "getting-started-with-azure-active-directory"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting azure-active-directory set up or installed.

## Azure Active Directory B2C - Setup
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

## Customize the Azure AD B2C user interface
The Azure AD B2C login screen can be customized to suit our branding. Refer [Customizing the UI](https://docs.microsoft.com/en-us/azure/active-directory-b2c/active-directory-b2c-reference-ui-customization)

 Refer https://github.com/NewtonJoshua/Azure-ADB2C-Angularjs-sample

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

