---
title: "Authentication"
slug: "authentication"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Syntax
 - GET https://accounts.google.com/o/oauth2/auth?client_id={clientid}&redirect_uri=urn:ietf:wg:oauth:2.0:oob&scope={Scopes}&response_type=code
 - POST https://accounts.google.com/o/oauth2/token
code={Code from above call}&client_id={ClientId}&client_secret={ClientSecret}&redirect_uri=urn:ietf:wg:oauth:2.0:oob&grant_type=authorization_code
 - POST https://accounts.google.com/o/oauth2/token
client_id={ClientId}&client_secret={ClientSecret}&refresh_token={From above call}&grant_type=refresh_token


## Parameters
| Parameter | Description |
| ------ | ------ |
| Client Id   | From [Google Developer][1] console identifies your project or application |
| secret | From [Google Developer][1] console identifies your project or application |
| redirect_uri| From [Google Developer][1] location where the authentication should be returned to.  In the case of Native applications `urn:ietf:wg:oauth:2.0:oob` can be used to denote localhost |


  [1]: https://console.developers.google.com/apis?project=daimto-tutorials-101&pli=1

In order to access any Google API you need to identify yourself as a developer and identify your project.   We do that by creating a new project on [Google Developers console][1].   

When you create your project you if you want to access the Google Analytics APIs you must enable the APIs you intend to access.

 - **Reporting API**: Access to the Google Analytics Reporting API v4.
 - **Analytics API**: Access to everything else.

Now you must decide how you would like to access the data.

With Google Data there are two types of Data Public and Private.

- public data is not owned by a user.  The metadata API is a public API you don't need to be logged in to access that data.
- The Reporting API contains a users Google Analytics data it is private you cant look at it unless the user has given you permission to access it.

If you are only accessing public data then all you need do is create a public API key and you will be able to access the API in question. If you are going to be accessing private user data then you will need to create either Oauth2 credentials or service account credentials. 


**Authorization Oauth2**

To access private user data we must have permission of the owner of the data to access it.   [Oauth2][2] allows us to request that access from a user.

You have probably seen before Oauth2 before.   

[![enter image description here][3]][3]

The Application "Google Analytics Windows" is requesting access to view the users "Google Analytics Data"

1. Google Analytics windows is the name of the project that was created on Google Developer console.
2. Google Analytics Data is the scope of permissions that we asked for.

***Scope***
We need to tell the user what we intend to do the Google analytics API has two scopes that you can use.

1. https://www.googleapis.com/auth/analytics.readonly
2. https://www.googleapis.com/auth/analytics

It is best to only request the scopes that you need. If you will only be reading a users data then you only need to request the readonly scope.

**Authorization service accounts**

[Service accounts][4] are different in that they are pre-approved.  If you create service account credentials you as the developer can take the service account email and add it as a user on your Google Analytics account **At the account level** this will grant the service account access to the data.  You wont need to pop up the authentication window and request access.   The service account will have access to the data for as long as it is a user on the Google Analytics account.

**Conclusion**

Authentication is needed to access most of the data exposed by the Google Analytics API.


> **You can not use client login / login and password to access any Google
> API as of May 2015.  You must use Open authentication.**

**


  [1]: https://console.developers.google.com/iam-admin/projects?authuser=0
  [2]: https://developers.google.com/identity/protocols/OAuth2
  [3]: http://i.stack.imgur.com/CH3Fc.png
  [4]: https://developers.google.com/identity/protocols/OAuth2ServiceAccount

## Oauth2 C#
Example uses the [Google APIs Dotnet client library](https://developers.google.com/api-client-library/dotnet/).

>PM>  Install-Package Google.Apis.AnalyticsReporting.v4         


        /// <summary>
        /// This method requests Authentcation from a user using Oauth2.  
        /// Credentials are stored in System.Environment.SpecialFolder.Personal
        /// Documentation https://developers.google.com/accounts/docs/OAuth2
        /// </summary>
        /// <param name="clientSecretJson">Path to the client secret json file from Google Developers console.</param>
        /// <param name="userName">Identifying string for the user who is being authentcated.</param>
        /// <returns>DriveService used to make requests against the Drive API</returns>
        public static AnalyticsReportingService AuthenticateOauth(string clientSecretJson, string userName)
        {
            try
            {
                if (string.IsNullOrEmpty(userName))
                    throw new Exception("userName is required.");
                if (string.IsNullOrEmpty(clientSecretJson))
                    throw new Exception("clientSecretJson is required.");
                if (!File.Exists(clientSecretJson))
                    throw new Exception("clientSecretJson file does not exist.");
                
                // These are the scopes of permissions you need.
                string[] scopes = new string[] { AnalyticsReportingService.Scope.AnalyticsReadonly };   // View your Google Analytics Data

                UserCredential credential;
                using (var stream = new FileStream(clientSecretJson, FileMode.Open, FileAccess.Read))
                {
                    string credPath = System.Environment.GetFolderPath(System.Environment.SpecialFolder.Personal);
                    credPath = Path.Combine(credPath, ".credentials/", System.Reflection.Assembly.GetExecutingAssembly().GetName().Name);

                    // Requesting Authentication or loading previously stored authentication for userName
                    credential = GoogleWebAuthorizationBroker.AuthorizeAsync(GoogleClientSecrets.Load(stream).Secrets,
                                                                             scopes,
                                                                             userName,
                                                                             CancellationToken.None,
                                                                             new FileDataStore(credPath, true)).Result;
                }

                // Create Drive API service.
                return new AnalyticsReportingService(new BaseClientService.Initializer()
                {
                    HttpClientInitializer = credential,
                    ApplicationName = string.Format("{0} Authentication", System.Reflection.Assembly.GetExecutingAssembly().GetName().Name),
                });
            }
            catch (Exception ex)
            {
                Console.WriteLine("Create Oauth2 DriveService failed" + ex.Message);
                throw new Exception("CreateOauth2DriveFailed", ex);
            }
        }

## Service Account Authentcation Vb.net
Sample uses [Install-Package Google.Apis.AnalyticsReporting.v4][1]

     Public Shared Function getServiceInitializer() As BaseClientService
        Dim serviceAccountCredentialFilePath = "Path to Json service account key file"                    REM from Google Developers console
        Dim myKeyEMail = "XXX@developer.gserviceaccount.com"   REM from Google Developers console
        Dim scope = Google.Apis.AnalyticsReporting.v4.AnalyticsReportingService.Scope.AnalyticsReadonly


        Try

            Dim credential
            Using stream As New FileStream(serviceAccountCredentialFilePath, FileMode.Open, FileAccess.Read)

                credential = GoogleCredential.FromStream(stream).CreateScoped(scope)

            End Using

            Dim Initializer As New BaseClientService.Initializer()
            Initializer.HttpClientInitializer = credential
            Initializer.ApplicationName = "SRJCGMail"

            Dim service As New AnalyticsReportingService(Initializer)
            Return service

        Catch ex As Exception
            Console.WriteLine(ex.Message)
            Return Nothing
        End Try

    End Function


  [1]: https://www.nuget.org/packages/Google.Apis.AnalyticsReporting.v4/

