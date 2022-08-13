---
title: "Azure Active Directory B2C implementation using ADAL Cordova Plugin"
slug: "azure-active-directory-b2c-implementation-using-adal-cordova-plugin"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Azure Active Directory B2C implementation using ADAL Cordova Plugin
Refer the example here: https://github.com/NewtonJoshua/Azure-ADB2C-Angularjs-sample

### Azure AD B2C

[Azure AD B2C](https://azure.microsoft.com/en-us/services/active-directory-b2c/) is a cloud identity management solution for your web and mobile applications. It is a highly available global service that scales to hundreds of millions of identities.

### Mobile app - ADAL plugin
Mobile app implementation uses [ADAL Cordova Plugin Patch For B2C](https://github.com/jospete/azure-activedirectory-library-for-cordova). This is a chopped version of Active Directory Authentication Library (ADAL) plugin for Apache Cordova apps, [cordova-plugin-ms-adal](https://github.com/AzureAD/azure-activedirectory-library-for-cordova) that works with Azure AD B2C. The original cordova-plugin-ms-adal plugin provides easy to use authentication functionality for your Apache Cordova apps by taking advantage of Active Directory.

Find the angularjs/ionicframework example below

Install the dependencies:

`cordova plugin add https://github.com/jospete/azure-activedirectory-library-for-cordova --save`

`bower install angular-jwt --save`

Let us have a LoginController


    .controller('LoginController', function($scope, $state, $ionicPopup, jwtHelper, AdalService) {

        $scope.login = function(){
            AdalService.login().then(function(authResponse) {
            displayUserDetails(getUserData(authResponse));
        });

        $scope.logout = AdalService.logout;

        // Decode decode the token and diaplay the user details
        function getUserData(response) {
            var user = {};
            user.token = response.access_token || response.token;
            var data = jwtHelper.decodeToken(user.token);
            user.expires_in = new Date(response.expires * 1000) || response.expiresOn;
            user.name = data.name;
            user.email = data.emails ? data.emails[0] : '';
            user.id = data.oid;
            return user;
        };

        function displayUserDetails(user) {
            $scope.user = user;
            $ionicPopup.alert({
                title: user.name,
                template: '<b>Email:</b> ' + user.email + '<br> <b>Id:</b> <code>' + user.id + '</code>'
            });
        }

    });

Enter the Azure AD B2C settings here

    .value('settings', {
        // ADAL-B2C configuration
        adalB2C: {
            tenantName: 'Enter your tenant name',
            clientId: 'Enter your client id',
            policy: 'Enter your policy name'
        }
    });

And here is the adal.service that implements Azure AD B2C using ADAL plugin

angular
    .module('azureADB2C')
    .service('AdalService', function($q, $http, settings) {

        var extraQueryParams = 'nux=1';
        var userId = null;
        var redirectUri = 'https://login.microsoftonline.com/tfp/oauth2/nativeclient';
        var authority = 'https://login.microsoftonline.com/' + settings.adalB2C.tenantName;
        var resourceUri = 'https://graph.windows.net';

        this.login = function() {
            var deferredLoginResponse = $q.defer();
            var authContext = new Microsoft.ADAL.AuthenticationContext(authority);
            // Attempt to authorize user silently
            authContext.acquireTokenSilentAsync(resourceUri, settings.adalB2C.clientId, userId, redirectUri, settings.adalB2C.policy)
                .then(function(authResponse) {
                    deferredLoginResponse.resolve(authResponse);
                }, function() {
                    // We require user credentials so triggers authentication dialog
                    authContext.acquireTokenAsync(resourceUri, settings.adalB2C.clientId, redirectUri, userId, extraQueryParams, settings.adalB2C.policy)
                        .then(function(authResponse) {
                            deferredLoginResponse.resolve(authResponse);
                        }, function(err) {
                            deferredLoginResponse.reject(err);
                        });
                });
            return deferredLoginResponse.promise;
        };

        this.logout = function() {
            // Step1: clear cache
            var authContext = new Microsoft.ADAL.AuthenticationContext(authority);
            authContext.tokenCache.clear();

            // Step2: make XmlHttpRequest pointing to the sign out url
            return $http.post(authority + '/oauth2/logout?post_logout_redirect_uri=' + redirectUri);
        };

    });

