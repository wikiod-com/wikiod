---
title: "Azure Active Directory B2C implementation using Hello.js"
slug: "azure-active-directory-b2c-implementation-using-hellojs"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Angularjs- Azure Active Directory B2Cusing Hello.js
Refer the example in https://github.com/NewtonJoshua/Azure-ADB2C-Angularjs-sample


Web app implementation uses [Hello.js](http://adodson.com/hello.js/) that performs identity management with Azure AD B2C . Hello.js is a client-side JavaScript SDK for authenticating with OAuth2 web services and querying REST APIs.

jwtHelper of [angular-jwt](https://github.com/auth0/angular-jwt) will take care of helping you decode the token (JWT) and check its expiration date. JSON Web Tokens are an open, industry standard [RFC 7519](https://tools.ietf.org/html/rfc7519) method for representing claims securely between two parties.

Find the angularjs example below

Let us have a LoginController



    .controller('LoginController', function($scope, $state, $ionicPopup, jwtHelper,  HelloService) {

        // Initialize
        (function initialize() {
                HelloService.initialize().then(function(authResponse) {
                displayUserDetails(getUserData(authResponse));
            });
        })();

        $scope.login = HelloService.login;
        $scope.logout = HelloService.logout;


        // Decode decode the token and display the user details
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

And here is the hello.service that implements Azure AD B2C using Hello.js


    .service('HelloService', function(hello, $q, settings) {

        var network = 'adB2CSignInSignUp';

        this.initialize = function() {
            //initiate all policies
            hello.init({
                adB2CSignIn: settings.adalB2C.clientId,
                adB2CSignInSignUp: settings.adalB2C.clientId,
                adB2CEditProfile: settings.adalB2C.clientId
            }, {
                redirect_uri: '../',
                scope: 'openid ' + settings.adalB2C.clientId,
                response_type: 'token id_token'
            });
            var adB2CSignInSignUpPolicy = getPolicyConfiguredData();
            hello.init(adB2CSignInSignUpPolicy);
            var authResponse = hello(network).getAuthResponse();
            if (authResponse && !authResponse.error) {
                return $q.when(authResponse);
            } else {
                var error = authResponse && authResponse.error ? authResponse.error : '';
                return $q.reject(error);
            }
        };

        this.login = function() {
            hello(network).login({
                display: 'page',
                force: true
            });
        };

        this.logout = function() {
            hello(network).logout({
                force: true
            });
        };

        function getPolicyConfiguredData() {

            var adB2CSignInSignUpPolicy = {};
            adB2CSignInSignUpPolicy[network] = {
                name: 'Azure Active Directory B2C',
                oauth: {
                    version: 2,
                    auth: 'https://login.microsoftonline.com/tfp/' + settings.adalB2C.tenantName + '/' + settings.adalB2C.policy + '/oauth2/v2.0/authorize',
                    grant: 'https://login.microsoftonline.com/tfp/' + settings.adalB2C.tenantName + '/' + settings.adalB2C.policy + '/oauth2/v2.0/token'
                },
                refresh: true,
                scope_delim: ' ',
                // Don't even try submitting via form.
                // This means no POST operations in <=IE9
                form: false
            };
            adB2CSignInSignUpPolicy[network].xhr = function(p) {
                if (p.method === 'post' || p.method === 'put') {
                    //toJSON(p);
                    if (typeof(p.data) === 'object') {
                        // Convert the POST into a javascript object
                        try {
                            p.data = JSON.stringify(p.data);
                            p.headers['content-type'] = 'application/json';
                        } catch (e) {}
                    }
                } else if (p.method === 'patch') {
                    hello.utils.extend(p.query, p.data);
                    p.data = null;
                }
                return true;
            };
            adB2CSignInSignUpPolicy[network].logout = function() {
                //get id_token from auth response
                var id_token = hello(network).getAuthResponse().id_token;
                //clearing local storage session
                hello.utils.store(network, null);

                //redirecting to Azure B2C logout URI
                window.location = ('https://login.microsoftonline.com/' + settings.adalB2C.tenantName + '/oauth2/v2.0/logout?p=' + settings.adalB2C.policy + '&id_token_hint=' +
                    id_token + '&post_logout_redirect_uri=https://login.microsoftonline.com/' + settings.adalB2C.tenantName + '/oauth2/logout');
            };
            return adB2CSignInSignUpPolicy;
        }

    });

