---
title: "Email Verification after Sign Up"
slug: "email-verification-after-sign-up"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Syntax
 - Send email verification to logged in user's email address on file. Firebase allows you to [customize what your email entails][1]
 - When email hits user's email account, the user clicks on
 - Using your Router of choice (used angular-ui-router in above example), intercept parameters in the URL.
 - Chew the params using the `applyCode` function in Firebase.
 - See below for the functions involved in the above process.


  [1]: https://console.firebase.google.com/project/your-project-name-here/authentication/emails

## Parameters
| The Function... | Does |
| ------ | ------ |
| [sendEmailVerification()][1]  | Sends a verification email to a user.   |
| [applyActionCode()][2] | Applies the action code which changes `emailVerified` from `false` to `true`


  [1]: https://firebase.google.com/docs/reference/js/firebase.User#sendEmailVerification
  [2]:https://firebase.google.com/docs/reference/js/firebase.auth.Auth.html#applyActionCode

The above pretty much sums up how to use the email verification scheme with Firebase. So far, it stands as one of the simplest ways to verify email I have seen.

There's a bit of an extended explanation of the above example available on [Email Verification with Firebase 3.0 SDK.][1]


  [1]: https://blog.khophi.co/email-verification-firebase-3-0-sdk/

## Send-cum-Process Verification Action Code - AngularJS
    // thecontroller.js
    $scope.sendVerifyEmail = function() {
        console.log('Email sent, whaaaaam!');
        currentAuth.sendEmailVerification();
      }

    // where currentAuth came from something like this:
    // routerconfig

    ....
    templateUrl: 'bla.html',
    resolve: {
        currentAuth:['Auth', function(Auth) {
          return Auth.$requireSignIn() // this throws an AUTH_REQUIRED broadcast
        }]
      }
    ...

    // intercept the broadcast like so if you want:
   
    ....

    $rootScope.$on("$stateChangeError", function(event, toState, toParams, fromState, fromParams, error) {
          if (error === "AUTH_REQUIRED") {
            $state.go('login', { toWhere: toState });
           }
        });
    ....

    // So user receives the email. How do you process the `oobCode` that returns?
    // You may do something like this:

    // catch the url with its mode and oobCode
    .state('emailVerify', {
      url: '/verify-email?mode&oobCode',
      templateUrl: 'auth/verify-email.html',
      controller: 'emailVerifyController',
      resolve: {
        currentAuth:['Auth', function(Auth) {
          return Auth.$requireSignIn()
        }]
      }
    })

    // Then digest like so where each term is what they sound like:

    .controller('emailVerifyController', ['$scope', '$stateParams', 'currentAuth', 'DatabaseRef',
      function($scope, $stateParams, currentAuth, DatabaseRef) {
        console.log(currentAuth);
        $scope.doVerify = function() {
          firebase.auth()
            .applyActionCode($stateParams.oobCode)
            .then(function(data) {
              // change emailVerified for logged in User
              toastr.success('Verification happened', 'Success!');
            })
            .catch(function(error) {
              $scope.error = error.message;
              toastr.error(error.message, error.reason, { timeOut: 0 });
            })
        };
      }
    ])
        
    


