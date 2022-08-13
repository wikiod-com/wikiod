---
title: "Create Dialog in Ionic"
slug: "create-dialog-in-ionic"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Parameters
| Parameters| Detail|
| ------ | ------ |
|  `title: '',` |// String. The title of the popup.|
|  `cssClass: ''`,| // String, The custom CSS class name|
| `subTitle: ''`, |// String (optional). The sub-title of the popup.|
| `template: ''`, |// String (optional). The html template to place in the popup body.|
| `templateUrl: ''`, | // String (optional). The URL of an html template to place in the popup   body.|
| `scope: null,` | // Scope (optional). A scope to link to the popup content. |


The Ionic Popup service allows programmatically creating and showing popup windows that require the user to respond in order to continue.



## Create Dialog in Ionic
     // An alert dialog
     $scope.showAlert = function() {
       var alertPopup = $ionicPopup.alert({
         title: 'Don\'t eat that!',
         template: 'It might taste good'
       });
    
       alertPopup.then(function(res) {
         console.log('Hello your first example.');
       });
     };
    });

