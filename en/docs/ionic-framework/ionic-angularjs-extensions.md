---
title: "Ionic AngularJS extensions"
slug: "ionic-angularjs-extensions"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Ionic offers a variety of Javascript AngularJS extensions for you to use. These extensions can be anything from normal form inputs to modal windows and makes coding your basic app a lot faster using these ready extensions.

## Form inputs
Ionic inputs are no different from normal HTML inputs but they are styled differently and used as a directive. You can also use normal HTML inputs in Ionic apps. Here are some basic examples that Ionic offers ready-to-go.

**Checkbox:**

    <ion-checkbox ng-model="checkbox">Label</ion-checkbox>

**Radio button:**

    <ion-radio ng-model="radio" ng-value="'radiovalue'">Value</ion-radio>

**Toggle:**

    <ion-toggle ng-model="toggle" toggle-class="toggle-calm">Toggle</ion-toggle>



## Modal windows
Ionic has it's own extension for displaying a modal window. Modals can be created by inserting the template straight to the view with a `<script>` tag or by using a separate template file. In this example we are assuming you have a html file named `modal-template.html` in a folder called `templates`. You set the template path in the modal initialisation function with `fromTemplateUrl`.

# Creating the modal object in the scope #

**HTML**

    <ion-modal-view>
      <ion-header-bar>
        <h1>Modal title</h1>
      </ion-header-bar>
      <ion-content>
        <p>Modal content</p>
      </ion-content>
    </ion-modal-view> 

**Controller**

    $ionicModal.fromTemplateUrl('/templates/modal-template.html', {
      scope: $scope,
      animation: 'slide-in-up'
    }).then(function(modal) {
      $scope.modal = modal;
    });

# Control the modal #

You can then control the modal with the following commands.

**Open modal**

    $scope.modal.show();

**Close modal**

    $scope.modal.hide();

**Remove modal**

    $scope.modal.remove();

# Modal events #

You can listen to modal events with the following functions.

**Modal is hidden**

    $scope.$on('modal.hidden', function() {
      // Do something when modal is hidden
    });

**Modal is removed**

    $scope.$on('modal.removed', function() {
      // Do something when modal is removed
    });

