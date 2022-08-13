---
title: "md-chips"
slug: "md-chips"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Static chips
This example uses `<md-chips>` and `<md-chip>`.

_NOTE_: Static chips cannot be selected, removed or edited, and are not part of any model.
If no `ng-model` is provided, there are no input elements in `<md-chips>`.

**index.html**:
<!-- language: lang-html -->
    <md-content ng-controller="ChipController">
    <md-chips>
        <md-chip>Test</md-chip>
        <md-chip>Another chip</md-chip>
        <md-chip>{{chip}}</md-chip>
    </md-chips>

**app.js**:
<!-- language: lang-js -->
    var app = angular.module('SomeApp', [/* Your dependencies here */ 'ngMaterial'])
    app.controller('ChipController', function($scope) {
        $scope.chip = "$scope";
    })

## Setting the delay to select a new chip before refocusing on the input (≥1.1.2)
<!-- if version [gte 1.1.2] -->
**(This content is only relavant for versions 1.1.2 and up)**

From versions 1.1.2 and up, you can set the delay to select a new chip before refocusing on the input.

Use the `md-chip-append-delay` attribute to set it (in milliseconds):

Example:

<!-- language: lang-html -->
    <md-chips md-chip-append-delay="500" ng-model="chipsModel" placeholder="Chips"></md-chips>

According to `angular-material`:
> This is necessary for keyboard accessibility for screen readers. It defaults to 300ms and any number less than 300 can cause issues with screen readers (particularly JAWS and sometimes NVDA).
<!-- end version if -->

