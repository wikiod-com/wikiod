---
title: "Ionic infinite scroll to show load items on demand (Already available data not by Http Request)"
slug: "ionic-infinite-scroll-to-show-load-items-on-demand-already-available-data-not-by-http-request"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Load n number of available data on demand
**HTML :**

    <li class="item" ng-repeat="schedule in Schedules | filter:scheduleSearch | limitTo:numberOfItemsToDisplay">
        Display some data
    </li> 
    <ion-infinite-scroll on-infinite="addMoreItem()" ng-if="Schedules.length > numberOfItemsToDisplay"></ion-infinite-scroll>

**Controller :**

    $scope.numberOfItemsToDisplay = 10; // Use it with limit to in ng-repeat
    $scope.addMoreItem = function(done) {
        if ($scope.Schedules.length > $scope.numberOfItemsToDisplay)
            $scope.numberOfItemsToDisplay += 10; // load number of more items
            $scope.$broadcast('scroll.infiniteScrollComplete')
    }   

Load 10 items every time addMoreItem() call.

