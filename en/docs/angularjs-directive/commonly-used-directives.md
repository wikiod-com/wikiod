---
title: "Commonly Used Directives"
slug: "commonly-used-directives"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## ngConfirmClick: Confirm before evaluating expression.
Description:

Evaluate expression after user's confirmation.

Arguments:

* ng-confirm-click:(*expression*) Expression to evaluate when confirmed.
* ng-confirm-message:(*template*) Message to be shown in confirm dialog.

Code:

    Directives.directive("ngConfirmClick", ["$parse","$interpolate",function ($parse,$interpolate) {
        return {
            restrict:"A",
            priority:-1,
            compile:function(ele,attr){
                var fn = $parse(attr.ngConfirmClick, null,  true);
                return function ngEventHandler(scope, ele) {
                    ele.on('click', function (event) {
                        var callback = function () {
                            fn(scope, {$event: "confirm"});
                        };
                        var message = $interpolate(attr.ngConfirmMessage)(scope) || 'Are you sure?';
                        if(confirm(message)) {
                            if (scope.$root.$$phase) {
                                scope.$evalAsync(callback);
                            } else {
                                scope.$apply(callback);
                            }
                        }
                    });
                }
            }
        }
    }]);

[Working Example](https://plnkr.co/edit/3XKGDhmvJra4KPiavZ6d?p=preview)


