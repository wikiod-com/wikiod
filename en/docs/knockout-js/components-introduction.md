---
title: "Components introduction"
slug: "components-introduction"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Components allow reusable controls/widgets represented by their own view (template) and viewmodel. They were added in Knockout 3.2. Inspired by WebComponents, Knockout allows Components to be defined as Custom Elements, allowing the use of more self-explanatory markup.

## Progress bar (Boostrap)
Component definition

    ko.components.register('progress-bar', {
        viewModel: function(params) {
                var that = this;
        
            // progress is a numeric value between 0 and 100
            that.progress = params.progress;
            
            
            that.progressPercentual = ko.computed(function(){
                return '' + ko.utils.unwrapObservable(that.progress) + '%';
            })
        },
        template:
            '<div class="progress"> <div data-bind="attr:{\'aria-valuenow\':progress}, style:{width:progressPercentual}, text:progressPercentual" class="progress-bar" role="progressbar" aria-valuenow="0" aria-valuemin="0" aria-valuemax="100" style="min-width: 2em;"></div> </div>'
    });

Html usage

    <progress-bar params="progress:5"></progress-bar>  

