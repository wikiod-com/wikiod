---
title: "Custom filters with ES6"
slug: "custom-filters-with-es6"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## FileSize Filter  using ES6
We have here a file Size filter to describe how to add costum filter to an existing module      :

    let fileSize=function (size,unit,fixedDigit) {
    return size.toFixed(fixedDigit) + ' '+unit;
    };

      let fileSizeFilter=function () {
            return function (size) {
                if (isNaN(size))
                    size = 0;
    
                if (size < 1024)
                    return size + ' octets';
    
                size /= 1024;
    
                if (size < 1024)
                   return fileSize(size,'Ko',2);
    
                size /= 1024;
    
                if (size < 1024)
                   return  fileSize(size,'Mo',2);
    
                size /= 1024;
    
                if (size < 1024)
                   return  fileSize(size,'Go',2);
    
                size /= 1024;
                return  fileSize(size,'To',2);
            };
        };
    export default fileSizeFilter;

The filter call into the module : 

    import fileSizeFilter from 'path...';
    let myMainModule =
        angular.module('mainApp', [])
       .filter('fileSize', fileSizeFilter);


The html code where we call the filter : 

    <div ng-app="mainApp">
      
        <div>        
            <input type="text" ng-model="size" />
        </div>
        <div>
            <h3>Output:</h3>
            <p>{{size| Filesize}}</p>
        </div>
    </div>





