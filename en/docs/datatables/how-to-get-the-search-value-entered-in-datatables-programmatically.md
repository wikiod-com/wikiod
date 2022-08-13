---
title: "How to get the search value entered in Datatables programmatically?"
slug: "how-to-get-the-search-value-entered-in-datatables-programmatically"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Example
This is the code to filter the Datatables [1.10.7] by value programmatically, you can find it on official documentation.


    function setFilterValue(datatable, value){
        if(datatable !== undefined){
            datatable
                .columns(0)
                .search(value)
                .draw();
        }    
    }


**This is the code to get the value by the previous search.**


    function getFilterValue(datatable){
        var value;
        if(datatable !== undefined){
            value = datatable
                        .settings()[0]
                        .oSavedState
                        .columns[0]
                        .search.search;
        }
        return value;
    }

This approach is useful when you have the cache active (*"stateSave": true*) and you need to know the previous search value after reloaded the page.

