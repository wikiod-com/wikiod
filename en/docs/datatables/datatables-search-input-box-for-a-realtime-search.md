---
title: "datatables search input box for a realtime search"
slug: "datatables-search-input-box-for-a-realtime-search"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Search input box for progressive search on the datatable
Below is an example for implementing a Search input box that helps users to search the occurances of a particular value across the datatable. 

In the below example, #report is the div id of the div that contains the search input box. This function is called as soon as the user enters a value in this input box. Since there can be many occurances of a single character, we call the actual search function only when more than 1 character is entered in the search box. 

    $('#report').on('input',function(e){
         if($(this).data("lastval")!= $(this).val()){
         $(this).data("lastval",$(this).val());
            //change action
            if($('#report').val().length >1 ){ 
                searchTable($(this).val()); 
            }
         };
     });

In the searchTable function we use the inbuilt datatables function **fnFilter** to find the matching occurances of the input string. **We can restrict the search to a particular column by passing the column index.** Here we are passing the column index 2. 

    function searchTable(inputVal) {
        var dataTable = $("#report_table").dataTable();
        dataTable.fnFilter(inputVal,2);
    }

If you need to search across all the columns **just make sure you do not pass the index paramete**r.

    function searchTable(inputVal) {
        var dataTable = $("#report_table").dataTable();
        dataTable.fnFilter(inputVal);
    }

