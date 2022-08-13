---
title: "Getting started with jqgrid"
slug: "getting-started-with-jqgrid"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## The first grid
JqGrid is implemented as jQuery plugin, our plugin uses jQuery UI CSS or Bootstrap CSS for styling. Thus one would have to include the corresponding JavaScript and CSS files. The second basic thing, which one should know, is the fact that free jqGrid uses HTML <table> internally. One would have to create an empty <table> element to reserve the place where the grid should be created. 

Finally one should call `jQuery("#tableId").jqGrid({/*options*/});` to create the grid. Different options of jqGrid provides the data of the table body and the information about the outer part of the grid. For example, the code below

    $(function () {
            "use strict";
            $("#grid").jqGrid({
                colModel: [
                    { name: "firstName" },
                    { name: "lastName" }
                ],
                data: [
                    { id: 10, firstName: "Angela", lastName: "Merkel" },
                    { id: 20, firstName: "Vladimir", lastName: "Putin" },
                    { id: 30, firstName: "David", lastName: "Cameron" },
                    { id: 40, firstName: "Barack", lastName: "Obama" },
                    { id: 50, firstName: "François", lastName: "Hollande" }
                ]
            });
        });

The differences between free jqGrid and a standard HTML table are as follows:

 - **Sortable Columns:** One can click on the column header to sort the rows
   by the content in the column.    

 - **Hover Effects**: Free jqGrid gives you the ability to use hovering effects for rows and the cells on the grid.    

 - **Selectable Rows**: One can click on a row of the grid to select/unselect it and can             
                    in-place-edit the cells as well. 

 - **Multi-Selectable Rows**: One can select multiple rows.  

 - **Selectable Rows**: One can click on a row of the grid to select it.

 - **Resizable Columns**: One can resize the columns in an intuitive way, as
   shown in the animated image below.

 ==> ***Some advance differences consists of:*** 

 - **Search/filter** : One can search or filter the table on as eq,lt,lte,gt etc.
        
      --Search : A new popup comes for searching
 
     --Filter : A text-box appears on the top of each column of the grid 


 - **Pagination/ Collapsible** row feature.
 

Full Example:

    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <title>Your page title</title>
        <meta name="viewport" content="width=device-width,initial-scale=1">
        <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/jqueryui/1.11.4/themes/redmond/jquery-ui.min.css">
        <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/free-jqgrid/4.14.0/css/ui.jqgrid.min.css">
        <script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/1.12.4/jquery.min.js"></script>
        <script src="https://cdnjs.cloudflare.com/ajax/libs/free-jqgrid/4.14.0/jquery.jqgrid.min.js"></script>
        <script>
        //<![CDATA[
        $(function () {
            "use strict";
            $("#grid").jqGrid({
                colModel: [
                    { name: "firstName" },
                    { name: "lastName" }
                ],
                data: [
                    { id: 10, firstName: "Angela", lastName: "Merkel" },
                    { id: 20, firstName: "Vladimir", lastName: "Putin" },
                    { id: 30, firstName: "David", lastName: "Cameron" },
                    { id: 40, firstName: "Barack", lastName: "Obama" },
                    { id: 50, firstName: "François", lastName: "Hollande" }
                ]
            });
        });
        //]]>
        </script>
    </head>
    <body>
    <table id="grid"></table>
    </body>
    </html>

