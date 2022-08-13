---
title: "Getting started with datatables"
slug: "getting-started-with-datatables"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## DataTables API
DataTables comes with an extensive API which is used to manipulate or obtain information about the DataTables on a page.

The API can be accessed in 3 ways:


    var table = $('#tableid').DataTable(); //DataTable() returns an API instance immediately
    var table = $('#tableid').dataTable().api(); //dataTable() returns a jQuery object
    var table = new $.fn.dataTable.Api('#tableid');

Once the object has been set, you can call any of the API functions on that object.

    var columns = table.columns();

A more complex example is [adding some rows][1] to your table:



    table.rows.add( [ {
            "name":       "John Doe",
            "employee_id":   "15135",
            "department":     "development",
        }, {
            "name":       "Jane Smith",
            "employee_id":   "57432",
            "department":     "quality assurance",
        } ] )
        .draw();

The full list of API functions can be found [here][2].


  [1]: https://datatables.net/reference/api/rows.add()
  [2]: https://datatables.net/reference/api/

## Initializing a minimal DataTables:
The below code will turn the table with an id of `tableid` into a DataTable, as well as return a DataTables API instance:
    
    $(document).ready(function() {
        $('#tableid').DataTable();
    });

Compare this to the below code, which will turn the table into a DataTable but will not return a DataTables API instance:

    $(document).ready(function() {
        $('#tableid').dataTable();
    });

See the [DataTables API documentation][1] section for more details on what can be done with the DataTables API instance.


  [1]: https://www.wikiod.com/datatables/getting-started-with-datatables#DataTables API

## Installation
Have required `JavaScript` and `CSS` files included in your `index.html`. You can do this by either using the CDN files availiable at the following paths:

    <link rel="stylesheet" type="text/css" href="//cdn.datatables.net/1.10.12/css/jquery.dataTables.css">
      
    <script type="text/javascript" charset="utf8" src="//cdn.datatables.net/1.10.12/js/jquery.dataTables.js"></script>

Or by downloading individual local files and hosting them yourself. To get a comprehensive package of required `JavaScript` and `CSS` files visit the DataTables [download builder][1] which will allow you to pick and choose features you need and condense them into a single package (or offer individual files). Include these in the order displayed at the bottom of the page.

DataTables depends on `jQuery`, so include it before `jquery.dataTables.js`:


    <script type="text/javascript" src="https://code.jquery.com/jquery-3.1.0.min.js"></script>


DataTables are also available through NPM

    npm install datatables.net    # Core library
    npm install datatables.net-dt # Styling

 and Bower

    bower install --save datatables.net
    bower install --save datatables.net-dt


  [1]: https://datatables.net/download/index

## Feature Enable/Disable (DataTables Options)
DataTables has the capability to enable or disable a number of its features, such as paging or searching. To choose these options, simply select them in your initialization:

    $(document).ready(function() {
        $('#tableid').DataTable( {
            "paging":   false, //Turn off paging, all records on one page
            "ordering": false, //Turn off ordering of records
            "info":     false  //Turn off table information
        } );
    } );

Note that the quotation marks around the option names are optional:

    paging: false,
    ordering: false,
    info: false
Is also perfectly valid.

A full list of options can be found [here][1], along with descriptions of the uses of each option.

These options can only be set once, when the table is initialised. However, you can work around this limitation by adding:

     destroy: true

  [1]: https://datatables.net/reference/option/

