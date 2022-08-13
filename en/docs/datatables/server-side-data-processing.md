---
title: "Server Side Data Processing"
slug: "server-side-data-processing"
draft: false
images: []
weight: 9918
type: docs
toc: true
---

## DataTables 1.10+ Serverside Processing
**Example Table**

There are several ways to inject your data into DataTables. Serverside Processing is just one method. In this manner, DataTables has a pre-configured endpoint to retrieve data from, and that endpoint is responsible for accepting all paging/filtering/sorting requests that DataTables applies. There are a variety of pros and cons to this versus sending your complete dataset back from the server and letting DataTables do it all client-side depending on your use case. 

    var tbl = $('#example').DataTable({
      processing: true,
      serverSide: true,
      ajax: {
        url: '/echo/json/',
        method: 'post'
      },
      columns: [{
        data: 'First',
        title: 'First Name'
      }, {
        data: 'Last',
        title: 'Last Name'
      }]
    });

**Example of hooking preXhr event to send additional data to ajax request**

This event fires directly before the ajax call is made allowing you to modify the request body. This is useful if there is a form that influences the data returned to the table (envision maybe a min/max bar to filter within a date range or similar).

    tbl.on('preXhr.dt', function(ev, settings, data) {
        $.extend(data, {
          min: $('form [name=min]').val(), 
          max: $('form [name=max]').val()
        });
    });

**Explanation of server side requirements for processing request**

In a typical no-option instance of DataTables, all filtering, sorting, and paging is handled in the client browser. With Serverside Processing enabled, these tasks are shifted to the webserver. For very large datasets which may be inefficient to send in their entirety to the client, this can help.

There are several default parameters which are sent by the Datatables request when you configure an ajax endpoint. It can get very long, so rather than itemizing each, a general overview of the server responsibilities may be more helpful.

Starting with any optional parameters you may have supplied to the request, compile your dataset and prepare it for DataTables operations. Use the `search[value]` and `search[regex]` parameters to apply any filtering across all columns/properties. There are also `columns[i][search][value]` and `columns[i][search][regex]` parameters for individual column filters, though these are not commonly used in simple instances of DataTables.

Sort your filtered data using the various `order` parameters. As with the search parameters, there will be a set of ordering parameters per column on which sorting is enabled. `order[i][column]` and `order[i][column][dir]`

Once filtering and sorting is complete, it's time to page the data based on the DataTables request using the `start` and `length` parameters. In .NET this could look like: 

    int start = Int32.TryParse(Request["start"]);
    int length = Int32.TryParse(Request["length"]); 
    return MyData.Skip(start).Take(length);

**Example Response**

This is the rough response structure DataTables expects. Whatever your data is (2d array, array of objects, etc) is nested in the `data` property with the other properties seeding DataTable's core to draw information about paging and filtering (e.g. "Showing records 11-20 of 500 (filtered from 1000)")

    {
      "draw": 1,
      "recordsTotal": 57,
      "recordsFiltered": 57,
      "data": [/*your data goes here*/]
    }

Also see [`.rows.add(data)`][1] and the [`data` option][2] for alternate methods of setting the contents of your DataTable using JSON data. Of course DataTables can also be [initialized from static HTML or HTML5 `data-` attributes][3]


  [1]: https://datatables.net/reference/api/rows.add()
  [2]: https://datatables.net/reference/option/data
  [3]: https://datatables.net/manual/data/

## Load data using ajax with server-side processing.
     var MY_AJAX_ACTION_URL = "path/to/controller.php";
    
     var table = $('#user_list_table').DataTable({
            "autoWidth": true,
            "paging": true,
            "searching": true,
            "ordering": true,
            "language": {
              "zeroRecords": "No data Found",
              "processing": 'Loading'
            },
            "info": false,
            "stripeClasses": [ "odd nutzer_tr", "even nutzer_tr"],
            "columns": [
                {'data':'uid',"visible": false},
                {'data':'name','orderable': true},
                {'data':'phone','orderable': true},
                {'data':'email','orderable': true },
                {'data':'address','orderable': true}
            ],
            "order": [[ 1, "desc" ]],
            "processing": true,
            "serverSide": true,
            "ajax":MY_AJAX_ACTION_URL
        });

The response of the call to `MY_AJAX_ACTION_URL` should be strictly in the below format:

  

    {
      "draw": 1,
      "recordsTotal": 2,
      "recordsFiltered": 2,
      "data": [
          {"name":"XYZ","phone":"678654454","email":"xyz@gmail.com","address":"true"},
          {"name":"ABC","phone":"678654455","email":"abc@gmail.com","address":"true"}
      ]
    }

Note that if the output of the call fails to match the above format, it will result in an error in the initialization of `table`.

## Get JSON data from MySQL table
On the official website of DataTable is an [example][1] of how a server-side process with PHP and MySQL can look. This example is **deprecated** and can no longer be used with PHP 7 (the function "mysql_pconnect" and the associated functions are deprecated, see this [post][2]).

So this function gives you a wellformed JSON data as response:

    <?php
    //include database connection file
    $servername = "localhost";
    $username = "root";
    $password = "root";
    $dbname = "name";
    
    $conn = mysqli_connect($servername, $username, $password, $dbname) or die("Connection failed: " . mysqli_connect_error());
    
    /* check connection */
    if (mysqli_connect_errno()) {
      printf("Connect failed: %s\n", mysqli_connect_error());
      exit();
    }
    
    // initilize all variable
    $params = $columns = $totalRecords = $data = array();
    $params = $_REQUEST;
    //define index of column name
    $columns = array(
        0 =>'id',
        1 =>'name',
        2 =>'salery',
    );
    
    $where = $sqlTot = $sqlRec = "";
    
    // check search value exist
    if( !empty($params['search']['value']) ) {
        $where .=" WHERE ";
        $where .=" ( id LIKE '".$params['search']['value']."%' ";
        $where .=" OR name LIKE '".$params['search']['value']."%' ";
        $where .=" OR salery LIKE '".$params['search']['value']."%' )";
    }
    
    // getting total number records without any search
    $sql = "SELECT * FROM `employees` ";
    $sqlTot .= $sql;
    $sqlRec .= $sql;
    
    //concatenate search sql if value exist
    if(isset($where) && $where != '') {
        $sqlTot .= $where;
        $sqlRec .= $where;
    }
    
     $sqlRec .=  " ORDER BY ". $columns[$params['order'][0]['column']]."   ".$params['order'][0]['dir']."  LIMIT ".$params['start']." ,".$params['length']." ";
    
    $queryTot = mysqli_query($conn, $sqlTot) or die("database error:". mysqli_error($conn));
    
    $totalRecords = mysqli_num_rows($queryTot);
    
    $queryRecords = mysqli_query($conn, $sqlRec) or die("error to fetch employees data");
    
    while( $row = mysqli_fetch_row($queryRecords) ) {
        $data[] = $row;
    }
    
    $json_data = array(
            "draw"            => intval( $params['draw'] ),
            "recordsTotal"    => intval( $totalRecords ),
            "recordsFiltered" => intval($totalRecords),
            "data"            => $data   // total data array
            );
    
    echo json_encode($json_data);  // send data as json format
    ?>

The response looks like and can then be processed by the DataTable:

    {
      "draw": 1,
      "recordsTotal": 3,
      "recordsFiltered": 2,
      "data": [
          {
            "id":"1",
            "name":"Jim",
            "salery":"1000"
          },
          {
            "id":"2",
            "name":"Claudia",
            "salery":"3000"
          },
          {
            "id":"3",
            "name":"Tommy",
            "salery":"2000"
          }
      ]
    }

  [1]: https://datatables.net/development/server-side/php_mysql
  [2]: https://stackoverflow.com/questions/21797118/deprecated-mysql-connect

