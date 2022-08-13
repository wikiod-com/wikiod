---
title: "Add export buttons to table in Bootstrap 4"
slug: "add-export-buttons-to-table-in-bootstrap-4"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

With the datatables plugin you can add export buttons to your table. 

You can export your table data to excel, pdf or copy it to the clipboard.

This manual is intended for the bootstrap 4 framework.

## Add buttons to table
  
In your JS File add this **option** to your datatable:

      buttons: [ 'excel', 'pdf', 'copy' ]

It will look like:

    $('#yourTableID').DataTable({
      buttons: [ 'excel', 'pdf', 'copy' ]
    });


Add the necessary **css** files for the datatable with the buttons:

    <link rel="stylesheet" type="text/css" href="//cdn.datatables.net/1.10.15/css/dataTables.bootstrap4.min.css"/>
    <link rel="stylesheet" type="text/css" href="//cdn.datatables.net/buttons/1.3.1/css/buttons.bootstrap4.min.css"/>

Add the necessary **javascript** files for the datatable with the buttons:

    <script type="text/javascript" src="//cdn.datatables.net/1.10.15/js/jquery.dataTables.min.js"></script>
    <script type="text/javascript" src="//cdn.datatables.net/1.10.15/js/dataTables.bootstrap4.min.js"></script>
    <script type="text/javascript" src="//cdn.datatables.net/buttons/1.3.1/js/dataTables.buttons.min.js"></script>
    <script type="text/javascript" src="//cdn.datatables.net/buttons/1.3.1/js/buttons.bootstrap4.min.js"></script>
    
    <script type="text/javascript" src="//cdnjs.cloudflare.com/ajax/libs/jszip/3.1.3/jszip.min.js"></script>
    <script type="text/javascript" src="//cdn.rawgit.com/bpampuch/pdfmake/0.1.27/build/pdfmake.min.js"></script>
    <script type="text/javascript" src="//cdn.rawgit.com/bpampuch/pdfmake/0.1.27/build/vfs_fonts.js"></script>
    <script type="text/javascript" src="//cdn.datatables.net/buttons/1.3.1/js/buttons.html5.min.js"></script>


It will look like this picture:

[![The export buttons][1]][1]

If you doesn't see the buttons, add this option:

    dom: 'Blfrtip',
to the datatable options list. So it looks like:

    $('#yourTableID').DataTable({
      dom: 'Blfrtip',
      buttons: [ 'excel', 'pdf', 'copy' ]
    });
You will find more informations to define the table control elements to appear on the page and in what order on this [page][2].

**Note:** 
The prerequisite is, that the jQuery and bootstrap4 files are installed in your project.


  [1]: https://i.stack.imgur.com/jytXy.png
  [2]: https://datatables.net/reference/option/dom

