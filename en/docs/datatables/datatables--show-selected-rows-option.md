---
title: "datatables - Show Selected Rows option"
slug: "datatables---show-selected-rows-option"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Show Selected Rows only
Its common for datatables to have a checkbox to select multiple rows. If the data is spread across multiple pages, it could be difficult for the user to view the records he selected. To enable the user view all the selected records in one go, we usually use a hyperlink that when clicked displays only the selected rows from the datatable. This link can be used a toggle between viewing selected records and all records.  

    $(sAnchor).click(function() {
                    $('#show_selected').text(function(_,txt) {
                        var checked = 0;
                        var ret='';
                        var dataTable = $("#report_table").dataTable();
                        if ( txt == 'Show Selected Reports' ) {
                              dataTable.fnFilter('Checked',8);
                              ret = 'Show All Reports';
                        }else{
                              dataTable.fnFilter('',8);
                              ret = 'Show Selected Reports';
                        }
                        return ret;
                     });
     });

In the above method, on click of the Select All hyperlink, If the hyperlink div text is 'Show Selected Reports' we filter the datatable to display only those rows for which the checkbox is checked. We use the inbuilt Datatables API function **fnFilter**. 

We pass 2 parameters to this method - query string and the index of the column to filter. In this case the value of the checkbox will be **'Checked'** if its selected on the UI and the index of the column containing the checkboxes is 8. Hence we are passing 'Checked'and 8 as pamaeters to the fnFilter function. After filter, we toggle the link to display 'Show All Reports.' 

When the user clicks on Show All Reports, we pass an empty string to the fnFilter function as the query string. So it displays all the records. 

