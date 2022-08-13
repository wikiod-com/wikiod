---
title: "ABAP GRID List Viewer (ALV)"
slug: "abap-grid-list-viewer-alv"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Creating and Displaying an ALV
This example portrays the most simple ALV creation using the `cl_salv_table` class and no additional formatting options. Additional formatting options would be included after the `TRY` `ENDTRY` block and before the `alv->display( )` method call.

All subsequent examples using the ABAP Objects approach to ALV creation will use this example as a starting point.

    DATA: t_spfli       TYPE STANDARD TABLE OF spfli,
          alv           TYPE REF TO cl_salv_table,
          error_message TYPE REF TO cx_salv_msg.

    " Fill the internal table with example data
    SELECT * FROM spfli INTO TABLE t_spfli.

    " Fill ALV object with data from the internal table
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = alv
          CHANGING
            t_table      = t_spfli ).
      CATCH cx_salv_msg INTO error_message.
        " error handling
    ENDTRY.

    " Use the ALV object's display method to show the ALV on the screen
    alv->display( ).

## Optimize ALV Column Width
This example shows how to optimize the column width so that column headings and data are not chopped off. 

    alv->get_columns( )->set_optimize( ).

## Hide Columns in an ALV
This example hides the `MANDT` (client) field from the ALV. Note that the parameter passed to `get_column( )` *must* be capitalized in order for this to work.

    alv->get_columns( )->get_column( 'MANDT' )->set_visible( if_salv_c_bool_sap=>false ).

## Rename Column Headings in an ALV
The column text may change upon the horizontal resizing of a column. There are three methods to accomplish this: 

| Method Name | Maximum Length of Heading |
| ------ | ------ |
| `set_short_text`   | 10   |
| `set_medium_text`   | 20   |
| `set_long_text`   | 40   |

The following example shows usage of all three. A `column` object is declared and instantiated as a reference to the result of `alv->get_columns( )->get_column( 'DISTID' )`. The column name *must* be in all capital letters. This is so that this method chaining is only called once in its instantiation, instead of being executed every time a column heading is changed.

    DATA column TYPE REF TO cl_salv_column.
    column = alv->get_columns( )->get_column( 'DISTID' ).

    column->set_short_text( 'Dist. Unit' ).
    column->set_medium_text( 'Unit of Distance' ).
    column->set_long_text( 'Mass Unit of Distance (kms, miles)' ).

## Enable ALV Toolbar Functionality
The following method call enables usage of many advanced features such as sorting, filtering, and exporting data.

    alv->get_functions( )->set_all( ).

## Enabling Every Other Row Striping in ALV
This method increases readability by giving consecutive rows alternating background color shading.

    alv->get_display_settings( )->set_striped_pattern( if_salv_c_bool_sap=>true ).

## Setting the Title of a Displayed ALV
By default, when an ALV is displayed, the title at the top is just the program name. This method allows the user to set a title of up to 70 characters. The following example shows how a dynamic title can be set that displays the number of records displayed.

    alv->get_display_settings( )->set_list_header( |Flight Schedule - { lines( t_spfli ) } records| ).

