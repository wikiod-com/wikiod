---
title: "SAS Formats"
slug: "sas-formats"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

Informats and formats are used to tell SAS how to read and write the data respectively. Informats are commonly used in a datastep when reading data from an external file. Informats are rarely used in PROCs. Formats are commonly used in both data steps and PROCs. 


SAS Formats convert either numeric or character values to character values. A format can either be applied using a `format` or `put` statement, which changes the way a value is displayed, or using the `put` function to store the formatted value in a new variable.


----------


There are four categories of formats :

 - Character - instructs SAS to write character data values from
   character variables. 
 - Date and Time - instructs SAS to write data
   values from variables that represent dates, times, and datetimes. 
 - ISO 8601 - instructs SAS to write date, time, and datetime values using
   the ISO 8601 standard. 
 - Numeric - instructs SAS to write numeric data values from numeric variables.


----------

Formats usually take the form `<formatname><w>.<d>;`, `w` being the width (including any decimals and the point), `d` being the number of decimal places.

----------

Common date formats (applied to SAS date values) :
- `date9.` e.g. 02AUG2016
- `ddmmyyn8.` e.g. 02082016
- `ddmmyy8.` e.g. 02/08/16
- `yymmdd10.` e.g. 20160802
- `year4.` e.g. 2016

Common numeric formats (applied to numbers) :
- `comma11.0` e.g. 1,234,567
- `comma12.2` e.g. 1,234,567.00
- `dollar11.2` e.g. $5,789.12
- `nlmnlgbp11.2` e.g. £2,468.02

Other formats :
- `$hex8.`, convert string to hex
- `$upcase.`, convert string to upper-case
- `$quote.`, enclose a string in quotes

A full list of formats can be found here > https://support.sas.com/documentation/cdl/en/lrdict/64316/HTML/default/viewer.htm#a001263753.htm

## Using the format statement
The `format` statement applies the given format to the specified variable *for display purposes only*, i.e. the underlying value does not change.

<pre>
data example1 ;
  Date  = '02AUG2016'd ; /* stored as a SAS date, i.e. a number */
  Date2 = '31AUG2016'd ;
  format Date monyy7. Date2 yymmddn8. ;
run ;
</pre>
| Date | Date2 |
| ------ | ----- |
| AUG2016| 20160831 |




## Using the format statement to group data
You can apply formats within a procedure, e.g. to change the groupings within a `proc summary` or `proc freq`.


----------
Grouping SAS dates

<pre>
data example2 ;
  do Date = '01JUN2016'dt to '31AUG2016'dt ;
    Days = 1 ;
    output ;
  end ;
run ;

/* Summarise by year & month */
proc summary data=example2 nway ;
  class Date ;
  var Days ;
  output out=example2_sum (drop=_TYPE_ _FREQ_) sum= ;
  format Date yymmn6. ; /* e.g. 201606 */
run ;
</pre>
| Date | Days |
| ----- | ----- |
| 201606 | 30 |
| 201607 | 31 |
| 201608 | 31 |

<pre>
/* Summarise by month & year */
proc summary data=example2 nway ;
  class Date ;
  var Days ;
  output out=example2_sum2 (drop=_TYPE_ _FREQ_) sum= ;
  format Date monyy7. ; /* e.g. JUN2016 */
run ;
</pre>
| Date | Days |
| ----- | ----- |
| JUN2016 | 30 |
| JUL2016 | 31 |
| AUG2016 | 31 |

The benefit of using a format is that the natural sort order is retained.

----------


Using `sashelp.class` as an example, say you wanted to compare the frequency of the first letter of each name. You could use the `substr()` function to find the first letter, and run a `proc freq` on the new variable. Alternatively, you can apply the `$1.` format to the `Name` variable :
<pre>
proc freq data=sashelp.class ;
  table Name ;
  format Name $1. ;
run ;
</pre>
| Name | COUNT |
| ----- | ----- |
| A | 7 |
| B | 4 |
| C | 2 |
| etc. | |


## Custom Formats
Custom formats, also known as user defined formats, can be created and used like any other default formats.

    /*Create new character format for state variables*/
    PROC FORMAT;
    VALUE $statef      'CA' = 'California'
                       'MA' = 'Massachusetts'
                       'NY' = 'New York';

    /*Once created, you can use your custom format in PROC and DATA steps*/
    PROC PRINT DATA=table;
    FORMAT state-var $statef.;
    RUN;

The variable `state-var` will be printed according to the new format. For example, the value `'CA'` will be printed as `'California'`. If a value was not formatted, such as `'CT'`, then that value will be printed as it appears in the data set.
    
     

## Using informats to read data
Informats are used to tell SAS how to read in the data and are identified with an `informat` statement. 

    data test;
     infile test.csv;
     informat     id $6.
                  date mmddyy10.
                  cost comma10.2
     ;
     input @1 id
           @7 date
           @20 cost
     ;
    run; 

Informats and Formats can also be used together to read in the data and write it out in a different format such as with the salary variable below:

    DATA workers;
      informat first last $16.;
      informat salary 12.1;
      informat birthdate 8.;
      input 
        first $ 
        last $ 
        birthdate 
        salary;
      format salary dollar10.;
    datalines;
    John Smith 19810505 54998.5
    Jane Doe 19950925 45884.5
    Frank James 19600222 70000.5
    Jamie Love 19630530 292000.5
    ;
    run;

