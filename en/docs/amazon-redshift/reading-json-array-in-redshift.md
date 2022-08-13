---
title: "Reading JSON array in Redshift"
slug: "reading-json-array-in-redshift"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Currently, reading all elements in JSON array is not possible in Redshift. For e.g. if you want to read Manufacturer and model as columns from following JSON



<table class="tableizer-table">
<thead><tr class="tableizer-firstrow"><th>dim_id</th><th>number</th><th>info</th><th>manufacturer</th><th>model</th></tr></thead><tbody>
 <tr><td>200</td><td>1</td><td>Nissan~Sentra^Nissan~Maxima^Ford~Taurus^Ford~Escort^</td><td>Nissan</td><td>Sentra</td></tr>
 <tr><td>200</td><td>2</td><td>Nissan~Sentra^Nissan~Maxima^Ford~Taurus^Ford~Escort^</td><td>Nissan</td><td>Maxima</td></tr>
 <tr><td>200</td><td>3</td><td>Nissan~Sentra^Nissan~Maxima^Ford~Taurus^Ford~Escort^</td><td>Ford</td><td>Taurus</td></tr>
 <tr><td>200</td><td>4</td><td>Nissan~Sentra^Nissan~Maxima^Ford~Taurus^Ford~Escort^</td><td>Ford</td><td>Escort</td></tr>
</tbody></table>  

## Reading array elements in JSON

-- Create a sample JSON with ARRAY

create table car_sample(dim_id integer, info varchar(2000));
insert into car_sample values (200, '{"cars": [ { "Manufacturer": "Nissan", "Models": [{"Name":"Sentra", "doors":4}, {"Name":"Maxima", "doors":4} ]}, {"Manufacturer": "Ford",     "Models": [{"Name":"Taurus", "doors":4}, {"Name":"Escort", "doors":4} ]} ] }')

-- Create a supporting table for CROSS JOIN

create table series1_10 (number integer );<br>
insert into series1_10 values (1);<br>
insert into series1_10 values (2);<br>
insert into series1_10 values (3);<br>
insert into series1_10 values (4);<br>
insert into series1_10 values (5);<br>
insert into series1_10 values (6);<br>
insert into series1_10 values (7);<br>
insert into series1_10 values (8);<br>
insert into series1_10 values (9);<br>
insert into series1_10 values (10);<br>

-- UDF for extracting JSON array into one ^ delimited string<br>
CREATE OR REPLACE FUNCTION f_extractJson (jsonVar varchar) RETURNS varchar IMMUTABLE as $$<br>
&nbsp;def myfunc(myParm):<br>
&nbsp;&nbsp;import json<br>
&nbsp;&nbsp;cars=json.loads(jsonVar)<br>
&nbsp;&nbsp;parsedString=''<br>
&nbsp;&nbsp;for car in cars["cars"]:<br>
&nbsp;&nbsp;&nbsp;for model in car["Models"]:<br>
                &nbsp;&nbsp;&nbsp;&nbsp;parsedString=parsedString+car["Manufacturer"]+'~'+model["Name"]+'^'<br>
&nbsp;&nbsp;return parsedString<br>

&nbsp;return myfunc(jsonVar)<br>
$$ LANGUAGE plpythonu;<br>


-- Check the data

select dim_id, f_extractJson(info) from car_sample;

-- Pivot rows

WITH w1 AS (select dim_id, f_extractJson(info) info from car_sample)<br>
select dim_id,number, info, split_part(split_part(info,'^',number),'~', 1) <br>Manufacturer, split_part(split_part(info,'^',number),'~', 2) Model<br>
from w1 cross join series1_10 <br>
where number <= regexp_count(info,'[=^=]') ;<br>



