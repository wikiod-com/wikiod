---
title: "CFLOOP How-To"
slug: "cfloop-how-to"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

Big thanks to 

 - Pete Freitag for his [CFScript Cheat Sheet][1]
 - Adam Cameron for [CF 11: CFLOOP in CFScript is Very Broken][2] (and it still is in CF 2016).


  [1]: http://www.petefreitag.com/cheatsheets/coldfusion/cfscript/
  [2]: http://blog.adamcameron.me/2014/05/coldfusion-11-cfloop-in-cfscript-very.html

## Array
> The ability to directly use an `array` object with `cfloop` was added in ColdFusion 8.

Consider this array;

```
<cfset aFoo = [
    "one"
    , "two"
    , "three"
    , "four"
] />
```

<h1>Tag syntax</h1>

<h2>ColdFusion 8 through current</h2>

Using the attribute `index` by itself.

<h3>Parameters</h3>

| Attribute | Required |Default|Description|
| ------ | ------ |----|----|
| array | true | | An array object. The variable must be evaluated (wrapped with ##) |
| index | true | | The current element of the array.
```
<cfoutput>
    <cfloop array="#aFoo#" index="x">
        <li>#x#</li>
    </cfloop>
</cfoutput>
```
<h2>Generated HTML</h2>

This will also have a line break between each line of HTML. 
```
<li>one</li>
<li>two</li>
<li>three</li>
<li>four</li>
```
<h2>ColdFusion 2016 through current</h2>

> The attribute `item` changes the behavior of `cfloop` as of Coldfusion 2016.

Using the attribute `item` instead of or in addition to `index`.

<h3>Parameters</h3>

| Attribute | Required |Default|Description|
| ------ | ------ |----|----|
| array | true | | An array object. The variable must be evaluated (wrapped with ##) |
| item | true | | The current element of the array.
| index | false | | The current index of the array.
```
<cfoutput>
    <cfloop array="#aFoo#" item="x" index="y">
        <li>#x# | #y#</li>
    </cfloop>
</cfoutput>
```

<h2>Generated HTML</h2>

This will also have a line break between each line of HTML. 
```
<li>one | 1</li>
<li>two | 2</li>
<li>three | 3</li>
<li>four | 4</li>
```

<h1>CFScript</h1>

<h2>Previous to ColdFusion 8</h2>

```
<cfscript>
for (i = 1; x LTE arrayLen(aFoo); i = i + 1) {
    writeOutput("<li>" & aFoo[i] & "</li>");
}
</cfscript>
```

<h2>ColdFusion 8 through current</h2>

```
<cfscript>
for (i = 1; i <= arrayLen(aFoo); i = i++) {
    writeOutput("<li>" & aFoo[i] & "</li>");
}
</cfscript>
```

<h2>ColdFusion 9 through current</h2>

> With the `FOR IN` syntax, x is the current array element, not the array index.
```
<cfscript>
for (x in aFoo) {
    writeOutput("<li>" & x & "</li>");
}
</cfscript>
```
<h2>ColdFusion 11 through current</h2>
> The cfscript function `cfloop` has no support for `array`.

<h2>Generated HTML</h2>

Notice that the `cfscript` output is all on one line.

```
<li>one</li><li>two</li><li>three</li><li>four</li>
```

## List Loop


## Query
Consider the table `dbo.state_zip`, which contains the columns `city`, `statecode` and `zipcode` and has over 80,000 records.

<h1>Parameters</h1>

| Attribute | Required |Type|Default|Description|
| ------ | ------ |----|----|----|
| query | true |string| | The variable name of a query object. |
| startrow| false | numeric | | The starting row index of the query object.|
| endrow| false | numeric | | The ending row index of the query object.|
| group| false | string | | The query column name on which to group records. |

Example query
------------

```
<cfquery name="geo" datasource="reotrans-dev">
    SELECT city, stateCode, zipCode
    FROM dbo.state_zip
</cfquery>
```

<h1>Tag syntax</h1>

Using the query object `geo` as the source for `cfloop`. Since the table `dbo.state_zip` has so many records, the HTML generated will take quite some time. This example shows only the first 20 records' worth of HTML.

```
<cfoutput>
    <ul>
    <cfloop query="geo">
        <!--- Scope the column names with the query name. --->
        <li>#geo.city# | #geo.stateCode# | #geo.zipCode#</li>
    </cfloop>
    </ul>
</cfoutput>
```
<h2>Generated HTML</h2>
```
<ul>
    <li>100 PALMS | CA | 92274</li>
    <li>1000 PALMS | CA | 92276</li>
    <li>12 MILE | IN | 46988</li>
    <li>1ST NATIONAL BANK OF OMAHA | NE | 68197</li>
    <li>29 PALMS | CA | 92277</li>
    <li>29 PALMS | CA | 92278</li>
    <li>3 STATE FARM PLAZA | IL | 61710</li>
    <li>3 STATE FARM PLAZA | IL | 61791</li>
    <li>30TH STREET | PA | 19104</li>
    <li>3M CORP | MN | 55144</li>
    <li>65TH INFANTRY | PR | 00923</li>
    <li>65TH INFANTRY | PR | 00924</li>
    <li>65TH INFANTRY | PR | 00929</li>
    <li>65TH INFANTRY | PR | 00936</li>
    <li>7 CORNERS | VA | 22044</li>
    <li>88 | KY | 42130</li>
    <li>9 MILE POINT | LA | 70094</li>
    <li>A A R P INS | PA | 19187</li>    
    <li>A A R P PHARMACY | CT | 06167</li>
    <li>A H MCCOY FEDERAL BLDG | MS | 39269</li>
</ul>
```

<h2>Limiting output to specific rows</h2>

To limit the query's output to a specific range of rows, specify `startrow` and `endrow`.
```
<cfloop query="geo" startrow="100" endrow="150">
    <li>#geo.city# | #geo.stateCode# | #geo.zipCode#</li>
</cfloop>
```

<h2>Grouping Output</h2>

In the example data, the same state listed multiple times in relation to the multiple cities that are associated to each state. You can also see the same city listed multiple times in relation to the multiple zip codes associated to each city. 

Let's group the output by state first. Notice the 2nd instance of `cfloop` wrapped around the content that will be output under the `stateCode` grouped content.

```
<cfoutput>
    <ul>
    <cfloop query="geo" group="stateCode">
        <!--- Scope the column names with the query name. --->
        <li>#geo.stateCode#
            <ul>
                <cfloop>
                    <li>#geo.city# | #geo.zipCode#</li>
                </cfloop>
            </ul>
        </li>
    </cfloop>
    </ul>
</cfoutput>
```
Generated HTML (extract) from one grouped `cfloop` tag.
```
<ul>
    <li>AK
        <ul>
            <li>KONGIGANAK | 99545</li>
            <li>ADAK | 99546</li>
            <li>ATKA | 99547</li>
            <!-- etc. -->
        </ul>
    </li>
    <li>AL
        <ul>
            <li>ALEX CITY | 35010</li>
            <li>ALEXANDER CITY | 35010</li>
            <li>ALEX CITY | 35011</li>
            <!-- etc. -->
        </ul>
    </li>
    <!-- etc. -->
</ul>
```

Finally, let's group the output by `stateCode`, then by `city` in order to see all the `zipCode` entries per city. Notice the 2nd `cfloop` is now grouped by `city` and a 3rd `cfloop` exists to output the `zipCode` data.

```
<cfoutput>
    <ul>
    <cfloop query="geo" group="stateCode">
        <li>#geo.stateCode#
            <ul>
            <cfloop group="city">
                <li>#geo.city#
                    <ul>
                        <cfloop>
                            <li>#geo.zipCode#</li>
                        </cfloop>
                    </ul>
                </li>
            </cfloop>
            </ul>
        </li>
    </cfloop>
    </ul>
</cfoutput>
```
Generated HTML (extract) from two grouped `cfloop` tags.
```
<ul>
    <li>AK
        <ul>
            <li>ADAK
                <ul>
                    <li>99546</li>
                    <li>99571</li>
                </ul>
            </li>
            <li>AKHIOK
                <ul>
                    <li>99615</li>
                </ul>
            </li>
            <!--- etc. --->
            <li>BARROW
                <ul>
                    <li>99723</li>
                    <li>99759</li>
                    <li>99789</li>
                    <li>99791</li>
                </ul>
            </li>
            <!--- etc. --->
        </ul>
    </li>
    <!--- stateCodes etc. --->
</ul>
```

<h1>CFScript</h1>

<h2>ColdFusion 6 (MX) though current</h2>
```
<cfscript>
    for (x = 1; x LTE geo.recordcount; x = x + 1) {
        writeOutput( '<li>' & geo.city[x] & ' | ' & 
            geo.stateCode[x] & ' | ' & geo.zipCode[x] & '</li>');
    }
</cfscript>
```

<h2>ColdFusion 8 though current</h2>
```
<cfscript>
    for (x = 1; x <= geo.recordcount; x++) {
        writeOutput( '<li>' & geo.city[x] & ' | ' & 
            geo.stateCode[x] & ' | ' & geo.zipCode[x] & '</li>');
    }
</cfscript>
```
<h2>ColdFusion 10 though current</h2>
> With the `FOR IN` syntax, `x` is a query row object, not the row index.

```
<cfscript>
    for (x in geo) {
        writeOutput( '<li>' & x.city & ' | ' & 
            x.stateCode & ' | ' & x.zipCode & '</li>');
    }
</cfscript>
```
<h2>ColdFusion 11 though current</h2>
> ColdFusion 11 allows most tags to be written as cfscript.
```
<cfscript>
    cfloop(query: geo, startrow: 1, endrow: 2) {
        writeOutput( '<li>' & geo.city & ' | ' & 
            geo.stateCode & ' | ' & geo.zipCode & '</li>');
    }
</cfscript>
```
With `group`.
```
<cfscript>
    cfloop(query: geo, group: 'city') {
        writeOutput( '<li>' & geo.city & '<ul>');
        cfloop() { // no arguments, just as in the tag syntax.
            writeOutput('<li>'  & geo.zipCode & '</li>');
        }
        writeOutput('</ul></li>');
    }
</cfscript>
```

## Structure
Consider this structure:

```
<cfset stFoo = {
    a = "one"
    , b = "two"
    , c = "three"
    , d = "foue"
} />
```

<h1>Tag syntax</h1>

<h2>Parameters</h2>

> Notice the use of the attribute `item` instead of `index`.

| Attribute | Required |Type|Default|Description|
| ------ | ------ |----|----|----|
| collection | true | structure | | A `struct` object. The variable must be evaluated (wrapped with ##). |
| item | true | string | | The current structure `key`,|

<h2>Using Structure Functions</h2>

```
<cfoutput>
    <cfloop collection="#stFoo#" item="x">
        <li>#structFind(stFoo, x)#</li>
    </cfloop>
</cfoutput>
```

<h2>Implicit Structure Syntax</h2>

```
<cfoutput>
    <cfloop collection="#stFoo#" item="x">
        <li>#stFoo[x]#</li>
    </cfloop>
</cfoutput>
```

<h2>Generated HTML</h2>

This will also have a line break between each line of HTML.
```
<li>one</li>
<li>two</li>
<li>three</li>
<li>four</li>
```

<h1>CFScript</h1>

> With the `FOR IN` syntax, `x` is a `key` of the structure object.

<h2>Output the structure's keys</h2>

```
<cfscript>
    for (x in stFoo) {
        writeOutput("<li>" & x & "</li>");
    }
</cfscript>
```
<h2>Generated HTML</h2>
```
<li>A</li><li>B</li><li>C</li><li>D</li>
```

<h2>Output the value of the structure's keys</h2>

<h3>Using Structure Functions</h3>

```
<cfscript>
    for (x in stFoo) {
        writeOutput("<li>" & structFind(stFoo, x) & "</li>");
    }
</cfscript>
```

<h3>Implicit Structure Syntax</h3>

```
<cfscript>
    for (x in stFoo) {
        writeOutput("<li>" & stFoo[x] & "</li>");
    }
</cfscript>
```

<h2>ColdFusion 11 through current</h2>

> The cfscript function `cfloop` has no support for `collection`.

<h2>Generated HTML</h2>

Notice that the cfscript output is all on one line.
```
<li>one</li><li>two</li><li>three</li><li>four</li>
```

## File Loop


## Looping through a collection using CFML tags.


## Looping through a collection using CFSCRIPT.


## Index
<h1>Parameters</h1>

| Attribute | Required |Type|Default|Description|
| ------ | ------ |----|----|----|
| index | true | string | | Variable name for the loop's index. Defaults to the `variables` scope. |
| from | true | numeric | | Starting value for the index. |
| to | true | numeric | | Ending value for the index. |
| step | false | numeric | 1 | Value by which to increase or decrease the index per iteration. |

<h1>Basic index loop</h1>

> Final value of `x` is 10.

```
<!--- Tags --->
<cfoutput>
    <cfloop index="x" from="1" to="10">
        <li>#x#</li>
    </cfloop>
</cfoutput>
<!--- cfscript --->
<cfscript>
    for (x = 1; x <= 10; x++) {
        writeOutput('<li>' & x & '</li>');
    }
</cfscript>
<!--- HTML Output --->
 - 1
 - 2
 - 3
 - 4
 - 5
 - 6
 - 7
 - 8
 - 9
 - 10
```

<h2>Increase step to 2</h2>

> Final value of `x` is 11.

```
<!--- Tags --->
<cfoutput>
    <cfloop index="x" from="1" to="10" step="2">
        <li>#x#</li>
    </cfloop>
</cfoutput>
<!--- cfscript --->
<cfscript>
    for (x = 1; x <= 10; x += 2) {
        writeOutput('<li>' & x & '</li>');
    }
</cfscript>
<!--- HTML Output --->
 - 1
 - 3
 - 5
 - 7
 - 9
```

<h2>Decrement step by 1</h2>

> Final value of `x` is 0.

```
<!--- Tags --->
<cfoutput>
    <cfloop index="x" from="10" to="1" step="-1">
        <li>#x#</li>
    </cfloop>
</cfoutput>
<!--- cfscript --->
<cfscript>
    for (x = 10; x > 0; x--) {
        writeOutput('<li>' & x & '</li>');
    }
</cfscript>
<!--- HTML Output --->
 - 10
 - 9
 - 8
 - 7
 - 6
 - 5
 - 4
 - 3
 - 2
 - 1
```

<h1>CFLoop in a Function</h1>

> Make sure to `var` or `local` scope the index inside a function. `Foo()` returns 11.

```
<!--- var scope --->
<cffunction name="foo" access="public" output="false" returntype="numeric">
    <cfset var x = 0 />
    <cfloop index="x" from="1" to="10" step="1">
        <cfset x++ />
    </cfloop>
    <cfreturn x />
</cffunction>

<!--- Local scope --->
<cffunction name="foo" access="public" output="false" returntype="numeric">
    <cfloop index="local.x" from="1" to="10" step="1">
        <cfset local.x++ />
    </cfloop>
    <cfreturn local.x />
</cffunction>
```

<h2>ColdFusion 11 through current</h2>

> The cfscript function `cfloop` has no support for `index` as a stand alone counter mechanism.

## Condition
<h1>Tag syntax</h1>

<h2>Parameters</h2>

| Attribute | Required |Type|Default|Description|
| ------ | ------ |----|----|----|
| condition | true |string | | Condition that manages the loop. Cannot contain math symbols like `<`, `>` or `=`. Must use ColdFusion text implementations like `less than`, `lt`, `greater than`, `gt`, `equals` or `eq`.|

Final value of `x` is 5.
```
<cfset x = 0 />
<cfoutput>
    <cfloop condition="x LT 5">
        <cfset x++ />
        <li>#x#</li>
    </cfloop>
</cfoutput>
```

<h2>Generated HTML</h2>

This will also have a line break between each line of HTML.
```
<li>1</li>
<li>2</li>
<li>3</li>
<li>4</li>
<li>5</li>
```

<h1>CFScript</h1>

<h2>Previous to ColdFusion 8</h2>

```
<cfscript>
x = 0;
while (x LT 5) {
    x = x + 1;
    writeOutput('<li>' & x & '</li>');
}
</cfscript>
```

<h2>ColdFusion 8 through current</h2>

```
<cfscript>
x = 0;
while (x LT 5) {
    x = x++;
    writeOutput('<li>' & x & '</li>');
}
</cfscript>
```

<h2>ColdFusion 11 through current</h2>

> The cfscript function `cfloop` has no support for `condition`.

<h2>Generated HTML</h2>

Notice that the cfscript output is all on one line.
```
<li>one</li><li>two</li><li>three</li><li>four</li>
```

## Date or time range
Example for date or time range.

## List
Consider this list: 
```
<cfset foo = "one,two,three,four" />
```

<h1>Tag syntax</h1>

<h2>Parameters</h2>

| Attribute | Required |Default|Description|
| ------ | ------ |----|----|
| list | true | | A list object. The variable must be evaluated (wrapped with ##) |
| index | true | | The current element of the list. |
```
<cfoutput>
    <cfloop list="#foo#" index="x">
        <li>#x#</li>
    </cfloop>
</cfoutput>
```

<h2>Generated HTML</h2>

This will also have a line break between each line of HTML.
```
<li>one</li>
<li>two</li>
<li>three</li>
<li>four</li>
```

<h1>CFScript</h1>

<h2>Previous to ColdFusion 8</h2>

```
<cfscript>
    for (x = 1; x LTE listLen(foo); x = x + 1) {
        writeOutput("<li>" & listGetAt(foo, x) & "</li>");
    }
</cfscript>
```

<h2>ColdFusion 8 through current</h2>

```
<cfscript>
    for (x = 1; x <= listLen(foo); x++) {
        writeOutput("<li>" & listGetAt(foo, x) & "</li>");
    }
</cfscript>
```

<h2>ColdFusion 9 through current</h2>

```
<cfscript>
    for (x in foo) {
        writeOutput("<li>" & x & "</li>");
    }
</cfscript>
```

<h2>ColdFusion 11 through current</h2>

> The cfscript function `cfloop` has no support for `list`.

<h2>Generated HTML</h2>

Notice that the cfscript output is all on one line.
```
<li>one</li><li>two</li><li>three</li><li>four</li>
```

## Index Loop


## Query Loop


## File
    <cfloop list="#myFile#" index="FileItem" delimiters="#chr(10)##chr(13)#">
      <cfoutput>
       #FileItem#<br />
     </cfoutput>
    </cfloop>

## Conditional Loop


## COM Collection/Structure Loops


