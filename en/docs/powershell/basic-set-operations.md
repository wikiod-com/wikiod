---
title: "Basic Set Operations"
slug: "basic-set-operations"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

A set is a collection of items which can be anything. Whatever operator we need to work on these sets are in short the *set operators* and the operation is  also known as *set operation*. Basic set operation includes Union, Intersection as well as addition, subtraction, etc.

## Syntax

- Group-Object

- Group-Object -Property \<propertyName\>

- Group-Object -Property \<propertyName\>, \<propertyName2\>

- Group-Object -Property \<propertyName\> -CaseSensitive

- Group-Object -Property \<propertyName\> -Culture \<culture\>

- Group-Object -Property \<ScriptBlock\>

- Sort-Object

- Sort-Object -Property \<propertyName\>

- Sort-Object -Property \<ScriptBlock\>

- Sort-Object -Property \<propertyName\>, \<propertyName2\>

- Sort-Object -Property \<propertyObject\> -CaseSensitive

- Sort-Object -Property \<propertyObject\> -Descending

- Sort-Object -Property \<propertyObject\> -Unique

- Sort-Object -Property \<propertyObject\> -Culture \<culture\>



## Filtering: Where-Object / where / ?
Filter an enumeration by using a conditional expression

Synonyms:  

    Where-Object
    where
    ?
Example:

    $names = @( "Aaron", "Albert", "Alphonse","Bernie", "Charlie", "Danny", "Ernie", "Frank")

    $names | Where-Object { $_ -like "A*" }
    $names | where { $_ -like "A*" }
    $names | ? { $_ -like "A*" }

Returns:  

> Aaron  
> Albert  
> Alphonse  

## Ordering: Sort-Object / sort
Sort an enumeration in either ascending or descending order

Synonyms:  

    Sort-Object
    sort

Assuming:

    $names = @( "Aaron", "Aaron", "Bernie", "Charlie", "Danny" )

Ascending sort is the default:

    $names | Sort-Object
    $names | sort
> Aaron  
> Aaron  
> Bernie  
> Charlie  
> Danny

To request descending order:

    $names | Sort-Object -Descending
    $names | sort -Descending
> Danny  
> Charlie  
> Bernie  
> Aaron  
> Aaron  

You can sort using an expression.

    $names | Sort-Object { $_.length }

>Aaron  
>Aaron  
>Danny  
>Bernie  
>Charlie  

## Grouping: Group-Object / group
You can group an enumeration based on an expression.

Synonyms:  

    Group-Object
    group

Examples:

    $names = @( "Aaron", "Albert", "Alphonse","Bernie", "Charlie", "Danny", "Ernie", "Frank")

    $names | Group-Object -Property Length
    $names | group -Property Length

Response:  

|Count|Name|Group                         |
|-----|----|------------------------------|                                                                                                                       
| 4   | 5  | {Aaron, Danny, Ernie, Frank} |
| 2   | 6  | {Albert, Bernie}             |
| 1   | 8  | {Alphonse}                   |
| 1   | 7  | {Charlie}                    |
 

## Projecting: Select-Object / select
Projecting an enumeration allows you to extract specific members of each object, to extract all the details, or to compute values for each object

Synonyms:  

    Select-Object
    select

Selecting a subset of the properties:

    $dir = dir "C:\MyFolder"

    $dir | Select-Object Name, FullName, Attributes
    $dir | select Name, FullName, Attributes

| Name | FullName | Attributes |
| ---- | -------- | ------------- |
| Images   | C:\MyFolder\Images   | Directory |
| data.txt | C:\MyFolder\data.txt | Archive |
| source.c | C:\MyFolder\source.c | Archive |

Selecting the first element, and show all its properties:

    $d | select -first 1 *

|   |   |   |
|---|---|---|
|PSPath            |:| Microsoft.PowerShell.Core\FileSystem::C:\MyFolder\Images  |
|PSParentPath      |:| Microsoft.PowerShell.Core\FileSystem::C:\MyFolder  |
|PSChildName       |:| Images  |
|PSDrive           |:| C  |
|PSProvider        |:| Microsoft.PowerShell.Core\FileSystem  |
|PSIsContainer     |:| True  |
|BaseName          |:| Images  |
|Mode              |:| d----  |
|Name              |:| Images  |
|Parent            |:| MyFolder  |
|Exists            |:| True  |
|Root              |:| C:\  |
|FullName          |:| C:\MyFolder\Images  |
|Extension         |:|  |
|CreationTime      |:| 4/26/2016 9:57:48 AM  |
|CreationTimeUtc   |:| 4/26/2016 1:57:48 PM  |
|LastAccessTime    |:| 4/26/2016 9:57:48 AM  |
|LastAccessTimeUtc |:| 4/26/2016 1:57:48 PM  |
|LastWriteTime     |:| 4/26/2016 9:57:48 AM  |
|LastWriteTimeUtc  |:| 4/26/2016 1:57:48 PM  |
|Attributes        |:| Directory  |


