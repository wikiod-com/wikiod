---
title: "Versions"
slug: "versions"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Version Table
Every tag has an overview topic. This topic has a special section named "Versions". In that section, different versions can be defined in a table that has that should

 - have at least 2 columns: 
   
    1. the first one should be the name of the version
    2. the last one should be the release date of the version, by which versions will be ordered.

       It should be in the `yyyy-MM-dd` format and a valid date.

There can be additional columns between the two to include more information. It makes sense to list versions in chronological order.

This markup:

    | Version | Additional Information | Release date |
    | --- | ----| ----|
    | 1.R | version names do not have to be numbers | 1980-11-25 |
    | 1 | we have to start somewhere | 1980-11-24 |

produces this result:

| Version | Additional Information | Release date |
| --- | ----| ----|
| 1.R | version names do not have to be numbers | 1980-11-25 |
| 1 | we have to start somewhere | 1980-11-24 |

## Multiple Version Tables
For some tags it makes sense to have multiple version tables. There might be different subsets of a programming language or framework available, say for different device types as in this example.

The different tables should be prefaced with a heading.

This markup:

    ## desktop development kit ##
    
    | Version | Release date |
    | --- | ----|
    | 1.0 | 1980-12-24 |
    | 1 RC | 1980-11-25 |
    | 1 beta | 1980-11-24 |
    
    ## mobile development kit ##
    
    | Version  | Release date |
    | --- | ----|
    | M 2 | 1980-12-24 |
    | M 1.2 | 1980-11-25 |
    | M 1 | 1980-11-24 |

produces this result:

## desktop development kit ##

| Version | Release date |
| --- | ----|
| 1.0 | 1980-12-24 |
| 1 RC | 1980-11-25 |
| 1 beta | 1980-11-24 |

## mobile development kit ##

| Version  | Release date |
| --- | ----|
| M 2 | 1980-12-24 |
| M 1.2 | 1980-11-25 |
| M 1 | 1980-11-24 |

## Versions of a Topic
A topic might only apply to certain versions. When creating a new topic, the appropriate versions can be chosen in the title section of the topic.

In the following example .NET 2.0, 4.0, 4.5.1; Compact Framework 3.7 and Micro Framework 4.2 are selected.

[![choosing versions for topic][1]][1]

This selection will show up next to the topic title in the list of all topics


  [1]: http://i.stack.imgur.com/qJALj.png

## Versions of an Example (single version table)
An example cannot be associated with a version number directly.

Parts of it can however be declared to apply only for certain versions.

>    \<!-- if version [eq Java SE 1.3] -->
>
>    This content is for Java SE 1.3
>
>    \<!-- end version if -->

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/TzlUk.png

## Versions of an Example (multiple version tables)
If there are many version tables, the title of the desired one has to be included in angle brackets.

>  \<!-- if version \<Micro Framework> [eq 4.4] -->
>
>    This content is for Micro Framework 4.4
>
>  \<!-- end version if -->

[![micro framework versions][1]][1]


  [1]: http://i.stack.imgur.com/BjGFl.png

## Available Conditionals
The [markdown help](http://stackoverflow.com/editing-help#inline-versions) states

> Available conditionals are `gt`, `gte`, `lt`, `lte`, `eq`, and `neq`.

>  \<!-- if version [eq C++03] -->
>
>    `eq` equal to
>
>  \<!-- end version if -->
    
>  \<!-- if version [neq C++03] -->
>
>    `neq` not equal to
>
>  \<!-- end version if -->
    
>  \<!-- if version [gt C++03] -->
>
>    `gt` greater than
>
>  \<!-- end version if -->
    
>  \<!-- if version [gte C++03] -->
>
>    `gte` greater than equal
>
>  \<!-- end version if -->
    
>  \<!-- if version [lt C++03] -->
>
>    `lt` less than
>
>  \<!-- end version if -->
    
>  \<!-- if version [lte C++03] -->
>
>    `lte` less than equal
>
>  \<!-- end version if -->

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/lzASZ.png

## Multiple Conditionals
Multiple conditions can be used.

>  \<!-- if version [lte 1.2.0] [gt 1.3.12] [gt 1.4.9] [neq 1.2.0]  [eq 1.5.7] -->
>
>    content
>
>  \<!-- end version if -->

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/Uh842.png

