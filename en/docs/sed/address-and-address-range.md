---
title: "Address and address range"
slug: "address-and-address-range"
draft: false
images: []
weight: 9869
type: docs
toc: true
---

Sed commands can be specified to act only on certain lines by using *addresses* or *address ranges*.

## Lines matching regular expression pattern
    $ cat ip.txt 
    address
    range
    substitution
    pattern
    sample
    Add Sub Mul Div


* Lines matching a pattern

      $ sed '/add/d' ip.txt 
      range
      substitution
      pattern
      sample
      Add Sub Mul Div
    
      $ sed -n '/t/p' ip.txt 
      substitution
      pattern
    
      $ sed -n '/[A-Z]/ s| |/|gp' ip.txt 
      Add/Sub/Mul/Div

* Range of patterns

      $ sed -n '/add/,/sub/p' ip.txt 
      address
      range
      substitution
    
      $ sed -n '/a/,/e/p' ip.txt 
      address
      range
      pattern
      sample

<br>

**Note**

* In the second example, it matched two ranges - lines `1,2` and lines `4,5`
* See [Using different delimiters](https://www.wikiod.com/sed/regular-expressions#Using different delimiters) on how to use other characters instead of `/` for specifying the pattern

<br>
<!-- if version [eq GNU sed] -->

* Case-insensitive match

      $ sed -n '/add/Ip' ip.txt 
      address
      Add Sub Mul Div
    
      $ sed -n '/add/I,/sub/p' ip.txt 
      address
      range
      substitution
      Add Sub Mul Div

<!-- end version if -->





## Specific line
    $ cat ip.txt
    address
    range
    substitution
    pattern
    sample

* *N*th line
        
      $ sed -n '2p' ip.txt 
      range
    
      $ sed '3d' ip.txt 
      address
      range
      pattern
      sample

* Last line

      $ sed -n '$p' ip.txt 
      sample



## Specific range of lines
    $ cat ip.txt 
    address
    range
    substitution
    pattern
    sample

* Range specified is inclusive of those line numbers

      $ sed -n '2,4p' ip.txt 
      range
      substitution
      pattern

* `$` can be used to specify last line. Space can be used between address and command for clarity

      $ sed -n '3,$ s/[aeiou]//gp' ip.txt 
      sbstttn
      pttrn
      smpl

<br>
<!-- if version [eq GNU sed] -->

* *i*th line to *i*+*j*th line

      $ sed '2,+2d' ip.txt 
      address
      sample

* *i*th line and *i*+*j*, *i*+2*j*, *i*+3*j*, etc.

      $ sed -n '1~2p' ip.txt 
      address
      substitution
      sample

<!-- end version if -->


## Specifying range using both number and pattern
    $ cat ip.txt 
    address
    range
    substitution
    pattern
    sample
    Add Sub Mul Div

* Line number to line matching pattern

      $ sed -n '2,/pat/p' ip.txt 
      range
      substitution
      pattern

* Line matching pattern to line number

      $ sed '/pat/,$d' ip.txt 
      address
      range
      substitution

<br>
<!-- if version [eq GNU sed] -->

* Line matching pattern plus number of lines following it

      $ sed -n '/add/I,+1p' ip.txt 
      address
      range
      Add Sub Mul Div

* `0` can be used as starting line number to signal end of range when pattern matches first line of input

      $ sed -n '0,/r/p' ip.txt 
      address

      $ sed -n '1,/r/p' ip.txt 
      address
      range

      $ sed -n '0,/u/p' ip.txt 
      address
      range
      substitution

<!-- end version if -->

## Negating address range
    $ cat ip.txt 
    address
    range
    substitution
    1234
    search pattern
    sample
    Add Sub Mul Div

* Deleting lines other than address specified

      $ sed '/[0-9]/!d' ip.txt 
      1234
       
      $ sed -n '/[0-9]/p' ip.txt 
      1234
        
      $ sed '$!d' ip.txt 
      Add Sub Mul Div
        
      $ sed -n '$p' ip.txt 
      Add Sub Mul Div

* Search and replace on lines not matching a pattern

      $ sed '/ /! s/^/#/' ip.txt 
      #address
      #range
      #substitution
      #1234
      search pattern
      #sample
      Add Sub Mul Div
    
      $ sed '/add/,/sub/! s/[aeiou]//gi' ip.txt 
      address
      range
      substitution
      1234
      srch pttrn
      smpl
      dd Sb Ml Dv



