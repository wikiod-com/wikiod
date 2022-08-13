---
title: "Arrays"
slug: "arrays"
draft: false
images: []
weight: 9588
type: docs
toc: true
---

## Array Assignments
**List Assignment**

If you are familiar with Perl, C, or Java, you might think that Bash would use commas to separate array elements, however this is not the case; instead, Bash uses spaces:

<!-- language: lang-perl -->
     # Array in Perl
     my @array = (1, 2, 3, 4);

<!-- language: lang-sh -->
     # Array in Bash
     array=(1 2 3 4)

Create an array with new elements:

    array=('first element' 'second element' 'third element')

**Subscript Assignment**

Create an array with explicit element indices:

    array=([3]='fourth element' [4]='fifth element')

**Assignment by index**

    array[0]='first element'
    array[1]='second element'

**Assignment by name (associative array)**
<!-- if version [gte 4.0] -->
    declare -A array
    array[first]='First element'
    array[second]='Second element'
<!-- end version if -->

**Dynamic Assignment**

Create an array from the output of other command, for example use __seq__ to get a range from 1 to 10:

    array=(`seq 1 10`)

Assignment from script's input arguments:

    array=("$@")

Assignment within loops:

    while read -r; do
        #array+=("$REPLY")     # Array append
        array[$i]="$REPLY"     # Assignment by index
        let i++                # Increment index 
    done < <(seq 1 10)  # command substitution
    echo ${array[@]}    # output: 1 2 3 4 5 6 7 8 9 10

where `$REPLY` is always the current input


## Accessing Array Elements
Print element at index 0

    echo "${array[0]}"

<!-- if version [lt 4.3] -->
Print last element using substring expansion syntax

    echo "${arr[@]: -1 }"
<!-- end version if -->

<!-- if version [gte 4.3] -->

Print last element using subscript syntax

    echo "${array[-1]}"

<!-- end version if -->

Print all elements, each quoted separately

    echo "${array[@]}"

Print all elements as a single quoted string

    echo "${array[*]}"

Print all elements from index 1, each quoted separately

    echo "${array[@]:1}"

Print 3 elements from index 1, each quoted separately

    echo "${array[@]:1:3}"

**String Operations**

If referring to a single element, string operations are permitted:
    
    array=(zero one two)
    echo "${array[0]:0:3}" # gives out zer (chars at position 0, 1 and 2 in the string zero)
    echo "${array[0]:1:3}" # gives out ero (chars at position 1, 2 and 3 in the string zero)

so `${array[$i]:N:M}` gives out a string from the `N`th position (starting from 0) in the string `${array[$i]}` with `M` following chars.

## Array Modification
**Change Index**

Initialize or update a particular element in the array

    array[10]="elevenths element"    # because it's starting with 0

<!-- if version [gte 3.1] -->

**Append**

Modify array, adding elements to the end if no subscript is specified.

    array+=('fourth element' 'fifth element')

<!-- end version if -->

Replace the entire array with a new parameter list.

    array=("${array[@]}" "fourth element" "fifth element")

Add an element at the beginning:

    array=("new element" "${array[@]}")

**Insert**

Insert an element at a given index:

    arr=(a b c d)
    # insert an element at index 2
    i=2
    arr=("${arr[@]:0:$i}" 'new' "${arr[@]:$i}")
    echo "${arr[2]}" #output: new

**Delete**

Delete array indexes using the `unset` builtin:

    arr=(a b c)
    echo "${arr[@]}"   # outputs: a b c
    echo "${!arr[@]}"  # outputs: 0 1 2
    unset -v 'arr[1]'
    echo "${arr[@]}"   # outputs: a c
    echo "${!arr[@]}"  # outputs: 0 2

**Merge**

    array3=("${array1[@]}" "${array2[@]}")

This works for sparse arrays as well.

**Re-indexing an array**

This can be useful if elements have been removed from an array, or if you're unsure whether there are gaps in the array. To recreate the indices without gaps:

    array=("${array[@]}")

## Array Length
`${#array[@]}` gives the length of the array `${array[@]}`:
    
    array=('first element' 'second element' 'third element')
    echo "${#array[@]}" # gives out a length of 3

This works also with Strings in single elements:
    
    echo "${#array[0]}"    # gives out the lenght of the string at element 0: 13

## Array Iteration
Array iteration comes in two flavors, foreach and the classic for-loop:


    a=(1 2 3 4)
    # foreach loop
    for y in "${a[@]}"; do
        # act on $y
        echo "$y"
    done
    # classic for-loop
    for ((idx=0; idx < ${#a[@]}; ++idx)); do
        # act on ${a[$idx]}
        echo "${a[$idx]}"
    done

## You can also iterate over the output of a command:

    a=($(tr ',' ' ' <<<"a,b,c,d")) # tr can transform one character to another
    for y in "${a[@]}"; do
        echo "$y"
    done
       

## Associative Arrays
<!-- if version [gte 4.0] -->

**Declare an associative array**

    declare -A aa 

Declaring an associative array before initialization or use is mandatory.

**Initialize elements**

You can initialize elements one at a time as follows:

    aa[hello]=world
    aa[ab]=cd
    aa["key with space"]="hello world"

You can also initialize an entire associative array in a single statement:

    aa=([hello]=world [ab]=cd ["key with space"]="hello world")

**Access an associative array element**

    echo ${aa[hello]}
    # Out: world

**Listing associative array keys**

    echo "${!aa[@]}"
    #Out: hello ab key with space

**Listing associative array values**

    echo "${aa[@]}"
    #Out: world cd hello world

**Iterate over associative array keys and values**

    for key in "${!aa[@]}"; do
        echo "Key:   ${key}"
        echo "Value: ${array[$key]}"
    done

    # Out:
    # Key:   hello
    # Value: world
    # Key:   ab
    # Value: cd
    # Key:   key with space
    # Value: hello world

**Count associative array elements**

    echo "${#aa[@]}"
    # Out: 3

<!-- end version if -->

## Looping through an array
Our example array:

    arr=(a b c d e f)
    
Using a `for..in` loop:

    for i in "${arr[@]}"; do
        echo "$i"
    done

<!-- if version [gte 2.04] -->
    
Using C-style `for` loop:

    for ((i=0;i<${#arr[@]};i++)); do
        echo "${arr[$i]}" 
    done

<!-- end version if -->
    
Using `while` loop:

    i=0
    while [ $i -lt ${#arr[@]} ]; do
        echo "${arr[$i]}"
        i=$((i + 1))
    done

<!-- if version [gte 2.04] -->
    
Using `while` loop with numerical conditional:

    i=0
    while (( $i < ${#arr[@]} )); do
        echo "${arr[$i]}"
        ((i++))
    done

<!-- end version if -->

Using an `until` loop:

    i=0
    until [ $i -ge ${#arr[@]} ]; do
        echo "${arr[$i]}"
        i=$((i + 1))
    done

<!-- if version [gte 2.04] -->
    
Using an `until` loop with numerical conditional:

    i=0
    until (( $i >= ${#arr[@]} )); do
        echo "${arr[$i]}"
        ((i++))
    done

<!-- end version if -->

## Destroy, Delete, or Unset an Array
To destroy, delete, or unset an array:

    unset array

To destroy, delete, or unset a single array element:

    unset array[10]

## List of initialized indexes
Get the list of inialized indexes in an array
    
    $ arr[2]='second'
    $ arr[10]='tenth'
    $ arr[25]='twenty five'
    $ echo ${!arr[@]}
    2 10 25



## Array from string
```
stringVar="Apple Orange Banana Mango"
arrayVar=(${stringVar// / })
```
Each space in the string denotes a new item in the resulting array.
```
echo ${arrayVar[0]} # will print Apple
echo ${arrayVar[3]} # will print Mango
```
Similarly, other characters can be used for the delimiter.
```
stringVar="Apple+Orange+Banana+Mango"
arrayVar=(${stringVar//+/ })
echo ${arrayVar[0]} # will print Apple
echo ${arrayVar[2]} # will print Banana
```

## Reading an entire file into an array
Reading in a single step:
    
    IFS=$'\n' read -r -a arr < file

Reading in a loop:

    arr=()
    while IFS= read -r line; do
      arr+=("$line")
    done

<!-- if version [gte 4.0] -->
Using `mapfile` or `readarray` (which are synonymous):

    mapfile -t arr < file
    readarray -t arr < file
<!-- end version if -->    

## Array insert function
This function will insert an element into an array at a given index:

    insert(){
        h='
    ################## insert ########################
    # Usage:
    #   insert arr_name index element
    #
    #   Parameters:
    #       arr_name    : Name of the array variable
    #       index       : Index to insert at
    #       element     : Element to insert
    ##################################################
        '
        [[ $1 = -h ]] && { echo "$h" >/dev/stderr; return 1; }
        declare -n __arr__=$1   # reference to the array variable
        i=$2                    # index to insert at
        el="$3"                 # element to insert
        # handle errors
        [[ ! "$i" =~ ^[0-9]+$ ]] && { echo "E: insert: index must be a valid integer" >/dev/stderr; return 1; }
        (( $1 < 0 )) && { echo "E: insert: index can not be negative" >/dev/stderr; return 1; }
        # Now insert $el at $i
        __arr__=("${__arr__[@]:0:$i}" "$el" "${__arr__[@]:$i}")
    }

Usage:

    insert array_variable_name index element
Example:

    arr=(a b c d)
    echo "${arr[2]}" # output: c
    # Now call the insert function and pass the array variable name,
    # index to insert at
    # and the element to insert
    insert arr 2 'New Element'
    # 'New Element' was inserted at index 2 in arr, now print them
    echo "${arr[2]}" # output: New Element
    echo "${arr[3]}" # output: c


