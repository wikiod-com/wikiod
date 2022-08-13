---
title: "Arrays"
slug: "arrays"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

Arrays are specific data type, representing an ordered collection of elements of another type.

In Go, Arrays can be simple (sometime called "lists") or multi-dimensional (like for example a 2-dimentions arrays is representing a ordered collection of arrays, that contains elements)

## Syntax
 - var variableName [5]ArrayType // Declaring an array of size 5.
 - var variableName [2][3]ArrayType = { {Value1, Value2, Value3}, {Value4, Value5, Value6} }  // Declaring a multidimensional array
 - variableName := [...]ArrayType {Value1, Value2, Value3} // Declare an array of size 3 (The compiler will count the array elements to define the size)
 - arrayName[2]              // Getting the value by index.
 - arrayName[5] = 0          // Setting the value at index.
 - arrayName[0]              // First value of the Array
 - arrayName[ len(arrayName)-1 ] // Last value of the Array

## Creating arrays
An array in go is an ordered collection of same types elements.  
The basic notation to represent arrays is to use `[]` with the variable name.

Creating a new array looks like `var array = [size]Type`, replacing `size` by a number (for example `42` to specify it will be a list of 42 elements), and replacing `Type` by the type of the elements the array can contains (for example `int` or `string`)

Just below it's a code example showing the different way to create an array in Go.


    

    // Creating arrays of 6 elements of type int,
    // and put elements 1, 2, 3, 4, 5 and 6 inside it, in this exact order:
    var array1 [6]int = [6]int {1, 2, 3, 4, 5, 6} // classical way
    var array2 = [6]int {1, 2, 3, 4, 5, 6} // a less verbose way
    var array3 = [...]int {1, 2, 3, 4, 5, 6} // the compiler will count the array elements by itself

    fmt.Println("array1:", array1) // > [1 2 3 4 5 6]
    fmt.Println("array2:", array2) // > [1 2 3 4 5 6]
    fmt.Println("array3:", array3) // > [1 2 3 4 5 6]


    // Creating arrays with default values inside:
    zeros := [8]int{}               // Create a list of 8 int filled with 0
    ptrs := [8]*int{}               // a list of int pointers, filled with 8 nil references ( <nil> )
    emptystr := [8]string{}         // a list of string filled with 8 times ""

    fmt.Println("zeroes:", zeros)      // > [0 0 0 0 0 0 0 0]
    fmt.Println("ptrs:", ptrs)         // > [<nil> <nil> <nil> <nil> <nil> <nil> <nil> <nil>]
    fmt.Println("emptystr:", emptystr) // > [       ]  
    // values are empty strings, separated by spaces,
    // so we can just see separating spaces


    // Arrays are also working with a personalized type
    type Data struct {
        Number int
        Text   string
    }

    // Creating an array with 8 'Data' elements
    // All the 8 elements will be like {0, ""} (Number = 0, Text = "")
    structs := [8]Data{}  
    
    fmt.Println("structs:", structs) // > [{0 } {0 } {0 } {0 } {0 } {0 } {0 } {0 }] 
    // prints {0 } because Number are 0 and Text are empty; separated by a space

<kbd>[play it on playground](https://play.golang.org/p/7oPdlN8xt8)</kbd>

## Array Indexes
Arrays values should be accessed using a number specifying the location of the desired value in the array. This number is called Index.

Indexes starts at **0** and finish at **array length -1**.

To access a value, you have to do something like this: `arrayName[index]`, replacing "index" by the number corresponding to the rank of the value in your array.

For example:

    var array = [6]int {1, 2, 3, 4, 5, 6}

    fmt.Println(array[-42]) // invalid array index -1 (index must be non-negative)
    fmt.Println(array[-1]) // invalid array index -1 (index must be non-negative)
    fmt.Println(array[0]) // > 1
    fmt.Println(array[1]) // > 2
    fmt.Println(array[2]) // > 3
    fmt.Println(array[3]) // > 4
    fmt.Println(array[4]) // > 5
    fmt.Println(array[5]) // > 6
    fmt.Println(array[6]) // invalid array index 6 (out of bounds for 6-element array)
    fmt.Println(array[42]) // invalid array index 42 (out of bounds for 6-element array)

To set or modify a value in the array, the way is the same.  
Example:
 
    var array = [6]int {1, 2, 3, 4, 5, 6}

    fmt.Println(array) // > [1 2 3 4 5 6]

    array[0] := 6
    fmt.Println(array) // > [6 2 3 4 5 6]

    array[1] := 5
    fmt.Println(array) // > [6 5 3 4 5 6]

    array[2] := 4
    fmt.Println(array) // > [6 5 4 4 5 6]

    array[3] := 3
    fmt.Println(array) // > [6 5 4 3 5 6]

    array[4] := 2
    fmt.Println(array) // > [6 5 4 3 2 6]

    array[5] := 1
    fmt.Println(array) // > [6 5 4 3 2 1]

## Multidimensional Array
Multidimensional arrays are basically arrays containing others arrays as elements.  
It is represented like `[sizeDim1][sizeDim2]..[sizeLastDim]type`, replacing `sizeDim` by numbers corresponding to the length of the dimention, and `type` by the type of data in the multidimensional array.

For example, `[2][3]int` is representing an array composed of **2 sub arrays** of **3 int typed elements**.  
It can basically be the representation of a matrix of **2 lines** and **3 columns**. 

So we can make huge dimensions number array like `var values := [2017][12][31][24][60]int` for example if you need to store a number for each minutes since Year 0.

To access this kind of array, for the last example, searching for the value of 2016-01-31 at 19:42, you will access `values[2016][0][30][19][42]` (because **array indexes starts at 0** and not at 1 like days and months)

Some examples following:

    // Defining a 2d Array to represent a matrix like
    // 1 2 3     So with 2 lines and 3 columns;
    // 4 5 6     
    var multiDimArray := [2/*lines*/][3/*columns*/]int{ [3]int{1, 2, 3}, [3]int{4, 5, 6} }

    // That can be simplified like this:
    var simplified := [2][3]int{{1, 2, 3}, {4, 5, 6}}

    // What does it looks like ?
    fmt.Println(multiDimArray)
    // > [[1 2 3] [4 5 6]]

    fmt.Println(multiDimArray[0]) 
    // > [1 2 3]    (first line of the array)

    fmt.Println(multiDimArray[0][1])
    // > 2          (cell of line 0 (the first one), column 1 (the 2nd one))

<!-- break -->

    // We can also define array with as much dimensions as we need
    // here, initialized with all zeros
    var multiDimArray := [2][4][3][2]string{} 
  
    fmt.Println(multiDimArray);
    // Yeah, many dimensions stores many data
    // > [[[["" ""] ["" ""]] [["" ""] ["" ""]] [["" ""] ["" ""]]]
    //    [[["" ""] ["" ""]] [["" ""] ["" ""]] [["" ""] ["" ""]]]
    //    [[["" ""] ["" ""]] [["" ""] ["" ""]] [["" ""] ["" ""]]]
    //    [[["" ""] ["" ""]] [["" ""] ["" ""]] [["" ""] ["" ""]]]]
    //   [[[["" ""] ["" ""]] [["" ""] ["" ""]] [["" ""] ["" ""]]]
    //    [[["" ""] ["" ""]] [["" ""] ["" ""]] [["" ""] ["" ""]]]
    //    [[["" ""] ["" ""]] [["" ""] ["" ""]] [["" ""] ["" ""]]]
    //    [[["" ""] ["" ""]] [["" ""] ["" ""]] [["" ""] ["" ""]]]]

<!-- break -->
    // We can set some values in the array's cells
    multiDimArray[0][0][0][0] := "All zero indexes"  // Setting the first value
    multiDimArray[1][3][2][1] := "All indexes to max"  // Setting the value at extreme location

    fmt.Println(multiDimArray);
    // If we could see in 4 dimensions, maybe we could see the result as a simple format
        
    // > [[[["All zero indexes" ""] ["" ""]] [["" ""] ["" ""]] [["" ""] ["" ""]]]
    //    [[["" ""] ["" ""]] [["" ""] ["" ""]] [["" ""] ["" ""]]]
    //    [[["" ""] ["" ""]] [["" ""] ["" ""]] [["" ""] ["" ""]]]
    //    [[["" ""] ["" ""]] [["" ""] ["" ""]] [["" ""] ["" ""]]]]
    //   [[[["" ""] ["" ""]] [["" ""] ["" ""]] [["" ""] ["" ""]]]
    //    [[["" ""] ["" ""]] [["" ""] ["" ""]] [["" ""] ["" ""]]]
    //    [[["" ""] ["" ""]] [["" ""] ["" ""]] [["" ""] ["" ""]]]
    //    [[["" ""] ["" ""]] [["" ""] ["" ""]] [["" ""] ["" "All indexes to max"]]]]
    


