---
title: "Arrays"
slug: "arrays"
draft: false
images: []
weight: 9673
type: docs
toc: true
---

## Syntax
 - **Declaring an array:** 

    &lt;type>[] &lt;name>;

 - **Declaring two-dimensional array:**

    &lt;type>[,] &lt;name> = new &lt;type>[&lt;value>, &lt;value>];

 - **Declaring a Jagged Array:**

    &lt;type>[][] &lt;name> = new &lt;type>[&lt;value>][];

- **Declaring a subarray for a Jagged Array:**

    &lt;name>[&lt;value>]  = new &lt;type>[&lt;value>];

 - **Initializing an array without values:** 

    &lt;name> = new &lt;type>[&lt;length>];

 - **Initializing an array with values:**

    &lt;name> = new &lt;type>[] {&lt;value>, &lt;value>, &lt;value>, ...};

- **Initializing a two-dimensional array with values:**

    &lt;name> = new &lt;type>[,] { {&lt;value>, &lt;value>}, {&lt;value>, &lt;value>}, ...};

 - **Accessing an element at index i:**

    &lt;name>[i]

 - **Getting the array's length:**

    &lt;name>.Length



In C#, an array is a reference type, which means it is *nullable*.

An array has a fixed length, which means you cant `.Add()` to it or `.Remove()` from it. In order to use these, you would need a dynamic array - `List` or `ArrayList`.

## Declaring an array
An array can be declared and filled with the default value using square bracket (`[]`) initialization syntax. For example, creating an array of 10 integers:

    int[] arr = new int[10];

Indices in C# are zero-based. The indices of the array above will be 0-9. For example:

    int[] arr = new int[3] {7,9,4};
    Console.WriteLine(arr[0]); //outputs 7
    Console.WriteLine(arr[1]); //outputs 9

Which means the system starts counting the element index from 0. Moreover, accesses to elements of arrays are done in **constant time**. That means accessing to the first element of the array has the same cost (in time) of accessing the second element, the third element and so on.

You may also declare a bare reference to an array without instantiating an array.

    int[] arr = null;   // OK, declares a null reference to an array.
    int first = arr[0]; // Throws System.NullReferenceException because there is no actual array.

An array can also be created and initialized with custom values using collection initialization syntax:

    int[] arr = new int[] { 24, 2, 13, 47, 45 };

The `new int[]` portion can be omitted when declaring an array variable. This is not a self-contained _expression_, so using it as part of a different call does not work (for that, use the version with `new`):

    int[] arr = { 24, 2, 13, 47, 45 };  // OK
    int[] arr1;
    arr1 = { 24, 2, 13, 47, 45 };       // Won't compile

**Implicitly typed arrays**

Alternatively, in combination with the `var` keyword, the specific type may be omitted so that the type of the array is inferred:

    // same as int[]
    var arr = new [] { 1, 2, 3 };
    // same as string[]
    var arr = new [] { "one", "two", "three" };
    // same as double[]
    var arr = new [] { 1.0, 2.0, 3.0 };


## Initializing an array filled with a repeated non-default value
As we know we can declare an array with default values:

    int[] arr = new int[10];

This will create an array of 10 integers with each element of the array having value `0` (the default value of type `int`).

To create an array initialized with a non-default value, we can use [`Enumerable.Repeat`][1] from the [`System.Linq`][2] Namespace:

 1. To create a `bool` array of size 10 filled with **"true"**

        bool[] booleanArray = Enumerable.Repeat(true, 10).ToArray(); 

 2. To create an `int` array of size 5 filled with **"100"**

        int[] intArray = Enumerable.Repeat(100, 5).ToArray();

 3. To create a `string` array of size 5 filled with **"C#"**

        string[] strArray = Enumerable.Repeat("C#", 5).ToArray();

  [1]: https://msdn.microsoft.com/en-us/library/bb348899(v=vs.100).aspx
  [2]: https://msdn.microsoft.com/en-us/library/system.linq%28v=vs.100%29.aspx

## Copying arrays
Copying a partial array with the static `Array.Copy()` method, beginning at index 0 in both, source and destination:

    var sourceArray = new int[] { 11, 12, 3, 5, 2, 9, 28, 17 };
    var destinationArray= new int[3];
    Array.Copy(sourceArray, destinationArray, 3);

    // destinationArray will have 11,12 and 3

Copying the whole array with the `CopyTo()` instance method, beginning at index 0 of the source and the specified index in the destination:

    var sourceArray = new int[] { 11, 12, 7 };
    var destinationArray = new int[6];
    sourceArray.CopyTo(destinationArray, 2);

    // destinationArray will have 0, 0, 11, 12, 7 and 0

`Clone` is used to create a copy of an array object.

    var sourceArray = new int[] { 11, 12, 7 };
    var destinationArray = (int)sourceArray.Clone();

    //destinationArray will be created and will have 11,12,17.

Both `CopyTo` and `Clone` perform shallow copy which means the contents contains references to the same object as the elements in the original array.  

## Comparing arrays for equality
LINQ provides a built-in function for checking the equality of two `IEnumerable`s, and that function can be used on arrays.

The [`SequenceEqual`][1] function will return `true` if the arrays have the same length and the values in corresponding indices are equal, and `false` otherwise.

    int[] arr1 = { 3, 5, 7 };
    int[] arr2 = { 3, 5, 7 };
    bool result = arr1.SequenceEqual(arr2);
    Console.WriteLine("Arrays equal? {0}", result);

This will print:

<!-- language: lang-none -->
    Arrays equal? True

  [1]: https://msdn.microsoft.com/en-us/library/bb348567(v=vs.110).aspx

## Multi-dimensional arrays
Arrays can have more than one dimension. The following example creates a two-dimensional array of ten rows and ten columns:

    int[,] arr = new int[10, 10];

An array of three dimensions:

    int[,,] arr = new int[10, 10, 10];

You can also initialize the array upon declaration:

    int[,] arr = new int[4, 2] { {1, 1}, {2, 2}, {3, 3}, {4, 4} };

    // Access a member of the multi-dimensional array:
    Console.Out.WriteLine(arr[3, 1]);  // 4

 

## Getting and setting array values
    int[] arr = new int[] { 0, 10, 20, 30}; 

    // Get 
    Console.WriteLine(arr[2]); // 20

    // Set 
    arr[2] = 100;

    // Get the updated value
    Console.WriteLine(arr[2]); // 100


## Iterate over an array
    int[] arr = new int[] {1, 6, 3, 3, 9};

    for (int i = 0; i < arr.Length; i++) 
    {
        Console.WriteLine(arr[i]);
    }

using foreach:

    foreach (int element in arr) 
    {
        Console.WriteLine(element);
    }

using unsafe access with pointers
https://msdn.microsoft.com/en-ca/library/y31yhkeb.aspx
 

    unsafe
    {
        int length = arr.Length;
        fixed (int* p = arr)
        {
            int* pInt = p;
            while (length-- > 0)
            {
                Console.WriteLine(*pInt);
                pInt++;// move pointer to next element
            }
        }
    }

Output:

> 1  
> 6  
> 3  
> 3  
> 9  


## Jagged arrays
Jagged arrays are arrays that instead of primitive types, contain arrays (or other collections). It's like an array of arrays - each array element contains another array.<br/><br/>
They are similar to multidimensional arrays, but have a slight difference - as multidimensional arrays are limited to a fixed number of rows and columns, with jagged arrays, every row can have a different number of columns.

**Declaring a jagged array**

For example, declaring a jagged array with 8 columns:

    int[][] a = new int[8][];
The second `[]` is initialized without a number. To initialize the sub arrays, you would need to do that separately:

    for (int i = 0; i < a.length; i++) 
    {
        a[i] = new int[10];
    }

**Getting/Setting values**

Now, getting one of the subarrays is easy. Let's print all the numbers of the 3rd column of `a`:

    for (int i = 0; i < a[2].length; i++)
    {
        Console.WriteLine(a[2][i]);
    }
Getting a specific value:

    a[<row_number>][<column_number>]
Setting a specific value:

    a[<row_number>][<column_number>] = <value>

**Remember**: It's always recommended to use jagged arrays (arrays of arrays) rather than multidimensional arrays (matrixes). It's faster and safer to use.

----------

**Note on the order of the brackets**

Consider a three-dimensional array of five-dimensional arrays of one-dimensional arrays of `int`. This is written in C# as:

    int[,,][,,,,][] arr = new int[8, 10, 12][,,,,][];

In the CLR type system, the convention for the ordering of the brackets is reversed, so with the above `arr` instance we have:

        arr.GetType().ToString() == "System.Int32[][,,,,][,,]"

and likewise:

        typeof(int[,,][,,,,][]).ToString() == "System.Int32[][,,,,][,,]"

## Creating an array of sequential numbers
LINQ provides a method that makes it easy to create a collection filled with sequential numbers. For example, you can declare an array which contains the integers between 1 and 100.

The [`Enumerable.Range`][1] method allows us to create sequence of integer numbers from a specified start position and a number of elements. 

The method takes two arguments: the starting value and the number of elements to generate.

    Enumerable.Range(int start, int count)

_Note that `count` cannot be negative._

## Usage:

    int[] sequence = Enumerable.Range(1, 100).ToArray();

This will generate an array containing the numbers 1 through 100 (`[1, 2, 3, ..., 98, 99, 100]`).

Because the `Range` method returns an `IEnumerable<int>`, we can use other LINQ methods on it:

    int[] squares = Enumerable.Range(2, 10).Select(x => x * x).ToArray();

This will generate an array that contains 10 integer squares starting at `4`: `[4, 9, 16, ..., 100, 121]`.


  [1]: https://msdn.microsoft.com/en-us/library/system.linq.enumerable.range(v=vs.110).aspx

## Array covariance
    string[] strings = new[] {"foo", "bar"};
    object[] objects = strings; // implicit conversion from string[] to object[]

This conversion is not type-safe. The following code will raise a runtime exception:

    string[] strings = new[] {"Foo"};
    object[] objects = strings;

    objects[0] = new object(); // runtime exception, object is not string
    string str = strings[0];   // would have been bad if above assignment had succeeded

## Checking if one array contains another array


## Arrays as IEnumerable<> instances
All arrays implement the non-generic `IList` interface (and hence non-generic `ICollection` and `IEnumerable` base interfaces).

More importantly, one-dimensional arrays implement the `IList<>` and `IReadOnlyList<>` generic interfaces (and their base interfaces) for the type of data that they contain. This means that they can be treated as generic enumerable types and passed in to a variety of methods without needing to first convert them to a non-array form.

    int[] arr1 = { 3, 5, 7 };
    IEnumerable<int> enumerableIntegers = arr1; //Allowed because arrays implement IEnumerable<T>
    List<int> listOfIntegers = new List<int>();
    listOfIntegers.AddRange(arr1); //You can pass in a reference to an array to populate a List.

After running this code, the list `listOfIntegers` will contain a `List<int>` containing the values 3, 5, and 7.

The `IEnumerable<>` support means arrays can be queried with LINQ, for example `arr1.Select(i => 10 * i)`.

