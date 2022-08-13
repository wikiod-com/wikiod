---
title: "Input"
slug: "input"
draft: false
images: []
weight: 9912
type: docs
toc: true
---

## Syntax
- readline()
- readlines()
- readstring(STDIN)
- chomp(str)
- open(f, file)
- eachline(io)
- readstring(file)
- read(file)
- readcsv(file)
- readdlm(file)

## Parameters
| Parameter | Details |
|-----------|---------|
| **`chomp(str)`** | **Remove up to one trailing newline from a string.** |
| `str` | The string to strip a trailing newline from. Note that [strings][1] are immutable by convention. This function returns a new string. |
| **`open(f, file)`** | **Open a file, call the function, and close the file afterward.** |
| `f` | The function to call on the IO stream opening the file generates. |
| `file` | The path of the file to open. |


  [1]: https://www.wikiod.com/julia-lang/strings

## Reading a String from Standard Input
The `STDIN` stream in Julia refers to [standard input](https://en.wikipedia.org/wiki/Standard_streams#Standard_input_.28stdin.29). This can represent either user input, for interactive command-line programs, or input from a file or [pipeline][1] that has been redirected into the program.

The `readline` function, when not provided any arguments, will read data from `STDIN` until a newline is encountered, or the `STDIN` stream enters the end-of-file state. These two cases can be distinguished by whether the `\n` character has been read as the final character:

    julia> readline()
    some stuff
    "some stuff\n"
    
    julia> readline()  # Ctrl-D pressed to send EOF signal here
    ""

Often, for interactive programs, we do not care about the EOF state, and just want a string. For instance, we may prompt the user for input:

    function askname()
        print("Enter your name: ")
        readline()
    end

This is not quite satisfactory, however, because of the additional newline:

    julia> askname()
    Enter your name: Julia
    "Julia\n"

The `chomp` function is available to remove up to one trailing newline off a string. For example:

    julia> chomp("Hello, World!")
    "Hello, World!"

    julia> chomp("Hello, World!\n")
    "Hello, World!"

We may therefore augment our function with `chomp` so that the result is as expected:

    function askname()
        print("Enter your name: ")
        chomp(readline())
    end

which has a more desirable result:

    julia> askname()
    Enter your name: Julia
    "Julia"

Sometimes, we may wish to read as many lines as is possible (until the input stream enters the end-of-file state). The `readlines` function provides that capability.

    julia> readlines()  # note Ctrl-D is pressed after the last line
    A, B, C, D, E, F, G
    H, I, J, K, LMNO, P
    Q, R, S
    T, U, V
    W, X  
    Y, Z
    6-element Array{String,1}:
     "A, B, C, D, E, F, G\n"
     "H, I, J, K, LMNO, P\n"
     "Q, R, S\n"
     "T, U, V\n"
     "W, X\n"
     "Y, Z\n"

<!-- if version [gte 0.5.0] -->
Once again, if we dislike the newlines at the end of lines read by `readlines`, we can use the `chomp` function to remove them. This time, we [broadcast][2] the `chomp` function across the entire array:

    julia> chomp.(readlines())
    A, B, C, D, E, F, G
    H, I, J, K, LMNO, P
    Q, R, S
    T, U, V
    W, X  
    Y, Z
    6-element Array{String,1}:
     "A, B, C, D, E, F, G"
     "H, I, J, K, LMNO, P"
     "Q, R, S"            
     "T, U, V"            
     "W, X  "             
     "Y, Z"               

<!-- end version if -->

Other times, we may not care about lines at all, and simply want to read as much as possible as a single string. The `readstring` function accomplishes this:

    julia> readstring(STDIN)
    If music be the food of love, play on,
    Give me excess of it; that surfeiting,
    The appetite may sicken, and so die.  # [END OF INPUT]
    "If music be the food of love, play on,\nGive me excess of it; that surfeiting,\nThe appetite may sicken, and so die.\n"

(the `# [END OF INPUT]` is not part of the original input; it has been added for clarity.)

Note that `readstring` must be passed the `STDIN` argument.


  [1]: https://www.wikiod.com/bash/pipelines
  [2]: https://www.wikiod.com/julia-lang/higher-order-functions

## Reading Numbers from Standard Input
Reading numbers from standard input is a combination of reading strings and parsing such strings as numbers.

The `parse` function is used to parse a string into the desired number type:

    julia> parse(Int, "17")
    17
    
    julia> parse(Float32, "-3e6")
    -3.0f6

The format expected by `parse(T, x)` is similar to, but not exactly the same, as the format Julia expects from [number literals][1]:

    julia> -00000023
    -23
    
    julia> parse(Int, "-00000023")
    -23
    
    julia> 0x23 |> Int
    35
    
    julia> parse(Int, "0x23")
    35
    
    julia> 1_000_000
    1000000
    
    julia> parse(Int, "1_000_000")
    ERROR: ArgumentError: invalid base 10 digit '_' in "1_000_000"
     in tryparse_internal(::Type{Int64}, ::String, ::Int64, ::Int64, ::Int64, ::Bool) at ./parse.jl:88
     in parse(::Type{Int64}, ::String) at ./parse.jl:152

Combining the `parse` and `readline` functions allows us to read a single number from a line:

    function asknumber()
        print("Enter a number: ")
        parse(Float64, readline())
    end

which works as expected:

    julia> asknumber()
    Enter a number: 78.3
    78.3

The usual caveats about [floating-point precision][2] apply. Note that `parse` can be used with `BigInt` and `BigFloat` to remove or minimize loss of precision.

Sometimes, it is useful to read more than one number from the same line. Typically, the line can be split with whitespace:

    function askints()
        print("Enter some integers, separated by spaces: ")
        [parse(Int, x) for x in split(readline())]
    end

which can be used as follows:

    julia> askints()
    Enter some integers, separated by spaces: 1 2 3 4
    4-element Array{Int64,1}:
     1
     2
     3
     4



  [1]: https://www.wikiod.com/julia-lang/arithmetic
  [2]: https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html

## Reading Data from a File
## Reading strings or bytes

Files can be opened for reading using the `open` function, which is often used together with [do block syntax][1]:

    open("myfile") do f
        for (i, line) in enumerate(eachline(f))
            print("Line $i: $line")
        end
    end

Suppose `myfile` exists and its contents are

    What's in a name? That which we call a rose
    By any other name would smell as sweet.

Then, this code would produce the following result:

    Line 1: What's in a name? That which we call a rose
    Line 2: By any other name would smell as sweet.

Note that `eachline` is a lazy [iterable][2] over the lines of the file. It is preferred to `readlines` for performance reasons.

Because `do` block syntax is just syntactic sugar for anonymous functions, we can pass named functions to `open` too:

    julia> open(readstring, "myfile")
    "What's in a name? That which we call a rose\nBy any other name would smell as sweet.\n"

    julia> open(read, "myfile")
    84-element Array{UInt8,1}:
     0x57
     0x68
     0x61
     0x74
     0x27
     0x73
     0x20
     0x69
     0x6e
     0x20
        ⋮
     0x73
     0x20
     0x73
     0x77
     0x65
     0x65
     0x74
     0x2e
     0x0a

The functions `read` and `readstring` provide convenience methods that will open a file automatically:

    julia> readstring("myfile")
    "What's in a name? That which we call a rose\nBy any other name would smell as sweet.\n"

## Reading structured data

Suppose we had a [CSV file][3] with the following contents, in a file named `file.csv`:

    Make,Model,Price
    Foo,2015A,8000
    Foo,2015B,14000
    Foo,2016A,10000
    Foo,2016B,16000
    Bar,2016Q,20000

Then we may use the `readcsv` function to read this data into a `Matrix`:

    julia> readcsv("file.csv")
    6×3 Array{Any,2}:
     "Make"  "Model"       "Price"
     "Foo"   "2015A"   8000       
     "Foo"   "2015B"  14000       
     "Foo"   "2016A"  10000       
     "Foo"   "2016B"  16000       
     "Bar"   "2016Q"  20000      

If the file were instead delimited with tabs, in a file named `file.tsv`, then the `readdlm` function can be used instead, with the `delim` argument set to `'\t'`. More advanced workloads should use the [CSV.jl](https://github.com/JuliaData/CSV.jl) [package][4].


  [1]: https://www.wikiod.com/julia-lang/functions#Anonymous functions
  [2]: https://www.wikiod.com/julia-lang/iterables
  [3]: https://www.wikiod.com/csv/getting-started-with-csv
  [4]: https://www.wikiod.com/julia-lang/packages#Install, use, and remove a registered package

