---
title: "String Macros"
slug: "string-macros"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Syntax
- macro"string" # short, string macro form
- @macro_str "string" # long, regular macro form
- macro\`command\`


String macros are not quite as powerful as plain old strings — because interpolation must be implemented in the macro's logic, string macros are unable to contain string literals of the same delimiter for interpolation.

For instance, although

    julia> "$("x")"
    "x"

works, the string macro text form

    julia> doc"$("x")"
    ERROR: KeyError: key :x not found

gets parsed incorrectly. This can be somewhat mitigated by using triple-quotes as the outer string delimiter;

    julia> doc"""$("x")"""
    "x"

does indeed work properly.


## Using string macros
String macros are syntactic sugar for certain macro invocations. The parser expands syntax like

    mymacro"my string"

into

    @mymacro_str "my string"

which then, like any other macro call, gets substituted with whatever expression the `@mymacro_str` macro returns. Base Julia comes with several string macros, such as:

## `@b_str`

This string macro constructs byte [arrays][1] instead of [strings][2]. The contents of the string, encoded as UTF-8, will be used as the array of bytes. This can be useful for interfacing with low-level APIs, many of which work with byte arrays instead of strings.

    julia> b"Hello World!"
    12-element Array{UInt8,1}:
     0x48
     0x65
     0x6c
     0x6c
     0x6f
     0x20
     0x57
     0x6f
     0x72
     0x6c
     0x64
     0x21


## `@big_str`

This macro will return a `BigInt` or a `BigFloat` parsed from the string it's given.

    julia> big"1"
    1

    julia> big"1.0"
    1.000000000000000000000000000000000000000000000000000000000000000000000000000000

This macro exists because `big(0.1)` does not behave as one might initially expect: the `0.1` is a `Float64` approximation of true `0.1` (`1//10`), and promoting that to `BigFloat` will keep the approximation error of `Float64`. Using the macro will parse `0.1` directly to a `BigFloat`, reducing the approximation error.

    julia> big(0.1)
    1.000000000000000055511151231257827021181583404541015625000000000000000000000000e-01
    
    julia> big"0.1"
    1.000000000000000000000000000000000000000000000000000000000000000000000000000002e-01

## `@doc_str`

This string macro constructs `Base.Markdown.MD` objects, which are used in the internal documentation system to provide rich-text documentation for any environment. These MD objects render well in a terminal:

[![terminal markdown documentation renders well][3]][3]

and also in a browser:

[![browser markdown documentation renders well][4]][4]

## `@html_str`

This string macro constructs HTML string literals, which render nicely in a browser:

[![html string macro rendering nicely in a browser][5]][5]

## `@ip_str`

This string macro constructs IP address literals. It works with both IPv4 and IPv6:

    julia> ip"127.0.0.1"
    ip"127.0.0.1"
    
    julia> ip"::"
    ip"::"

## `@r_str`

This string macro constructs [`Regex` literals][6].

## `@s_str`

This string macro constructs `SubstitutionString` literals, which work together with `Regex` literals to allow more advanced textual substitution.

## `@text_str`

This string macro is similar in spirit to `@doc_str` and `@html_str`, but does not have any fancy formatting features:

[![plain text in the browser][7]][7]

## `@v_str`

This string macro constructs `VersionNumber` literals. See [Version Numbers][8] for a description of what they are and how to use them.

## `@MIME_str`

This string macro constructs the singleton types of MIME types. For instance, `MIME"text/plain"` is the type of `MIME("text/plain")`.


  [1]: https://www.wikiod.com/julia-lang/arrays
  [2]: https://www.wikiod.com/julia-lang/strings
  [3]: http://i.stack.imgur.com/DOnB2.png
  [4]: http://i.stack.imgur.com/Tq06q.png
  [5]: http://i.stack.imgur.com/m4JSw.png
  [6]: https://www.wikiod.com/julia-lang/regexes#Regex literals
  [7]: http://i.stack.imgur.com/i6h1h.png
  [8]: https://www.wikiod.com/julia-lang/cross-version-compatibility#Version numbers

## Implementing interpolation in a string macro
String macros do not come with built-in [interpolation][1] facilities. However, it is possible to manually implement this functionality. Note that it is not possible to embed without escaping string literals that have the same delimiter as the surrounding string macro; that is, although `""" $("x") """` is possible, `" $("x") "` is not. Instead, this must be escaped as `" $(\"x\") "`. See the [remarks](//stackoverflow.com/documentation/julia-lang/5817/string-macros#remarks) section for more details about this limitation.

There are two approaches to implementing interpolation manually: implement parsing manually, or get Julia to do the parsing. The first approach is more flexible, but the second approach is easier.

## Manual parsing

    macro interp_str(s)
        components = []
        buf = IOBuffer(s)
        while !eof(buf)
            push!(components, rstrip(readuntil(buf, '$'), '$'))
            if !eof(buf)
                push!(components, parse(buf; greedy=false))
            end
        end
        quote
            string($(map(esc, components)...))
        end
    end

## Julia parsing

    macro e_str(s)
        esc(parse("\"$(escape_string(s))\""))
    end

This method escapes the string (but note that `escape_string` does *not* escape the `$` signs) and passes it back to Julia's parser to parse. Escaping the string is necessary to ensure that `"` and `\` do not affect the string's parsing. The resulting expression is a `:string` expression, which can be examined and decomposed for macro purposes.

  [1]: https://www.wikiod.com/julia-lang/strings#String interpolation (insert value defined by variable into string)

## Symbols that are not legal identifiers
Julia Symbol literals must be legal identifiers. This works:

    julia> :cat
    :cat

But this does not:

    julia> :2cat
    ERROR: MethodError: no method matching *(::Int64, ::Base.#cat)
    Closest candidates are:
      *(::Any, ::Any, ::Any, ::Any...) at operators.jl:288
      *{T<:Union{Int128,Int16,Int32,Int64,Int8,UInt128,UInt16,UInt32,UInt64,UInt8}}(::T<:Union{Int128,Int16,Int32,Int64,Int8,UInt128,UInt16,UInt32,UInt64,UInt8}, ::T<:Union{Int128,Int16,Int32,Int64,Int8,UInt128,UInt16,UInt32,UInt64,UInt8}) at int.jl:33
      *(::Real, ::Complex{Bool}) at complex.jl:180
      ...

What looks like a symbol literal here is actually being parsed as an implicit multiplication of `:2` (which is just `2`) and the function `cat`, which obviously does not work.

We can use

    julia> Symbol("2cat")
    Symbol("2cat")

to work around the issue.

A string macro could help to make this more terse. If we define the `@sym_str` macro:

    macro sym_str(str)
        Meta.quot(Symbol(str))
    end

then we can simply do

    julia> sym"2cat"
    Symbol("2cat")

to create symbols which are not valid Julia identifiers.

Of course, these techniques can also create symbols that _are_ valid Julia identifiers. For example,

    julia> sym"test"
    :test

## Command macros
<!-- if version [gte 0.6.0-dev] -->
In Julia v0.6 and later, command macros are supported in addition to regular string macros. A command macro invocation like

    mymacro`xyz`

gets parsed as the macro call

    @mymacro_cmd "xyz"

Note that this is similar to string macros, except with `_cmd` instead of `_str`.

We typically use command macros for code, which in many languages frequently contains `"` but rarely contains <code>\`</code>. For instance, it is fairly straightforward to reimplement a simple version of [quasiquoting][1] using command macros:

    macro julia_cmd(s)
        esc(Meta.quot(parse(s)))
    end

We can use this macro either inline:

    julia> julia`1+1`
    :(1 + 1)
    
    julia> julia`hypot2(x,y)=x^2+y^2`
    :(hypot2(x,y) = begin  # none, line 1:
                x ^ 2 + y ^ 2
            end)

or multiline:

    julia> julia```
           function hello()
               println("Hello, World!")
           end
           ```
    :(function hello() # none, line 2:
            println("Hello, World!")
        end)

Interpolation using `$` is supported:

    julia> x = 2
    2
    
    julia> julia`1 + $x`
    :(1 + 2)

but the version given here only allows one expression:

    julia> julia```
           x = 2
           y = 3
           ```
    ERROR: ParseError("extra token after end of expression")

However, extending it to handle multiple expressions is not difficult.

<!-- end version if -->


  [1]: https://www.wikiod.com/julia-lang/expressions#Intro to Expressions


