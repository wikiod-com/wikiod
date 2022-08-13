---
title: "Modules"
slug: "modules"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## **Module Names**

In Elixir, module names such as ```IO``` or ```String``` are just atoms under the hood and are converted to the form ```:"Elixir.ModuleName"``` at compile time. 
```
iex(1)> is_atom(IO)
true
iex(2)> IO == :"Elixir.IO"
true
```





## List a module's functions or macros
The `__info__/1` function takes one of the following atoms:

 - `:functions` - Returns a keyword list of public functions along with their
   arities 
 - `:macros` - Returns a keyword list of public macros along with their
   arities 

To list the `Kernel` module’s functions:

    iex> Kernel.__info__ :functions
    [!=: 2, !==: 2, *: 2, +: 1, +: 2, ++: 2, -: 1, -: 2, --: 2, /: 2, <: 2, <=: 2,
     ==: 2, ===: 2, =~: 2, >: 2, >=: 2, abs: 1, apply: 2, apply: 3, binary_part: 3,
     bit_size: 1, byte_size: 1, div: 2, elem: 2, exit: 1, function_exported?: 3,
     get_and_update_in: 3, get_in: 2, hd: 1, inspect: 1, inspect: 2, is_atom: 1,
     is_binary: 1, is_bitstring: 1, is_boolean: 1, is_float: 1, is_function: 1,
     is_function: 2, is_integer: 1, is_list: 1, is_map: 1, is_number: 1, is_pid: 1,
     is_port: 1, is_reference: 1, is_tuple: 1, length: 1, macro_exported?: 3,
     make_ref: 0, ...]

Replace `Kernel` with any module of your choosing.

## Delegating functions to another module
Use `defdelegate` to define functions that delegate to functions of the same name defined in another module:

    defmodule Math do
      defdelegate pi, to: :math
    end


    iex> Math.pi
    3.141592653589793



## Using modules
Modules have four associated keywords to make using them in other modules: `alias`, `import`, `use`, and `require`.

`alias` will register a module under a different (usually shorter) name:

    defmodule MyModule do
      # Will make this module available as `CoolFunctions`
      alias MyOtherModule.CoolFunctions
      # Or you can specify the name to use
      alias MyOtherModule.CoolFunctions, as: CoolFuncs
    end

`import` will make all the functions in the module available with no name in front of them:

    defmodule MyModule do
      import Enum
      def do_things(some_list) do
        # No need for the `Enum.` prefix
        join(some_list, " ")
      end
    end

`use` allows a module to inject code into the current module - this is typically done as part of a framework that creates its own functions to make your module confirm to some behaviour.

`require` loads macros from the module so that they can be used.

