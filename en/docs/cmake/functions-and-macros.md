---
title: "Functions and Macros"
slug: "functions-and-macros"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

The main difference between _macros_ and _functions_ is, that _macros_ are evaluated within the current context, while _functions_ open a new scope within the current one. Thus, variables defined within _functions_ are not known after the function has been evaluated. On the contrary, variables within _macros_ are still defined after the macro has been evaluated.

## Simple Macro to define a variable based on input
```
macro(set_my_variable _INPUT)
  if("${_INPUT}" STREQUAL "Foo")
    set(my_output_variable "foo")
  else()
    set(my_output_variable "bar")
  endif()
endmacro(set_my_variable)
```
Use the macro:
```
set_my_variable("Foo")
message(STATUS ${my_output_variable})
```
will print
```
-- foo
```
while
```
set_my_variable("something else")
message(STATUS ${my_output_variable})
```
will print
```
-- bar
```

## Macro to fill a variable of given name
```
macro(set_custom_variable _OUT_VAR)
  set(${_OUT_VAR} "Foo")
endmacro(set_custom_variable)
```
Use it with
```
set_custom_variable(my_foo)
message(STATUS ${my_foo})
```
which will print
```
-- Foo
```

