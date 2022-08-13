---
title: "global and local variables"
slug: "global-and-local-variables"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

By default, every variable in bash is **global** to every function, script and even the outside shell if you are declaring your variables inside a script.

If you want your variable to be local to a function, you can use `local` to have that variable a new variable that is independent to the global scope and whose value will only be accessible inside that function.

## Global variables
```
var="hello"

function foo(){
    echo $var
}

foo
```

Will obviously output "hello", but this works the other way around too:


```
function foo()  {
    var="hello"
}

foo
echo $var
```

Will also output <code>"hello"</code>

## Local variables
```
function foo() {
    local var
    var="hello"
}

foo
echo $var
```

Will output nothing, as var is a variable local to the function foo, and its value is not visible from outside of it.

## Mixing the two together
```
var="hello"

function foo(){
    local var="sup?"
    echo "inside function, var=$var"
}

foo
echo "outside function, var=$var"
```

Will output
```
inside function, var=sup?
outside function, var=hello
```


