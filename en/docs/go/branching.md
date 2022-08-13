---
title: "Branching"
slug: "branching"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## Goto statements
A `goto` statement transfers control to the statement with the corresponding label within the same function.
Executing the `goto` statement must not cause any variables to come into scope that were not already in scope at the point of the `goto`. 

for example see the standard library source code: https://golang.org/src/math/gamma.go :  

        for x < 0 {
            if x > -1e-09 {
                goto small
            }
            z = z / x
            x = x + 1
        }
        for x < 2 {
            if x < 1e-09 {
                goto small
            }
            z = z / x
            x = x + 1
        }
    
        if x == 2 {
            return z
        }
    
        x = x - 2
        p = (((((x*_gamP[0]+_gamP[1])*x+_gamP[2])*x+_gamP[3])*x+_gamP[4])*x+_gamP[5])*x + _gamP[6]
        q = ((((((x*_gamQ[0]+_gamQ[1])*x+_gamQ[2])*x+_gamQ[3])*x+_gamQ[4])*x+_gamQ[5])*x+_gamQ[6])*x + _gamQ[7]
        return z * p / q
    
    small:
        if x == 0 {
            return Inf(1)
        }
        return z / ((1 + Euler*x) * x) 



## Switch Statements
A simple `switch` statement:
```
switch a + b {
case c:
    // do something
case d:
    // do something else
default:
    // do something entirely different
}
```
The above example is equivalent to:
```
if a + b == c {
    // do something
} else if a + b == d {
    // do something else
} else {
    // do something entirely different
}
```


----------


The `default` clause is optional and will be executed if and only if none of the cases compare true, even if it does not appear last, which is acceptable.  The following is semantically the same as the first example:
```
switch a + b {
default:
    // do something entirely different
case c:
    // do something
case d:
    // do something else
}
```
This could be useful if you intend to use the `fallthrough` statement in the `default` clause, which must be the last statement in a case and causes program execution to proceed to the next case:
```
switch a + b {
default:
    // do something entirely different, but then also do something
    fallthrough
case c:
    // do something
case d:
    // do something else
}
```


----------


An empty switch expression is implicitly `true`:
```
switch {
case a + b == c:
    // do something
case a + b == d:
    // do something else
}
```


----------


Switch statements support a simple statement similar to `if` statements:
```
switch n := getNumber(); n {
case 1:
    // do something
case 2:
    // do something else
}
```


----------


Cases can be combined in a comma-separated list if they share the same logic:
```
switch a + b {
case c, d:
    // do something
default:
    // do something entirely different
}
```

## If Statements
A simple `if` statement:
```
if a == b {
    // do something
}
```
Note that there are no parentheses surrounding the condition and that the opening curly brace `{` must be on the same line.  The following will *not* compile:
```
if a == b
{
    // do something
}
```


----------


An `if` statement making use of `else`:
```
if a == b {
    // do something
} else if a == c {
    // do something else
} else {
    // do something entirely different
}
```


----------


Per [golang.org's documentation][1], "The expression may be preceded by a simple statement, which executes before the expression is evaluated."  Variables declared in this simple statement are scoped to the `if` statement and cannot be accessed outside it:
```
if err := attemptSomething(); err != nil {
    // attemptSomething() was successful!
} else {
    // attemptSomething() returned an error; handle it
}
fmt.Println(err) // compiler error, 'undefined: err'
```


  [1]: https://golang.org/ref/spec#If_statements

## Type Switch Statements
A simple type switch:
```
// assuming x is an expression of type interface{}
switch t := x.(type) {
case nil:
    // x is nil
    // t will be type interface{}
case int: 
    // underlying type of x is int
    // t will be int in this case as well
case string:
    // underlying type of x is string
    // t will be string in this case as well
case float, bool:
    // underlying type of x is either float or bool
    // since we don't know which, t is of type interface{} in this case
default:
    // underlying type of x was not any of the types tested for
    // t is interface{} in this type
}
```

----------

You can test for any type, including `error`, user-defined types, interface types, and function types:
```
switch t := x.(type) {
case error:
    log.Fatal(t)
case myType:
    fmt.Println(myType.message)
case myInterface:
    t.MyInterfaceMethod()
case func(string) bool:
    if t("Hello world?") {
        fmt.Println("Hello world!")
    }
}
```

## Break-continue statements
The break statement, on execution makes the current loop to force exit    

package main
    
    import "fmt"
    
    func main() {
        i:=0
        for true {
          if i>2 {
            break
            }
        fmt.Println("Iteration : ",i)
        i++
        }
    }

The continue statement, on execution moves the control to the start of the loop
    
    import "fmt"
    
    func main() {
        j:=100
        for j<110 {
         j++
         if j%2==0 {
            continue
            } 
        fmt.Println("Var : ",j)        
        }
    }

Break/continue loop inside switch

    import "fmt"
    
    func main() {
        j := 100
    
    loop:
        for j < 110 {
            j++
    
            switch j % 3 {
            case 0:
                continue loop
            case 1:
                break loop
            }
    
            fmt.Println("Var : ", j)
        }
    }



