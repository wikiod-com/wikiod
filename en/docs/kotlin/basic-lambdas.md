---
title: "Basic Lambdas"
slug: "basic-lambdas"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Syntax
- Explicit parameters:
- { parameterName: ParameterType, otherParameterName: OtherParameterType -> anExpression() }

- Inferred parameters:
- val addition: (Int, Int) -> Int = { a, b -> a + b }

- Single parameter `it` shorthand
- val square: (Int) -> Int = { it*it }

- Signature:
- () -> ResultType
- (InputType) -> ResultType
- (InputType1, InputType2) -> ResultType  

Input type parameters can be left out when they can be left out when they can be inferred from the context. For example say you have a function on a class that takes a function:

```
data class User(val fistName: String, val lastName: String) {
    fun username(userNameGenerator: (String, String) -> String) =
        userNameGenerator(firstName, secondName)
}
```

You can use this function by passing a lambda, and since the parameters are already specified in the function signature there's no need to re-declare them in the lambda expression:

```
val user = User("foo", "bar")
println(user.userName { firstName, secondName ->
     "${firstName.toUppercase}"_"${secondName.toUppercase}"
 }) // prints FOO_BAR
```

This also applies when you are assigning a lambda to a variable:
```
//valid:
val addition: (Int, Int) = { a, b -> a + b }
//valid:
val addition = { a: Int, b: Int -> a + b }
//error (type inference failure):
val addition = { a, b -> a + b }
```

When the lambda takes one parameter, and the type can be inferred from the context, you can refer to the parameter by `it`.

```
listOf(1,2,3).map { it * 2 } // [2,4,6]
```




## Lambda as parameter to filter function
```
val allowedUsers = users.filter { it.age > MINIMUM_AGE }
```

## Lambda passed as a variable
```
val isOfAllowedAge = { user: User -> user.age > MINIMUM_AGE }
val allowedUsers = users.filter(isOfAllowedAge)
```

## Lambda for benchmarking a function call
General-purpose stopwatch for timing how long a function takes to run:
```
object Benchmark {
    fun realtime(body: () -> Unit): Duration {
        val start = Instant.now()
        try {
            body()
        } finally {
            val end = Instant.now()
            return Duration.between(start, end)
        }
    }
}
```

Usage:
```
val time = Benchmark.realtime({
    // some long-running code goes here ...
})
println("Executed the code in $time")
```

