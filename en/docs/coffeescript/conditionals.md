---
title: "Conditionals"
slug: "conditionals"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## if, if / then, if / else, unless, ternary operator
The most basic instance of an `if` construct evaluates a condition and executes some code according to the condition outcome. If the condition returns `true`, the code within the conditional is executed.

```
counter = 10
if counter is 10
  console.log 'This will be executed!'
```

The `if` construct can be enriched with an `else` statement. The code within the `else` statement will be executed whenever the `if` condition is not met.

```
counter = 9
if counter is 10
  console.log 'This will not be executed...'
else
  console.log '... but this one will!'
```

`if` constructs can be chained using `else`, without any limitation on how many can be chained. The first conditional that returns `true` will run its code and stop the check: no conditional below that point will be evaluated thereafter, and no code block from withing those conditionals will be executed.

```
if counter is 10
  console.log 'I counted to 10'
else if counter is 9
  console.log 'I counted to 9'
else if counter < 7
  console.log 'Not to 7 yet'
else
  console.log 'I lost count'
```

The opposite form of `if` is `unless`. Unlike `if`, `unless` will only run if the conditional returns `false`. 

```
counter = 10
unless counter is 10
  console.log 'This will not be executed!
```

The `if` statements can be placed in a single line, but in this case, the `then` keyword is required.

```
if counter is 10 then console.log 'Counter is 10'
```

An alternative syntax is the Ruby-like:

```
console.log 'Counter is 10' if counter is 10
```

The last two  blocks of code are equivalent.

The ternary operator is a compression of an `if / then / else` construct, and can be used when assigning values to variables. The final value assigned to the variable will be the one defined after the `then` when the `if` condition is met. Otherwise, the value after the `else` will be assigned.

```
outcome = if counter is 10 then 'Done counting!' else 'Still counting'
```

## Switch
**TL; DR:** CoffeeScript `switch` statements use `when` for each case and `else` for the default case. They use `then` for one-line cases and commas for multiple cases with a single outcome. They intentionally disallow fallthrough and so don't need an explicit `break` (since it's always there implicitly). A switch statement can be used as a returnable, assignable expression.

CoffeeScript `switch` statements are a sort of control statement that allows you to take different actions based on a value. They are like `if` statements, but where an `if` statement usually takes one of two actions based on whether something is `true` or `false`, `switch` statements take one of any number of actions depending on the value of any expression - a string, number, or anything at all.

CoffeeScript `switch` start with the keyword `switch` followed by the expression to switch on. Then, each case is represented by the keyword `when` followed by the value for that case.

    switch name
      when "Alice"
        # Code here will run when name is Alice
        callAlice()
      when "Bob"
        # Code here will run when name is Bob
        giveBobSandwich()

There is also a shorthand syntax for when each case is one line, using the `then` keyword instead of a newline:

    livesLeft = 2
    switch livesLeft
      when 3 then fullHealth()
      when 2 then healthAt 2
      when 1 then healthAt 1
      when 0 then playerDie()

You can mix and match the two formats as necessary:

    livesLeft = 2
    switch livesLeft
      when 3 then fullHealth()
      when 2 then healthAt 2
      when 1
        healthAt 1
        alert "Warning! Health low!"
      when 0 then playerDie()

Although the most common things to switch on are a variable (as in the previous example) or the result of a functoin, you can switch on any expression you choose:

    indexOfAnswer = 0
    switch indexOfAnswer + 1
      when 1 then console.log "The answer is the 1st item"
      when 2 then console.log "The answer is the 2nd item"
      when 3 then console.log "The answer is the 3rd item"

You can also have multiple cases lead to the same action:

    switch password
      when "password", "123456", "letmein" then console.log "Wrong!"
      when "openpoppyseed" then console.log "Close, but no cigar."
      when "opensesame" then console.log "You got it!"

A very useful feature is a default or catch-all case, that will only execute if none of the other criteria are met. CoffeeScript signifies this with the `else` keyword:

    switch password
      when "password", "123456", "letmein" then console.log "Wrong!"
      when "openpoppyseed" then console.log "Close, but no cigar."
      when "opensesame" then console.log "You got it!"
      else console.log "Not even close..."

(Note that you don't need the `then` keyword for the `else` case because there is no condition.)

Now here's an example of all the features of `switch` in action!

    switch day
      when "Mon" then go work
      when "Tue" then go relax
      when "Thu" then go iceFishing
      when "Fri", "Sat"
        if day is bingoDay
          go bingo
          go dancing
      when "Sun" then go church
      else go work

You can also have the condition of a case be an expression:

    switch fullName
      when myFullName() then alert "Doppelgänger detected"
      when presidentFirstName + " " + presidentLastName
        alert "Get down Mr. president!"
        callSecretService()
      when "Joey Bonzo" then alert "Joey Bonzo everybody"

CoffeeScript `switch` statements also have a unique trait: they can return values like a function. If you assign a variable to a `switch` statement, then it will be assigned whatever the statement returns.

    address = switch company
      when "Apple" then "One Infinite Loop"
      when "Google" then "1600 Amphitheatre Parkway"
      when "ACME"
        if isReal
          "31918 Hayman St"
        else
          "Unknown desert location"
      else lookUpAddress company

(Remember that the last statement in a block is implicitly returned. You can also use the `return` keyword manually.)

Switch statements can also be used without a control expression, turning them in to a cleaner alternative to if/else chains.

    score = 76
    grade = switch
      when score < 60 then 'F'
      when score < 70 then 'D'
      when score < 80 then 'C'
      when score < 90 then 'B'
      else 'A'

(This is functionally equivalent to `grade = switch true` because the first case that evaluates to `true` will match. However, since each case implicitly `break`s at the end, only the first case to match will be executed.)

