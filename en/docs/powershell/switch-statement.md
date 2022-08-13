---
title: "Switch statement"
slug: "switch-statement"
draft: false
images: []
weight: 9200
type: docs
toc: true
---

A switch statement allows a variable to be tested for equality against a list of values. Each value is called a *case*, and the variable being *switched* on is checked for each switch case. It enables you to write a script that can choose from a series of options, but without requiring you to write a long series of if statements.

This topic is documenting the ***switch statement*** used for branching the flow of the script. Do not confuse it with ***switch parameters*** which are used in functions as boolean flags.

## Switch Statement with CaseSensitive Parameter
The `-CaseSensitive` parameter enforces switch statements to perform exact, case-sensitive matching against conditions.

Example:

    switch -CaseSensitive ('Condition')
    { 
      'condition'   {'First Action'}
      'Condition'   {'Second Action'} 
      'conditioN'   {'Third Action'}  
    }

Output:

    Second Action

The second action is the only action executed because it is the only condition that exactly matches the string `'Condition'` when accounting for case-sensitivity.

## Switch Statement with Wildcard Parameter
The `-Wildcard` parameter allows switch statements to perform wildcard matching against conditions.

Example:

    switch -Wildcard ('Condition')
    { 
        'Condition'           {'Normal match'}
        'Condit*'             {'Zero or more wildcard chars.'} 
        'C[aoc]ndit[f-l]on'   {'Range and set of chars.'}  
        'C?ndition'           {'Single char. wildcard'}
        'Test*'               {'No match'} 
    }


Output:

    Normal match
    Zero or more wildcard chars.
    Range and set of chars.
    Single char. wildcard

## Switch Statement with File Parameter
The `-file` parameter allows the switch statement to receive input from a file.  Each line of the file is evaluated by the switch statement.

Example file `input.txt`:

    condition
    test

Example switch statement:

    switch -file input.txt
    { 
      'condition' {'First Action'}
      'test'      {'Second Action'} 
      'fail'      {'Third Action'}   
    }

Output:

    First Action
    Second Action

## Simple Switch with Default Condition
The `Default` keyword is used to execute an action when no other conditions match the input value.

Example:

    switch('Condition')
    {
      'Skip Condition'
      {
        'First Action'
      }
      'Skip This Condition Too'
      {
        'Second Action'
      }
      Default
      {
        'Default Action'
      }
    }

Output:

    Default Action

## Simple Switch
Switch statements compare a single test value to multiple conditions, and performs any associated actions for successful comparisons. It can result in multiple matches/actions.

Given the following switch...

    switch($myValue)
    {
        'First Condition'    { 'First Action' }
        'Second Condition'   { 'Second Action' }
    }

`'First Action'` will be output if `$myValue` is set as `'First Condition'`.

`'Section Action'` will be output if `$myValue` is set as `'Second Condition'`.

Nothing will be output if `$myValue` does not match either conditions. 

## Switch Statement with Regex Parameter
The `-Regex` parameter allows switch statements to perform regular expression matching against conditions.

Example:

    switch -Regex ('Condition')
    { 
      'Con\D+ion'    {'One or more non-digits'}
      'Conditio*$'   {'Zero or more "o"'} 
      'C.ndition'    {'Any single char.'}  
      '^C\w+ition$'  {'Anchors and one or more word chars.'} 
      'Test'         {'No match'} 
    }

Output:

    One or more non-digits
    Any single char.
    Anchors and one or more word chars.

## Simple Switch With Break
The `break` keyword can be used in switch statements to exit the statement before evaluating all conditions.

Example:

    switch('Condition')
    {
      'Condition'
      {
        'First Action'
      }
      'Condition'
      {
        'Second Action'
        break
      }
      'Condition'
      {
        'Third Action'
      }
    }

Output:

    First Action
    Second Action

Because of the `break` keyword in the second action, the third condition is not evaluated.

## Switch Statement with Exact Parameter
The `-Exact` parameter enforces switch statements to perform exact, case-insensitive matching against string-conditions.

Example:

    switch -Exact ('Condition')
    { 
      'condition'   {'First Action'}
      'Condition'   {'Second Action'} 
      'conditioN'   {'Third Action'}  
      '^*ondition$' {'Fourth Action'} 
      'Conditio*'   {'Fifth Action'} 
    }

Output:

    First Action
    Second Action
    Third Action

The first through third actions are executed because their associated conditions matched the input. The regex and wildcard strings in the fourth and fifth conditions fail matching.  

Note that the fourth condition would also match the input string if regular expression matching was being performed, but was ignored in this case because it is not.

## Switch Statement with Expressions
Conditions can also be expressions:

```
$myInput = 0

switch($myInput) {
    # because the result of the expression, 4, 
    # does not equal our input this block should not be run.
    (2+2)  { 'True. 2 +2 = 4' }

    # because the result of the expression, 0, 
    # does equal our input this block should be run.
    (2-2) { 'True. 2-2 = 0' }

    # because our input is greater than -1 and is less than 1 
    # the expression evaluates to true and the block should be run.
    { $_ -gt -1 -and $_ -lt 1 } { 'True. Value is 0' }
}

#Output
True. 2-2 = 0
True. Value is 0
```

