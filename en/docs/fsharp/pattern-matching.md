---
title: "Pattern Matching"
slug: "pattern-matching"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

Pattern Matching is a powerful feature of many functional languages as it often allows branching to be handled very succinctly compared to using multiple `if`/`else if`/`else` style statements.  However given enough options and ["when" guards][1], Pattern Matching can also become verbose and difficult to understand at a glance.

When this happens F#'s [Active Patterns][2] can be a great way to give meaningful names to the matching logic, which simplifies the code and also enables reuse.

  [1]: https://www.wikiod.com/docs/f%23/1335/pattern-matching/7377/when-guards-let-you-add-arbitrary-conditionals#t=201607222211464650253
  [2]: https://www.wikiod.com/docs/f%23/962/active-patterns#t=201607222206441747382

## Pattern matching checks the entire domain is covered
    let x = true
    match x with
    | true -> printfn "x is true"

# yields a warning

> C:\\Program Files (x86)\\Microsoft VS Code\\Untitled-1(2,7): warning FS0025: Incomplete pattern matches on this expression. For example, the value 'false' may indicate a case not covered by the pattern(s).

This is because not all of the possible bool values were covered.

# bools can be explicitly listed but ints are harder to list out


    let x = 5
    match x with
    | 1 -> printfn "x is 1"
    | 2 -> printfn "x is 2"
    | _ -> printfn "x is something else"


here we use the special `_` character. The `_` matches all other possible cases.

# The `_` can get you into trouble

consider a type we create ourselves it looks like this

    type Sobriety = 
        | Sober
        | Tipsy
        | Drunk

We might write a match with expession that looks like this

    match sobriety with
    | Sober -> printfn "drive home"
    | _ -> printfn "call an uber"

The above code makes sense. We are assuming if you aren't sober you should call an uber so we use the `_` to denote that

We later refactor our code to this

    type Sobriety = 
        | Sober
        | Tipsy
        | Drunk
        | Unconscious

The F# compiler should give us a warning and prompt us to refactor our match expression to have the person seek medical attention. Instead the match expression silently treats the unconscious person as if they were only tipsy. The point is you should opt to explicitly list out cases when possible to avoid logic errors.

##  When guards let you add arbitrary conditionals
    type Person = {
        Age : int
        PassedDriversTest : bool }

    let someone = { Age = 19; PassedDriversTest = true }

    match someone.PassedDriversTest with
    | true when someone.Age >= 16 -> printfn "congrats"
    | true -> printfn "wait until you are 16"
    | false -> printfn "you need to pass the test"

## Cases are evaluated from top to bottom and the first match is used
**Incorrect usage:**

In the following snippet, the last match will never be used:

    let x = 4
    match x with
    | 1 -> printfn "x is 1"
    | _ -> printfn "x is anything that wasn't listed above"
    | 4 -> printfn "x is 4"

prints

> x is anything that wasn't listed above

**Correct usage:**

Here, both `x = 1` and `x = 4` will hit their specific cases, while everything else will fall through to the default  case `_`:

    let x = 4
    match x with
    | 1 -> printfn "x is 1"
    | 4 -> printfn "x is 4"
    | _ -> printfn "x is anything that wasn't listed above"

prints

> x is 4




## Matching Options
Pattern matching can be useful to handle Options:

    let result = Some("Hello World")
    match result with
    | Some(message) -> printfn message
    | None -> printfn "Not feeling talkative huh?"



