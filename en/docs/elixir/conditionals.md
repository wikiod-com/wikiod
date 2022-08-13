---
title: "Conditionals"
slug: "conditionals"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

Note that the `do...end` syntax is syntactic sugar for regular keyword lists, so you can actually do this:

    unless false, do: IO.puts("Condition is false")
    # Outputs "Condition is false"

    # With an `else`:
    if false, do: IO.puts("Condition is true"), else: IO.puts("Condition is false")
    # Outputs "Condition is false"

## case

      
     case {1, 2} do
      {3, 4} ->
        "This clause won't match."
      {1, x} ->
        "This clause will match and bind x to 2 in this clause."
      _ ->
        "This clause would match any value."
    end

   `case` is only used to match the given pattern of the particular data. Here , `{1,2}` is matching with different case pattern that is given in the code example.
         

## if and unless
    if true do
        "Will be seen since condition is true."
    end    

    if false do
        "Won't be seen since condition is false."
    else
        "Will be seen.
    end

    unless false do
        "Will be seen."
    end

    unless true do 
        "Won't be seen."
    else
        "Will be seen."
    end

## cond
    cond do 
       0 == 1 -> IO.puts "0 = 1"
       2 == 1 + 1 -> IO.puts "1 + 1 = 2" 
       3 == 1 + 2 -> IO.puts "1 + 2 = 3" 
    end

    # Outputs "1 + 1 = 2" (first condition evaluating to true)

`cond` will raise a `CondClauseError` if no conditions are true.

    cond do
      1 == 2 -> "Hmmm"
      "foo" == "bar" -> "What?"
    end
    # Error

This can be avoided by adding a condition that will always be true.

    cond do
      ... other conditions
      true -> "Default value"
    end

Unless it is never expected to reach the default case, and the program should in fact crash at that point.

## with clause
`with` clause is used to combine matching clauses.
It looks like we combine anonymous functions or handle function with multiple bodies (matching clauses).
Consider the case: we create a user, insert it into DB, then create greet email and then send it to the user.

Without the `with` clause we might write something like this (I omitted functions implementations):

    case create_user(user_params) do
      {:ok, user} ->
        case Mailer.compose_email(user) do
          {:ok, email} ->
            Mailer.send_email(email)
          {:error, reason} ->
            handle_error
        end
      {:error, changeset} ->
        handle_error
    end
    
Here we handle our business process's flow with `case` (it could be `cond` or `if`). That leads us to so-called ['pyramid of doom'][1], because we have to deal with possible conditions and decide: whether move further or not. It would be much nicer to rewrite this code with `with` statement:

    with {:ok, user} <- create_user(user_params),
         {:ok, email} <- Mailer.compose_email(user) do
      {:ok, Mailer.send_email}
    else
      {:error, _reason} ->
        handle_error
    end

In the code snippet above we've rewrite nested `case` clauses with `with`. Within `with` we invoke some functions (either anonymous or named) and pattern match on their outputs. If all matched, `with` return `do` block result, or `else` block result otherwise.

We can omit `else` so `with` will return either `do` block result or the first fail result.

So, the value of `with` statement is its `do` block result.


  [1]: https://en.wikipedia.org/wiki/Pyramid_of_doom_(programming)

