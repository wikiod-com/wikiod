---
title: "Conditionals"
slug: "conditionals"
draft: false
images: []
weight: 9923
type: docs
toc: true
---

Pug can conditionally run code based on variables (passed from your server or based in Pug itself). 

## Syntax
 - if (statement)

       // Pug code
 - else if (statement)

       // Pug code
 - else

       // Pug code
 - unless (statement)

       // Pug code

## Parameters
| Parameter | Details |
| --------- | ------- |
| if (statement)      | Evaluates `statement` to see if it returns true or false. The code nested underneath `if` will run only if `statement` returns true. |
| else if (statement) | Chained to an existing `if` or `else if` statement; it only runs if the previous statement evaluated to false. The code nested underneath the `else if` statement will run only if `statement` evaluates to true. |
| else                |  The code nested underneath the `else` statement will run only if all previous statements returned false. |
| unless (statement)  | The negation of `if (statement)`; the code nested underneath `if` will run only if `statement` returns false. It is the same as `if (!statement)`.

[Official PugJS documentation on conditionals](https://pugjs.org/language/conditionals.html)

## If/Else Statement in Pug
Conditionals in Pug can evaluate statements in a manner similar to JavaScript. You can evaluate variables created in Pug, or those passed to it by your route (`res.render`, `pug.renderFile`, etc).

**index.js**


    var authorized = true
    res.render("index", {
        authorized: authorized
    });
**index.pug**

    - var showLogin = false;
    if authorized && showLogin === true
        .welcome Welcome back to our website!
    else
        .login
            a(href="/login") Login

**index.pug output**

    <div class="login"><a href="/login">Login</a></div>

## If/Else Statement in Pug (with a dash)
You can choose to prepend an `if` or `else` operator with a dash, but it is not necessary. You will need to wrap the statement in parentheses, though (if you omit a dash, you do not need parentheses.)

    - var showLogin = false;
    - if (showLogin === true)
        .welcome Welcome back to our website!
    - else
        .login
            a(href="/login") Login

**index.pug output**

    <div class="login"><a href="/login">Login</a></div>

## Else If Statement
You can chain any number of `else if` statements to an existing `if` statement, to evaluate a sequence of statements.

**index.pug**

    - var page = 60;
    if page => 52
        h1 Lots of numbers!
    else if page > 26 && page < 52
        h1 A few numbers
    else
        h1 Not a lot of numbers

**index.pug output**

    <h1>Lots of numbers!</h1>

## Unless Operator
`unless` is the inverse operation of `if` in Pug. It is analogous to `if !(statement)`.

**index.pug**

    - var likesCookies = true;
    unless likesCookies === true
        h2 You don't like cookies :(
    else
        h2 You like cookies!

**index.pug output**

    <h1>You like cookies!</h1>

**Note**: `else unless` statements do not work with `unless`; you can chain an `else if` statement to an `unless` statement, but `else unless` does not work.

