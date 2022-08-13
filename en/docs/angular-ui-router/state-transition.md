---
title: "State transition"
slug: "state-transition"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Reload current state
You can reload the current state using the `$state.reload` method from your controller

    $state.reload()

This is a shorthand for (code taken from the [official docs][1])

    $state.transitionTo($state.current, $stateParams, { 
      reload: true, inherit: false, notify: false 
    });

Running a reload on your state will also restart your controller/s and re-resolve all your resolved values.


  [1]: https://github.com/angular-ui/ui-router/wiki/Quick-Reference#statereload

## Use $state.go to transition between states
`$state.go` is shorthand method to `$state.transitionTo`

> $state.go(toState [, toParams] [, options])

This method automatically sets your options to `{ location: true, inherit: true, relative: $state.$current, notify: true }` (unless you override them) and allows you to transition with less code.

**Examples:**

Lets say we have an app with a 'main' state, with 2 child states: 'dashboard' and 'help', and 'dashboard' also has a child called 'about'.

Transition to another state

    $state.go("main.dashboard") // from anywhere to 'main.dashboard'

Transition to parent state

    $state.go("^") // from 'main.dashboard' to 'main'

You can also transit to another child of the parent state (sibling)

    $state.go("^.help") // from 'main.dashboard' to main.help

Placing a `.` will allow you to transition to child states

    $state.go(".about") // from 'main.dashboard' to 'main.dashboard.about'



## Use $state.transitionTo to transition between states

Use `$state.transitionTo` to go form one state to another. This is a low level method to transition and `$state.go` is the recommended way for most common use cases as it uses this method internally.

>$state.transitionTo(toState [, toParams] [, options])

*toState* - the state to transition to

*toParams* (optional) - a map of parameters to send the target state

*options* (optional) - the state transition options

**Examples:**

    $state.transitionTo("dashboard.history", {period: "week"}) 
    // transitions to the history child state with a state parameter

