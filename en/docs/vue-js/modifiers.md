---
title: "Modifiers"
slug: "modifiers"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

There are some frequently used operations like `event.preventDefault()` or `event.stopPropagation()` inside event handlers. Although we can do this easily inside methods, it would be better if the methods can be purely about data logic rather than having to deal with DOM event details.

## Event Modifiers
Vue provides event modifiers for `v-on` by calling directive postfixes denoted by a dot.
* `.stop`
* `.prevent`
* `.capture`
* `.self`
* `.once`

For examples:
```
<!-- the click event's propagation will be stopped -->
<a v-on:click.stop="doThis"></a>

<!-- the submit event will no longer reload the page -->
<form v-on:submit.prevent="onSubmit"></form>

<!-- use capture mode when adding the event listener -->
<div v-on:click.capture="doThis">...</div>

<!-- only trigger handler if event.target is the element itself -->
<!-- i.e. not from a child element -->
<div v-on:click.self="doThat">...</div>
```

## Key Modifiers
When listening for keyboard events, we often need to check for common key codes.
Remembering all the keyCodes is a hassle, so Vue provides aliases for the most commonly used keys:
* `.enter`
* `.tab`
* `.delete` (captures both “Delete” and “Backspace” keys)
* `.esc`
* `.space`
* `.up`
* `.down`
* `.left`
* `.right`

For examples:
```
<input v-on:keyup.enter="submit">
```

## Input Modifiers
* `.trim`

If you want user input to be trimmed automatically, you can add the `trim` modifier to your `v-model` managed inputs:
```
<input v-model.trim="msg">
```
* `.number`

If you want user input to be automatically typecast as a number, you can do as follow:
```
<input v-model.number="age" type="number">
```
* `.lazy`

Generally, `v-model` syncs the input with the data after each input event, but you can add the `lazy` modifier to instead sync after change events:
```
<input v-model.lazy="msg" >
```

