---
title: "Inspections"
slug: "inspections"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Intellij IDEA provides lots of code inspections, that can significantly simplify writing code.

Inspections parameters can be found in `Preferences` | `Editor` | `Inspections` section. By default, IDEA has lots of them enabled. And lots of inspections support auto-fixing options, that can be seen on pressing Alt + Enter.

To run inspections for your whole project (or some custom scope), you need to select `Analyze` | `Inspect code`.

## @NotNull / @Nullable inspections
[These inspections][1] are extremely useful for preventing `NullPointerException`s. By default they are disabled. You can find these inspections in `Inspections` preferences: `Java` | `Probable bugs` | `Constant conditions & exceptions` and `@NotNull/@Nullable problems`. There you can also configure your annotations. You can use [this manual][2] to add JetBrains annotations into your project.

For example, consider this methods:

[![enter image description here][3]][1]

If `getString` can't possibly return `null`, everything is fine. But if we enable our inspections and in some cases it can return null, we will immediately will see inspection triggered:

[![enter image description here][4]][2]

which says `'null' is returned by the method which is not declared as @Nullable`. And if we hit Alt + Enter, there will be an option `Annotate method as '@Nullable'`. If we hit Enter again, our code will look like that:

[![enter image description here][5]][3]

with inspection triggered on `length()` method saying `Method invocation 'length' may produce 'java.lang.NullPointerException'`. And if we go further and introduce the result of `getString()` method as a variable, after hitting Alt + Enter IDEA will suggest a few ways to fix this inspection:

[![enter image description here][6]][4]

This way you can inspect your code on-the-fly, and fix all potential `NullPointerException`s. If you want to check your whole project (or some random scope), you can use `Analyze` | `Inspect code`. Just make sure that your selected inspections profile has all necessary inspections enabled.


  [1]: https://www.jetbrains.com/help/idea/2016.3/nullable-and-notnull-annotations.html
  [2]: https://www.jetbrains.com/help/idea/2016.3/enabling-annotations.html
  [3]: https://i.stack.imgur.com/4993C.png
  [4]: https://i.stack.imgur.com/wJgSj.png
  [5]: https://i.stack.imgur.com/xTALc.png
  [6]: https://i.stack.imgur.com/71dsM.png

