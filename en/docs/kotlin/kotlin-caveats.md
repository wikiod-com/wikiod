---
title: "Kotlin Caveats"
slug: "kotlin-caveats"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Calling a toString() on a nullable type
A thing to look out for when using the `toString` method in Kotlin is the handling of null in combination with the `String?`.

For example you want to get text from an `EditText` in Android.

You would have a piece of code like:

```Kotlin
// Incorrect:
val text = view.textField?.text.toString() ?: ""
```
You would expect that if the field did not exists the value would be empty string but in this case it is `"null"`.


```
// Correct:
val text = view.textField?.text?.toString() ?: ""
```

