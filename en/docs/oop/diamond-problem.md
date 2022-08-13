---
title: "Diamond problem"
slug: "diamond-problem"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Diamond Problem - Example
Diamond problem is a common problem occurred in Object Oriented Programming, while using `multiple-inheritance`.

Consider the case where `class C`, is inherited from `class A` and `class B`. Suppose that both `class A` and `class B` have a method called `foo()`.

Then when we are calling the method `foo()`, compiler cannot identify the exact method we are trying to use
* `foo()` from `class A`
* `foo()` from `class B`

This is called the diamond problem basically. There are some variants of this problem. To avoid this, there are multiple approaches. __Java__ doesn't allow multiple inheritance. Hence the problem is avoided. But C++ is allowing multiple inheritance, therefore you must be careful to use of multiple inheritance.

