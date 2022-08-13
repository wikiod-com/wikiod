---
title: "Getting started with geometry"
slug: "getting-started-with-geometry"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Questions to ask yourself
When investigating some geometric problem, there are a number of questions you might want to ask yourself to narrow down the scope of the question.

* How many dimensions are you dealing with? Is it 2d, 3d, a specific number of higher dimensions, or without respect to dimensionality?
* In case of a 2d geometry, is it planar geometry, or is it e.g. spherical geometry, as one would encounter when dealing with geographic coordinates?
* Are you looking for exact solutions, or are numeric approximations acceptable? Actually doing exact geometry becomes quickly difficult, so using floating-point approximations is quite common in practice.
* Do you need to interface with a specific kind of framework? If so, how do they describe geometric objects. For most objects there are *many* possible descriptions, and although one can usually convert between them, these conversions tend to come at a cost, so a solution more in line with the required representations may be more useful.

