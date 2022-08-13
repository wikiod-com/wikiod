---
title: "Representing Lines"
slug: "representing-lines"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

A geometric line is a straight line that extends to infinity in both directions. This distinguishes it from the segment or the ray, which end et some point, and also from the curve or polygon, which need not be straight.

There are different ways how a line may be presented. Each comes with its own benefits and drawbacks.

## Point and Direction
One can describe a line in arbitrary dimensions as

    X = A + t*D

where `A` and `D` are both vectors of suitably many dimensions. So in 2d this would be

    x = Ax + t*Dx
    y = Ay + t*Dy

Now as `t` assumes any real value, this equation will produce all points along the line. The representation is not unique, though: any point along the line may be used as starting point `A`, and any multiple of the vector `D` represents the same direction.

## Pair of Points
Given a pair of points `A` and `B` in a vector space of arbitrary dimension, one can describe the line between them as

    X = A + t*(B - A) = (1 - t)*A + t*B

so in 2d this would be

    x = Ax + t*(Bx - Ax) = (1 - t)*Ax + t*Bx
    y = Ay + t*(By - Ay) = (1 - t)*Ay + t*By

As `t` assumes any real value, this will produce all points along the line. The representation is not unique, as any pair of distinct points along the line will describe the same line. It is easy to switch between full line and line segment using this representation, since restricting `t` to the range `[0, 1]` will yield a line segment instead.

## Normal form / homogeneous equation
A line in the plane can be described as

    a*x + b*y + c = 0

This uses a three-element parameter vector `[a, b, c]` to describe the line. Sometimes the constant term `c` is moved to the right hand side of the equation instead. The representation is not unique, since the length of that vector is arbitrary (as long as it is not zero). Such a vector is called [homogeneous](https://en.wikipedia.org/wiki/Homogeneous_coordinates).

The vector `[a, b]` is perpendicular to the line, hence the name “[normal](https://en.wikipedia.org/wiki/Normal_(geometry)) form”. If the length of that vector is one, this is called the [Hesse normal form](https://en.wikipedia.org/wiki/Hesse_normal_form) which can be used to easily compute the distance of arbitrary points to the line in question. Even that doesn't make the representation fully unique, since one may negate all three parameters and obtain the same set of points satisfying the equation.

The concept does generalize to higher dimensions, but then it no longer describes a line, but instead a plane in 3d and a hyperplane in general.

## Simple Function
One can describe a line in the plane as

    y = a*x + b

so that the line is essentially controlled by two parameters `a, b`. For a given line, the choice of these parameters is unique. But vertical lines cannot be described like this.

