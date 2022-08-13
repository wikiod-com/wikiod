---
title: "Role"
slug: "role"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

The `TypeFamilies` language extension allows the programmer to define type-level functions. What distinguishes type functions from non-GADT type constructors is that parameters of type functions can be non-parametric whereas parameters of type constructors are always parametric. This distinction is important to the correctness of the `GeneralizedNewTypeDeriving` extension. To explicate this distinction, roles are introduced in Haskell.

  

See also [`SafeNewtypeDeriving`][1].

[1]: https://ghc.haskell.org/trac/ghc/wiki/SafeRoles

## Nominal Role
[Haskell Wiki][1] has an example of a non-parametric parameter of a type function:

    type family Inspect x
    type instance Inspect Age = Int    
    type instance Inspect Int = Bool

Here `x` is non-parametric because to determine the outcome of applying `Inspect` to a type argument, the type function must inspect `x`. 

In this case, the role of `x` is nominal. We can declare the role explicitly with the `RoleAnnotations` extension:
    
    type role Inspect nominal

[1]: https://ghc.haskell.org/trac/ghc/wiki/Roles

## Representational Role

An example of a parametric parameter of a type function:

    data List a = Nil | Cons a (List a)

    type family DoNotInspect x
    type instance DoNotInspect x = List x

Here `x` is parametric because to determine the outcome of applying `DoNotInspect` to a type argument, the type function do not need to inspect `x`.

In this case, the role of x is representational. We can declare the role explicitly with the `RoleAnnotations` extension:
    
    type role DoNotInspect representational




## Phantom Role
A [phantom type parameter][1] has a phantom role. Phantom roles cannot be declared explicitly.

  [1]: https://www.wikiod.com/haskell/phantom-types

