---
title: "Annotations"
slug: "annotations"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## Declaring an annotation
Annotations are means of attaching metadata to code. To declare an annotation, put the 
annotation
 modifier in front of a
class:
    
    annotation class Strippable

Annotations can have meta-anotations:
    
        @Target(AnnotationTarget.CLASS, AnnotationTarget.FUNCTION, AnnotationTarget.VALUE_PARAMETER, AnnotationTarget.EXPRESSION)
        annotation class Strippable

Annotations, like other classes, can have constructors:
    
    annotation class Strippable(val importanceValue: Int)

But unlike other classes, is limited to the following types:

 - types that correspond to Java primitive types (Int, Long etc.);
 - strings
 - classes ( Foo:: class)
 - enums
 - other annotations
 - arrays of the types listed above



## Meta-annotations
When declaring an annotation, meta-info can be included using the following meta-annotations:

- `@Target`: specifies the possible kinds of elements which can be annotated with the annotation (classes, functions, properties, expressions etc.)

- `@Retention` specifies whether the annotation is stored in the compiled class files and whether it's visible through reflection at runtime (by default, both are true.)

- `@Repeatable` allows using the same annotation on a single element multiple times.

- `@MustBeDocumented` specifies that the annotation is part of the public API and should be included in the class or method signature shown in the generated API documentation.

Example:

    @Target(AnnotationTarget.CLASS, AnnotationTarget.FUNCTION,
            AnnotationTarget.VALUE_PARAMETER, AnnotationTarget.EXPRESSION)
    @Retention(AnnotationRetention.SOURCE)
    @MustBeDocumented
    annotation class Fancy

