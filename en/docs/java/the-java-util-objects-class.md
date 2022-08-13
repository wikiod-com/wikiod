---
title: "The java.util.Objects Class"
slug: "the-javautilobjects-class"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Basic use for object null check
# For null check in method

    Object nullableObject = methodReturnObject();
    if (Objects.isNull(nullableObject)) {
        return;
    }

# For not null check in method

    Object nullableObject = methodReturnObject();
    if (Objects.nonNull(nullableObject)) {
        return;
    }


## Objects.nonNull() method reference use in stream api
In the old fashion way for collection null check    

    List<Object> someObjects = methodGetList();
    for (Object obj : someObjects) {
        if (obj == null) {
            continue;
        }
        doSomething(obj);
    }

With the `Objects.nonNull` method and Java8 Stream API, we can do the above in this way:

    List<Object> someObjects = methodGetList();
    someObjects.stream()
               .filter(Objects::nonNull)
               .forEach(this::doSomething);

