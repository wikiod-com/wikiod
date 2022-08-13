---
title: "Structs"
slug: "structs"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Defining a new Struct
To define the struct called Person with an integer type variable age, integer type variable height and float type variable ageXHeight:

    struct Person {
        int age;
        int height;
        float ageXHeight;
    }

Generally:

    struct structName {
        /+ values go here +/
    }

## Struct Constructors
In D we can use constructors to initialize structs just like a class. To define a construct for the struct declared in the previous example we can type:

    struct Person {
        this(int age, int height) {
            this.age = age;
            this.height = height;
            this.ageXHeight = cast(float)age * height;
        }
    }

    auto person = Person(18, 180);



