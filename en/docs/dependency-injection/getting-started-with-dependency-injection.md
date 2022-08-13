---
title: "Getting started with dependency-injection"
slug: "getting-started-with-dependency-injection"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## What is a basic example of dependency injection?
Here is a class (Dog) creating its own dependency (Food):

    class Dog {
        public Dog() {
            var food = new Food();

            this.eat(food);
        }
    }

Here is the same class being injected with its dependency using constructor injection:

    class Dog {
        public Dog(Food food) {
            this.eat(food);
        }
    }

