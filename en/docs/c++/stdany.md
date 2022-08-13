---
title: "stdany"
slug: "stdany"
draft: false
images: []
weight: 9498
type: docs
toc: true
---

The class `std::any` provides a type-safe container to which we can put single values of any type.

## Basic usage
    std::any an_object{ std::string("hello world") };
    if (an_object.has_value()) {
        std::cout << std::any_cast<std::string>(an_object) << '\n';
    }
    
    try {
      std::any_cast<int>(an_object);
    } catch(std::bad_any_cast&) {
        std::cout << "Wrong type\n";
    }

    std::any_cast<std::string&>(an_object) = "42";
    std::cout << std::any_cast<std::string>(an_object) << '\n';

Output

    hello world
    Wrong type
    42

