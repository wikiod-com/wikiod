---
title: "Optimization in C++"
slug: "optimization-in-c++"
draft: false
images: []
weight: 9860
type: docs
toc: true
---

## Introduction to performance
C and C++ are well known as high-performance languages - largely due to the heavy amount of code customization, allowing a user to specify performance by choice of structure.

When optimizing it is important to benchmark relevant code and completely understand how the code will be used.

Common optimization mistakes include:
 - **Premature optimization:** Complex code may perform *worse* after optimization, wasting time and effort. First priority should be to write _correct_ and _maintainable_ code, rather than optimized code.
 - **Optimization for the wrong use case:** Adding overhead for the 1% might not be worth the slowdown for the other 99%
 - **Micro-optimization:** Compilers do this very efficiently and micro-optimization can even hurt the compilers ability to further optimize the code

Typical optimization goals are:
 - To do less work
 - To use more efficient algorithms/structures
 - To make better use of hardware

Optimized code can have negative side effects, including:
 - Higher memory usage
 - Complex code -being difficult to read or maintain
 - Compromised API and code design

## Empty Base Class Optimization


## Optimizing by executing less code
The most straightforward approach to optimizing is by executing less code. This approach usually gives a fixed speed-up without changing the time complexity of the code.

Even though this approach gives you a clear speedup, this will only give noticable improvements when the code is called a lot.

Removing useless code
---------------------

    void func(const A *a); // Some random function
    
    // useless memory allocation + deallocation for the instance
    auto a1 = std::make_unique<A>();
    func(a1.get()); 

    // making use of a stack object prevents 
    auto a2 = A{};
    func(&a2);

<!-- if version [gte C++14] -->
From C++14, compilers are allowed to optimize this code to remove the allocation and matching deallocation.
<!-- end version if -->

Doing code only once
--------------------
    std::map<std::string, std::unique_ptr<A>> lookup;
    // Slow insertion/lookup
    // Within this function, we will traverse twice through the map lookup an element
    // and even a thirth time when it wasn't in
    const A *lazyLookupSlow(const std::string &key) {
        if (lookup.find(key) != lookup.cend())
            lookup.emplace_back(key, std::make_unique<A>());
        return lookup[key].get();
    }

    // Within this function, we will have the same noticeable effect as the slow variant while going at double speed as we only traverse once through the code
    const A *lazyLookupSlow(const std::string &key) {
        auto &value = lookup[key];
        if (!value)
            value = std::make_unique<A>();
        return value.get();
    }

A similar approach to this optimization can be used to implement a stable version of `unique`

    std::vector<std::string> stableUnique(const std::vector<std::string> &v) {
        std::vector<std::string> result;
        std::set<std::string> checkUnique;
        for (const auto &s : v) {
            // As insert returns if the insertion was successful, we can deduce if the element was already in or not
            // This prevents an insertion, which will traverse through the map for every unique element
            // As a result we can almost gain 50% if v would not contain any duplicates
            if (checkUnique.insert(s).second)
                result.push_back(s);
        }
        return result;
    }

Preventing useless reallocating and copying/moving
-------------------------------------------
In the previous example, we already prevented lookups in the std::set, however the `std::vector` still contains a growing algorithm, in which it will have to realloc 
its storage. This can be prevented by first reserving for the right size.

    std::vector<std::string> stableUnique(const std::vector<std::string> &v) {
        std::vector<std::string> result;
        // By reserving 'result', we can ensure that no copying or moving will be done in the vector
        // as it will have capacity for the maximum number of elements we will be inserting
        // If we make the assumption that no allocation occurs for size zero
        // and allocating a large block of memory takes the same time as a small block of memory
        // this will never slow down the program
        // Side note: Compilers can even predict this and remove the checks the growing from the generated code
        result.reserve(v.size());
        std::set<std::string> checkUnique;
        for (const auto &s : v) {
            // See example above
            if (checkUnique.insert(s).second)
                result.push_back(s);
        }
        return result;
    }


## Using efficient containers
Optimizing by using the right data structures at the right time can change the time-complexity of the code.

    // This variant of stableUnique contains a complexity of N log(N)
    // N > number of elements in v
    // log(N) > insert complexity of std::set
    std::vector<std::string> stableUnique(const std::vector<std::string> &v) {
        std::vector<std::string> result;
        std::set<std::string> checkUnique;
        for (const auto &s : v) {
            // See Optimizing by executing less code
            if (checkUnique.insert(s).second)
                result.push_back(s);
        }
        return result;
    }

By using a container which uses a different implementation for storing its elements (hash container instead of tree), we can transform our implementation to complexity N.
As a side effect, we will call the comparison operator for std::string less, as it only has to be called when the inserted string should end up in the same bucket.

    // This variant of stableUnique contains a complexity of N
    // N > number of elements in v
    // 1 > insert complexity of std::unordered_set
    std::vector<std::string> stableUnique(const std::vector<std::string> &v) {
        std::vector<std::string> result;
        std::unordered_set<std::string> checkUnique;
        for (const auto &s : v) {
            // See Optimizing by executing less code
            if (checkUnique.insert(s).second)
                result.push_back(s);
        }
        return result;
    }

## Small Object Optimization


