---
title: "AST Transformations"
slug: "ast-transformations"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## @CompileStatic
Enables a code to be statically compiled. Its bytecode will be closer to Java's, thus having better performance, though some dynamic features won't be available.

    @groovy.transform.CompileStatic
    class ListMath {
        def countSize(List<String> strings) {
            strings.collect { it.size() }.sum()
        }
    }
    
    
    assert new ListMath().countSize(["a", "bb", "ccc"]) == 6

