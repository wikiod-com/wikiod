---
title: "Test Execution Order"
slug: "test-execution-order"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Syntax
- @FixMethodOrder // Runs test using default method sorter
- @FixMethodOrder(MethodSorters) // Runs test using MethodSorter associated with the MethodSorters enum.

## Default Order
Use the annotation -- `@FixMethodOrder(MethodSorters.DEFAULT)`. This runs all tests within the class in a deterministic and somewhat predictable order. The implementation hashes the method names and compares them. In the scenario of a tie, it sorts by lexicographical order. 

[Code Segment Below Taken from JUnit Github -- MethodSorter.java][1]

    public int compare(Method m1, Method m2) {
        int i1 = m1.getName().hashCode();
        int i2 = m2.getName().hashCode();
        if(i1 != i2) {
            return i1 < i2 ? -1 : 1;
        }
        return NAME_ASCENDING.compare(m1,m2);
    }

**Example**

    @FixMethodOrder(MethodSorters.DEFAULT)
    public class OrderedTest {
        @Test
        public void testA() {}

        @Test
        public void testB() {}

        @Test
        public void testC() {}
    }

Suppose hashes for `testA`, `testB`, and `testC` are 3, 2, and 1 respectively. Then the execution order is

 1. testC
 2. testB
 3. testA

Suppose hashes for all tests are the same. Since all hashes are the same, execution order is based on lexicographical order. The execution order is

 1. testA
 2. testB
 3. testC

  [1]: https://github.com/junit-team/junit4/blob/master/src/main/java/org/junit/internal/MethodSorter.java

## Lexicographical Order
Use the annotation `@FixMethodOrder` with the method sorter `MethodSorters.NAME_ASCENDING`. This will run all tests within the class in a deterministic and predictable order. The implementation compares the method names and in the case of a tie, it compares the methods' `toString()`.

[Code Segment Below Taken from JUnit Github -- MethodSorter.java][1]

    public int compare(Method m1, Method m2) {
        final int comparison = m1.getName().compareTo(m2.getName());
        if(comparison != 0) {
            return comparison;
        }
        return m1.toString().compareTo(m2.toString());
    }

**Example**

    @FixMethodOrder(MethodSorters.NAME_ASCENDING)
    public class OrderedTest {
        @Test
        public void testA() {}

        @Test
        public void testB() {}

        @Test
        public void testC() {}
    }

The execution order is

 1. testA
 2. testB
 3. testC

  [1]: https://github.com/junit-team/junit4/blob/master/src/main/java/org/junit/internal/MethodSorter.java

