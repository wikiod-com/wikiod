---
title: "Equality and other comparison predicates"
slug: "equality-and-other-comparison-predicates"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## The difference between EQ and EQL
1. `EQ` checks if two values have the same address of memory: in other words, it checks if the two values are are actually the *same*, *identical* object. So, it is can be considered the identity test, and should be applied *only* to structures: conses, arrays, structures, objects, typically to see if you are dealing in fact with the same object “reached” through different paths, or aliased through different variables.

2. `EQL` checks if two structures are the same object (like `EQ`) *or* if they are the same non-structured values (that is, the same numeric values for numbers of the same type or the character values). Since it includes the `EQ` operator and can be used also on non-structured values, is the most important and most commonly used operator, and almost all the primitive functions that require an equality comparison, like `MEMBER`, *use by default this operator*.

So, it is always true that `(EQ X Y)` implies `(EQL X Y)`, while the viceversa does not hold.

A few examples can clear the difference between the two operators:

    (eq 'a 'a)
    T ;; => since two s-expressions (QUOTE A) are “internalized” as the same symbol by the reader.
    (eq (list 'a) (list 'a))
    NIL ;; => here two lists are generated as different objects in memory
    (let* ((l1 (list 'a))
           (l2 l1))
      (eq l1 l2))
    T ;; => here there is only one list which is accessed through two different variables
    (eq 1 1)
    ?? ;; it depends on the implementation: it could be either T or NIL if integers are “boxed”
    (eq #\a #\a)
    ?? ;; it depends on the implementation, like for numbers
    (eq 2d0 2d0)
    ?? ;; => dependes on the implementation, but usually is NIL, since numbers in double 
       ;;    precision are treated as structures in many implementations
    (let ((a1 2d0)
          (a2 2d0))
      (eq a1 a2))
    ?? ;; => also in this case the results depends on the implementation

Let’s try the same examples with `EQL`:

    (eql 'a 'a)
    T ;; => equal because they are the same value, as for EQ
    (eql (list 'a) (list 'a))
    NIL ;; => different because they different objects in memory, as for EQ
    (let* ((l1 (list 'a))
           (l2 l1))
      (eql l1 l2))
    T ;; => as above
    (eql 1 1)
    T ;; they are the same number, even if integers are “boxed”
    (eql #\a #\a)
    T ;; they are the same character
    (eql 2d0 2d0)
    T ;; => they are the same number, even if numbers in double precision are treated as
       ;;   structures in many implementations
    (let ((a1 2d0)
          (a2 2d0))
      (eql a1 a2))
    T ;; => as before
    (eql 2 2.0)
    NIL;; => since the two values are of a different numeric type

From the examples we can see why the `EQL` operator should be used to portably check for “sameness” for all the values, structured and non-structured, and why actually many experts advise against the use of `EQ` in general.

## Structural equality with EQUAL, EQUALP, TREE-EQUAL
These three operators implement structural equivalence, that is they check if different, complex objects have equivalent structure with equivalent component.

`EQUAL` behaves like `EQL` for non-structured data, while for structures built by conses (lists and trees), and the two special types of arrays, strings and bit vectors, it performs *structural equivalence*, returning true on two structures that are isomorphic and whose elementary components are correspondingly equal by `EQUAL`. For instance:

    (equal (list 1 (cons 2 3)) (list 1 (cons 2 (+ 2 1))))
    T ;; => since the two arguments are both equal to (1 (2 . 3))
    (equal "ABC" "ABC")
    T ;; => equality on strings
    (equal "Abc" "ABC")
    NIL ;; => case sensitive equality on strings
    (equal '(1 . "ABC") '(1 . "ABC"))
    T ;; => equal since it uses EQL on 1 and 1, and EQUAL on "ABC" and "ABC"
    (let* ((a (make-array 3 :initial-contents '(1 2 3)))
           (b (make-array 3 :initial-contents '(1 2 3)))
           (c a))
      (values (equal a b)
              (equal a c)))
    NIL ;; => the structural equivalence is not used for general arrays
    T   ;; => a and c are alias for the same object, so it is like EQL

`EQUALP` returns true on all cases in which `EQUAL` is true, but it uses also structural equivalence for arrays of any kind and dimension, for structures and for hash tables (but not for class instances!). Moreover, it uses case insensitive equivalence for strings. 

    (equalp "Abc" "ABC")
    T ;; => case insensitive equality on strings
    (equalp (make-array 3 :initial-contents '(1 2 3))
            (make-array 3 :initial-contents (list 1 2 (+ 2 1))))
    T ;; => the structural equivalence is used also for any kind of arrays
    (let ((hash1 (make-hash-table))
          (hash2 (make-hash-table)))
          (setf (gethash 'key hash1) 42)
          (setf (gethash 'key hash2) 42)
          (print (equalp hash1 hash2))
          (setf (gethash 'another-key hash1) 84)
          (equalp hash1 hash2))   
    T   ;; => after the first two insertions, hash1 and hash2 have the same keys and values
    NIL ;; => after the third insertion, hash1 and hash2 have different keys and values
    (progn (defstruct s) (equalp (make-s) (make-s)))
    T ;; => the two values are structurally equal
    (progn (defclass c () ()) (equalp (make-instance 'c) (make-instance 'c)))
    NIL ;; => two structurally equivalent class instances returns NIL, it's up to the user to
        ;;    define an equality method for classes

Finally, `TREE-EQUAL` can be applied to structures built through `cons`and checks if they are isomorphic, like `EQUAL`, but leaving to the user the choice of which function to use to compare the leafs, i.e. the non-cons (atom) encountered, that can be of any other data type (by default, the test used on atom is `EQL`). For instance:

    (let ((l1 '(1 . ("A" . 2)))
          (l2 '(1 . ("A" . 2))))
      (tree-equal l1 l2 :test #'eql))
    NIL ;; => since (eql "A" "A") gives NIL
    (let ((l1 '(1 . ("A" . 2)))
          (l2 '(1 . ("A" . 2))))
      (tree-equal l1 l2 :test #'equal))
    T ;; since (equal "A" "A") gives T



## Comparison operators on numeric values
Numeric values can compared with `=` and the other numeric comparison operators (`/=`, `<`, `<=`, `>`, `>=`) that ignore the difference in the physical representation of the different types of numbers, and perform the comparison of the corresponding mathematical values. For instance:

    (= 42 42)
    T ;; => both number have the sme numeric type and the same value
    (= 1 1.0 1d0)
    T ;; => all the tree values represent the number 1, while for instance (eql 1 1d0) => NIL
      ;;    since it returns true only if the operands have the same numeric type
    (= 0.0 -0.0)
    T ;; => again, the value is the same, while (eql 0.0 -0.0) => NIL
    (= 3.0 #c(3.0 0.0))
    T ;; => a complex number with 0 imaginary part is equal to a real number
    (= 0.33333333 11184811/33554432)
    T ;; => since a float number is passed to RATIONAL before comparing it to another number
      ;; => and (RATIONAL 0.33333333) => 11184811/33554432 in 32-bit IEEE floats architectures
    (= 0.33333333 0.33333334)
    T ;; => since the result of RATIONAL on both numbers is equal in 32-bit IEEE floats architectures
    (= 0.33333333d0 0.33333334d0)
    NIL ;; => since the RATIONAL of the two numbers in double precision is different

From these examples, we can conclude that `=` is the operator that should normally be used to perform comparison between numeric values, unless we want to be strict on the fact that two numeric values are equal *only* if they have also the same numeric type, in which case `EQL` should be used.

     

## Comparison operators on characters and strings
Common Lisp has 12 type specific operators to compare two characters, 6 of them case sensitives and the others case insensitives. Their names have a simple pattern to make easy to remember their meaning:

| Case Sensitive | Case Insensitive |
| ------ | ------ |
| CHAR=   | CHAR-EQUAL   |
| CHAR/=   | CHAR-NOT-EQUAL   |
| CHAR<   | CHAR-LESSP   |
| CHAR<=   | CHAR-NOT-GREATERP   |
| CHAR>   | CHAR-GREATERP   |
| CHAR>=   | CHAR-NOT-LESSP   |

Two characters of the same case are in the same order as the corresponding codes obtained by `CHAR-CODE`, while for case insensitive comparisons the relative order between any two characters taken from the two ranges `a..z`, `A..Z` is implementation dependent. Examples:

    (char= #\a #\a)
    T ;; => the operands are the same character
    (char= #\a #\A)
    NIL ;; => case sensitive equality
    (CHAR-EQUAL #\a #\A)
    T ;; => case insensitive equality
    (char> #\b #\a)
    T ;; => since in all encodings (CHAR-CODE #\b) is always greater than (CHAR-CODE #\a)
    (char-greaterp #\b \#A)
    T ;; => since for case insensitive the ordering is such that A=a, B=b, and so on,
      ;;    and furthermore either 9<A or Z<0.
    (char> #\b #\A)
    ?? ;; => the result is implementation dependent

For strings the specific operators are `STRING=`, `STRING-EQUAL`, etc. with the word STRING instead of CHAR. Two strings are equal if they have the same number of characters *and* the correspending characters are equal according to `CHAR=` or `CHAR-EQUAL` if the test is case sensitive or not.

The ordering between strings is tje lexicographic order on the characters of the two strings. When an ordering comparison succeeds, the result is not `T`, but the index of the first character in which the two strings differ (which is equivalent to true, since every non-NIL object is a “generalized boolean” in Common Lisp).

An important thing is that *all* the comparison operators on string accept four keywords parameters: `start1`, `end1`, `start2`, `end2`, that can be used to restrict the comparison to only a contiguous run of characters inside one or both strings. The start index if omitted is 0, the end index is omitted is equal to the length of the string, and the comparison in performed on the substring starting at character with index `:start` and terminating with the character with index `:end - 1` included.

Finally, note that a string, even with a single character, cannot be compared to a character.

Examples:

    (string= "foo" "foo")
    T ;; => both strings have the same lenght and the characters are `CHAR=` in order
    (string= "Foo" "foo")
    NIL ;; => case sensitive comparison
    (string-equal "Foo" "foo")
    T ;; => case insensitive comparison
    (string= "foobar" "barfoo" :end1 3 :start2 3)
    T ;; => the comparison is perform on substrings
    (string< "fooarr" "foobar")
    3 ;; => the first string is lexicographically less than the second one and 
      ;;   the first character different in the two string has index 3
    (string< "foo" "foobar")
    3 ;; => the first string is a prefix of the second and the result is its length

As a special case, the string comparison operators can also be applied to symbols, and the comparison is made on the `SYMBOL-NAME` of the symbol. For instance:

    (string= 'a "A")
    T ;; since (SYMBOL-NAME 'a) is "A"
    (string-equal '|a| 'a)
    T ;; since the the symbol names are "a" and "A" respectively

As final note, `EQL` on characters is equivalent to `CHAR=`; `EQUAL` on strings is equivalent to `STRING=`, while `EQUALP` on strings is equivalent to `STRING-EQUAL`.



## Overwiew
In Common Lisp there are many different predicates for comparing values. They can be classified in the following categories:

1. Generic equality operators: EQ, EQL, EQUAL, EQUALP. They can be used for values of any type and return always a boolean value T or NIL.
2. Type specific equality operators: = and \= for numbers, CHAR= CHAR\= CHAR-EQUAL CHAR-NOT-EQUAL for characters, STRING= STRING\= STRING-EQUAL STRING-NOT-EQUAL for strings, TREE-EQUAL for conses.
3. Comparison operators for numeric values: <, <=, >, >=. They can be applied to any type of number and compare the mathematical value of the number, independently from the actual type.
4. Comparison operators for characters, like CHAR<, CHAR-LESSP, etc., that compare characters either in a case sensitive way or in a case insensitive way, according to an implementation depending order that preserves the natural alphabetical ordering.
5. Comparison operators for strings, like STRING<, STRING-LESSP, etc., that compare strings lexicographically,  either in a case sensitive way or in a case insensitive way, by using the character comparison operators.

