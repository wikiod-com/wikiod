---
title: "Types of Lists"
slug: "types-of-lists"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Association Lists
Plain lists are useful for representing a sequence of elements, but sometimes it is more helpful to represent a kind of key to value mapping.  Common Lisp provides several ways to do this, including genuine hash tables (see [18.1 Hash Table Concepts][1]).  There are two primary ways or representing key to value mappings in Common Lisp:  [property lists][1] and [association lists][2].  This example describes association lists.

An association list, or *alist* is a "plain" list whose elements are dotted pairs in which the *car* of each pair is the key and the *cdr* of each pair is the associated value. For instance, 

    (defparameter *ages* (list (cons 'john 34) (cons 'mary 23) (cons 'tim 72)))

can be considered as an association list that maps symbols indicating a personal name with an integer indicating age.  It is possible to implement some retrieval functions using plain list functions, like **member**.  For instance, to retrieve the age of **john**, one could write

    (cdr (first (member 'mary *age* :key 'car)))
    ;=> 23

The **member** function returns the tail of the list beginning with with a cons cell whose *car* is **mary**, that is, **((mary . 23) (tim . 72))**, **first** returns the first element of that list, which is **(mary . 23)**, and **cdr** returns the right side of that pair, which is **23**.  While this is one way to access values in an association list, the purpose of a convention like association lists is to abstract away from the underlying representation (a list) and to provide higher-level functions for working with the data structure.

For association lists, the retrieval function is [**assoc**][3], which takes a key, an association list and optional testing keywords (key, test, test-not), and returns the pair for the corresponding key:

    (assoc 'tim *ages*)
    ;=> (tim . 72)

Since the result will always be a cons cell if an item is present, if **assoc** returns **nil**, then the item was not in the list:

    (assoc 'bob *ages*)
    ;=> nil

For updating values in an association list, **setf** may be used along with **cdr**.  For instance, when **john**'s birthday arrives and his age increases, either of the following could be performed:

    (setf (cdr (assoc 'john *ages*) 35)

    (incf (cdr (assoc 'john *ages*)))

**incf** works in this case because it is based on **setf**.

Association lists can also be used as a type of bidirectional map, since key to value mappings be retrieved based on the value by using the reversed assoc function, [**rassoc**][4].

In this example, the association list was created by using **list** and **cons** explicitly, but association lists can also be created by using [**pairlis**][5], which takes a list of keys and data and creates an association list based on them:

    (pairlis '(john mary tim) '(23 67 82))
    ;=> ((john . 23) (mary . 67) (tim . 82))

A single key and value pair can be added to an association list using [**acons**][6]:

    (acons 'john 23 '((mary . 67) (tim . 82)))
    ;=> ((john . 23) (mary . 67) (tim . 82))

The **assoc** function searches through the list from left to right, which means that is is possible to "mask" values in an association list without removing them from a list or updating any of the structure of the list, just by adding new elements to the beginning of the list.  The [**acons**][7] function is provided for this:

    (defvar *ages* (pairlis '(john mary tim) '(34 23 72)))
    
    (defvar *new-ages* (acons 'mary 29 *ages*))

    *new-ages*
    ;=> ((mary . 29) (john . 34) (mary . 23) (tim . 72))

And now, a lookup for **mary** will return the first entry:

    (assoc 'mary *new-ages*)
    ;=> 29


  [1]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_p.htm#property_list
  [2]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_a.htm#association_list
  [3]: http://www.lispworks.com/documentation/HyperSpec/Body/f_assocc.htm
  [4]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rassoc.htm#rassoc
  [5]: http://www.lispworks.com/documentation/lw50/CLHS/Body/f_pairli.htm
  [6]: http://www.lispworks.com/documentation/lw50/CLHS/Body/f_acons.htm#acons
  [7]: http://www.lispworks.com/documentation/lw50/CLHS/Body/f_acons.htm

## Plain Lists
Plain lists are the simplest type of list in Common Lisp.  They are an ordered sequence of elements.  They support basic operations like getting the first element of a list and the rest of a list in constant time, support random access in linear time.  

    (list 1 2 3)
    ;=> (1 2 3)

    (first (list 1 2 3))
    ;=> 1
    
    (rest (list 1 2 3))
    ;=> (2 3)

There are many functions that operate on "plain" lists, insofar as they only care about the elements of the list.  These include **find**, **mapcar**, and many others.  (Many of those functions will also work on [17.1 Sequence Concepts][1] for some of these functions.


  [1]: http://www.lispworks.com/documentation/HyperSpec/Body/17_a.htm

## Property Lists

Plain lists are useful for representing a sequence of elements, but sometimes it is more helpful to represent a kind of key to value mapping.  Common Lisp provides several ways to do this, including genuine hash tables (see [18.1 Hash Table Concepts][1]).  There are two primary ways or representing key to value mappings in Common Lisp:  [property lists][1] and [association lists][2].  This example describes property lists.

A property list, or *plist*, is a "plain" list in which alternating values are interpreted as keys and their associated values.  For instance:

    (defparameter *ages* (list 'john 34 'mary 23 'tim 72))

can be considered as a property list that maps symbols indicating a personal name with an integer indicating age.  It is possible to implement some retrieval functions using plain list functions, like **member**.  For instance, to retrieve the age of **john**, one could write

    (second (member 'mary *age*))
    ;=> 23

The **member** function returns the tail of the list beginning with **mary**, that is, **(mary 23 tim 72)**, and **second** returns the second element of that list, that is **23**.  While this is one way to access values in a property list, the purpose of a convention like property lists is to abstract away from the underlying representation (a list) and to provide higher-level functions for working with the data structure.

For property lists, the retrieval function is [**getf**][3], which takes the property list, a key (more commonly called an *indicator*), and an optional default value to return in case the property list does not contain a value for the key.

    (getf *ages* 'tim)
    ;=> 72

    (getf *ages* 'bob -1)
    ;=> -1

For updating values in a property list, **setf** may be used.  For instance, when **john**'s birthday arrives and his age increases, either of the following could be performed:

    (setf (getf *ages* 'john) 35)

    (incf (getf *ages* 'john))

**incf** works in this case because it is based on **setf**.

To look up multiple properties in a property list as once, use [**get-properties**][4].

The **getf** function searches through the list from left to right, which means that is is possible to "mask" values in a property list without removing them from a list or updating any of the structure of the list.  For instance,  using **list\***:

    (defvar *ages* '(john 34 mary 23 tim 72))
    
    (defvar *new-ages* (list* 'mary 29 *ages*))

    *new-ages*
    ;=> (mary 29 john 34 mary 23 tim 72)

And now, a lookup for **mary** will return the first entry:

    (getf *new-ages* 'mary)
    ;=> 29




  [1]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_p.htm#property_list
  [2]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_a.htm#association_list
  [3]: http://www.lispworks.com/documentation/lw50/CLHS/Body/f_getf.htm
  [4]: http://www.lispworks.com/documentation/lw50/CLHS/Body/f_get_pr.htm#get-properties

