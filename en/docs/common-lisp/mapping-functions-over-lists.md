---
title: "Mapping functions over lists"
slug: "mapping-functions-over-lists"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Overview
A set of [high-level mapping functions](http://www.lispworks.com/documentation/HyperSpec/Body/f_mapc_.htm) is available in Common Lisp, to apply a function to the elements of one or more lists. They differ in the way in which the function is applied to the lists and how the final result is obtained. The following table summarize the differences and shows for each of them the equivalent LOOP form. *f* is the function to be applied, that must have a number of arguments equal to the number of lists; “applied to car” means that it is applied in turn to the elements of the lists, “applied to cdr” means that it is applied in turn to the lists, their cdr, their cddr, etc.; the “returns” column shows if the global result is the obtained by listing the results, concatenating them (so they must be lists!), or simply used for side-effects (and in this case the first list is returned).

Function | Applied to | Returns | Equivalent LOOP 
---------| ------------ | ------------ | -----------------
(mapcar f l<sub>1</sub>… l<sub>n</sub>) | car | list of results | (loop for x<sub>1</sub> in l<sub>1</sub>… for x<sub>n</sub> in l<sub>n</sub> collect (f x<sub>1</sub>… x<sub>n</sub>))
(maplist f l<sub>1</sub>… l<sub>n</sub>) | cdr | list of results | (loop for x<sub>1</sub> on l<sub>1</sub>… for x<sub>n</sub> on l<sub>n</sub> collect (f x<sub>1</sub>… x<sub>n</sub>))
(mapcan  f  l<sub>1</sub>… l<sub>n</sub>) | car | concatenation of results | (loop for x<sub>1</sub> in l<sub>1</sub>… for x<sub>n</sub> in l<sub>n</sub> nconc (f x<sub>1</sub>… x<sub>n</sub>))
(mapcon f  l<sub>1</sub>… l<sub>n</sub>) | cdr | concatenation of results | (loop for x<sub>1</sub> on l<sub>1</sub>… for x<sub>n</sub> on l<sub>n</sub> nconc (f x<sub>1</sub>… x<sub>n</sub>))
(mapc f  l<sub>1</sub>… l<sub>n</sub>) | car | l<sub>1</sub> | (loop for x<sub>1</sub> in l<sub>1</sub>… for x<sub>n</sub> in l<sub>n</sub> do (f x<sub>1</sub>… x<sub>n</sub>) finally (return l<sub>1</sub>))
(mapl f l<sub>1</sub>… l<sub>n</sub>) | cdr | l<sub>1</sub> | (loop for x<sub>1</sub> on l<sub>1</sub>… for x<sub>n</sub> on l<sub>n</sub> do (f x<sub>1</sub>… x<sub>n</sub>) finally (return l<sub>1</sub>))

Note that, in all the cases, the lists can be of different lengths, and the application terminates when the shortest list is terminated.

Another couple of map functions are available: [`map`](http://www.lispworks.com/documentation/HyperSpec/Body/f_map.htm), that can be applied to sequences (strings, vectors, lists), analogous to `mapcar`, and that can return any type of sequence, specified as first argument, and [`map-into`](http://www.lispworks.com/documentation/HyperSpec/Body/f_map_in.htm), analogous to `map`, but that destructively modifies its first sequence argument to keep the results of the application of the function.




## Examples of MAPC and MAPL
MAPC:

    CL-USER> (mapc (lambda (x) (print (* x x))) '(1 2 3 4))
    
    1 
    4 
    9 
    16 
    (1 2 3 4)
    CL-USER> (let ((sum 0))
               (mapc (lambda (x y) (incf sum (* x y)))
                     '(1 2 3)
                     '(100 200 300))
               sum)
    1400  ; => (1 x 100) + (2 x 200) + (3 x 300)

MAPL:

    CL-USER> (mapl (lambda (list) (print (reduce #'+ list))) '(1 2 3 4 5))
    
    15 
    14 
    12 
    9 
    5 
    (1 2 3 4 5)



## Examples of MAPCAR
MAPCAR is the most used function of the family:

    CL-USER> (mapcar #'1+ '(1 2 3))
    (2 3 4)
    CL-USER> (mapcar #'cons '(1 2 3) '(a b c))
    ((1 . A) (2 . B) (3 . C))
    CL-USER> (mapcar (lambda (x y z) (+ (* x y) z)) 
                     '(1 2 3) 
                     '(10 20 30) 
                     '(100 200 300))
    (110 240 390)
    CL-USER> (let ((list '(a b c d e f g h i))) ; randomize this list
               (mapcar #'cdr
                       (sort (mapcar (lambda (x)
                                       (cons (random 100) x))
                                     list)
                             #'<=
                             :key #'car)))
    (I D A G B H E C F)

An idiomatic use of `mapcar` is to transpose a matrix represented as a list of lists:

    CL-USER> (defun transpose (list-of-lists)
               (apply #'mapcar #'list list-of-lists))
    ROTATE
    CL-USER> (transpose '((a b c) (d e f) (g h i)))
    ((A D G) (B E H) (C F I))

    ;  +---+---+---+               +---+---+---+
    ;  | A | B | C |               | A | D | G |
    ;  +---+---+---+               +---+---+---+
    ;  | D | E | F |    becomes    | B | E | H |
    ;  +---+---+---+               +---+---+---+
    ;  | G | H | I |               | C | F | I |
    ;  +---+---+---+               +---+---+---+

For an explanation, see [this answer](http://stackoverflow.com/a/3513158/2382734).


## Examples of MAPLIST
    CL-USER> (maplist (lambda (list) (cons 0 list)) '(1 2 3 4))
    ((0 1 2 3 4) (0 2 3 4) (0 3 4) (0 4))
    CL-USER> (maplist #'append
                      '(a b c d -)
                      '(1 2 3))
    ((A B C D - 1 2 3) (B C D - 2 3) (C D - 3))


## Examples of MAPCAN and MAPCON
MAPCAN:

    CL-USER> (mapcan #'reverse '((1 2 3) (a b c) (100 200 300)))
    (3 2 1 C B A 300 200 100)
    CL-USER> (defun from-to (min max)
               (loop for i from min to max collect i))
    FROM-TO
    CL-USER> (from-to 1 5)
    (1 2 3 4 5)
    CL-USER> (mapcan #'from-to '(1 2 3) '(5 5 5))
    (1 2 3 4 5 2 3 4 5 3 4 5)

One of the uses of MAPCAN is to create a result list without NIL values:

    CL-USER> (let ((l1 '(10 20 40)))
               (mapcan (lambda (x)
                         (if (member x l1)
                             (list x)
                           nil))
                       '(2 4 6 8 10 12 14 16 18 20
                         18 16 14 12 10 8 6 4 2)))
    (10 20 10)


MAPCON:

    CL-USER> (mapcon #'copy-list '(1 2 3))
    (1 2 3 2 3 3)
    CL-USER> (mapcon (lambda (l1 l2) (list (length l1) (length l2))) '(a b c d) '(d e f))
    (4 3 3 2 2 1)



