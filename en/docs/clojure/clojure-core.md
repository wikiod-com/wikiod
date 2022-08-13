---
title: "clojure.core"
slug: "clojurecore"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

This document gives various basic functionalities offered by clojure. There is no explicit dependency needed for this and comes as a part of org.clojure.

## Defining functions in clojure
    (defn x [a b] 
            (* a b)) ;; public function

    => (x 3 2) ;; 6
    => (x 0 9) ;; 0

    (defn- y [a b] 
            (+ a b)) ;; private function
    
    => (x (y 1 2) (y 2 3)) ;; 15



 

## Assoc - updating map/vector values in clojure
When applied on a map, returns a new map with new or updated key val pairs.    

It can be used to add new information in existing map.

    (def userData {:name "Bob" :userID 2 :country "US"})
    (assoc userData :age 27)            ;; { :name "Bob" :userID 2 :country "US" :age 27} 
It replaces old information value if existing key is supplied.

    (assoc userData :name "Fred")       ;; { :name "Fred" :userID 2 :country "US" } 
    (assoc userData :userID 3 :age 27)  ;; {:name "Bob" :userID 3 :country "US" :age 27} 

It can also be used on a vector for replacing value at the specified index.

    (assoc [3 5 6 7] 2 10)              ;; [3 5 10 7]
    (assoc [1 2 3 4] 6 6)               ;; java.lang.IndexOutOfBoundsException 

## Comparison Operators in Clojure
Comparisons are functions in clojure. What that means in `(2>1)` is `(> 2 1)` in clojure. Here are all the comparison operators in clojure.

 1. Greater Than

`(> 2 1)    ;; true` <br>
`(> 1 2)    ;; false`

 2. Less Than

`(< 2 1)    ;; false`

 3. Greater Than or Equal To

`(>= 2 1)    ;; true` <br>
`(>= 2 2)    ;; true` <br>
`(>= 1 2)    ;; false`

 4. Less Than or Equal To

`(<= 2 1)    ;; false` <br>
`(<= 2 2)    ;; true` <br>
`(<= 1 2)    ;; true`

 5. Equal To

`(= 2 2)    ;; true` <br>
`(= 2 10)   ;; false`

 6. Not Equal To

`(not= 2 2)    ;; false` <br>
`(not= 2 10)   ;; true`





## Dissoc - disassociating a key from a clojure map
This returns a map without the key-value pairs for the keys mentioned in the function argument. It can be used to remove information from existing map.

    (dissoc {:a 1 :b 2} :a) ;; {:b 2}

It can also be used for dissocing multiple keys as:

    (dissoc {:a 1 :b 2 :c 3} :a :b) ;; {:c 3}


