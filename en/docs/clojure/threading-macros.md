---
title: "Threading Macros"
slug: "threading-macros"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Also known as arrow macros, threading macros convert nested function calls into a linear flow of function calls.

## Thread Last (->>)
This macro gives the output of a given line as the last argument of the next line function call. For e.g.

    (prn (str (+ 2 3)))
is same as

    (->> 2
        (+ 3)
        (str)
        (prn))

## Thread First (->)
This macro gives the output of a given line as the first argument of the next line function call. For e.g.

     (rename-keys (assoc {:a 1} :b 1) {:b :new-b}))

Can't understand anything, right? Lets try again, with ->

    (-> {:a 1}
        (assoc :b 1)                ;;(assoc map key val)
        (rename-keys {:b :new-b}))  ;;(rename-keys map key-newkey-map)


## Thread as (as->)
This is a more flexible alternative to thread first or thread last. It can be inserted anywhere in the list of parameters of the function. 

    (as-> [1 2] x
          (map #(+ 1 %) x)
          (if (> (count x) 2) "Large" "Small"))
      

