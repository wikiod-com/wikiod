---
title: "Regular Expressions"
slug: "regular-expressions"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Using with pattern matching to bind captured groups
The pattern matching library trivia provides a system `trivia.ppcre` that allows captured groups to be bound through pattern matching

    (trivia:match "John Doe"
      ((trivia.ppcre:ppcre "(.*)\\W+(.*)" first-name last-name)
       (list :first-name first-name :last-name last-name)))
    
    ;; => (:FIRST-NAME "John" :LAST-NAME "Doe")

* Note: the library Optima provides a similar facility in the system `optima.ppcre`

## Binding register groups with CL-PPCRE
`CL-PPCRE:REGISTER-GROUPS-BIND` will match a string against a regular expression, and if it matches, bind register groups in the regex to variables. If the string does not match, `NIL` is returned.

    (defun parse-date-string (date-string)
      (cl-ppcre:register-groups-bind
          (year month day)
          ("(\\d{4})-(\\d{2})-(\\d{2})" date-string)
        (list year month day)))
    
    (parse-date-string "2016-07-23") ;=> ("2016" "07" "23")
    (parse-date-string "foobar")     ;=> NIL
    (parse-date-string "2016-7-23")  ;=> NIL



