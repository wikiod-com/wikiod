---
title: "Parsing logs with clojure"
slug: "parsing-logs-with-clojure"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Parse a line of log with record & regex
    (defrecord Logline [datetime action user id])
    (def pattern #"(\d{8}-\d{2}:\d{2}:\d{2}.\d{3})\|.*\|(\w*),(\w*),(\d*)")
    (defn parser [line] 
      (if-let [[_ dt a u i] (re-find pattern line)] 
              (->Logline dt a u i)))

Define a sample line : 

    (def sample "20170426-17:20:04.005|bip.com|1.0.0|alert|Update,john,12")

Test it : 

    (parser sample)

Result : 

    #user.Logline{:datetime "20170426-17:20:04.005", :action "Update", :user "john", :id "12"}



