---
title: "JavaScript Events"
slug: "javascript-events"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
- (goog.events dom-element event-type event-handler-function) ;;Creates a Google Closure event listener
- (.addEventListener dom-element load-event) ;;Creates normal JavaScript event listener. Can be browser specific.

All Closure event names can be found in their [documentation on the EventType enum][1].


  [1]: https://google.github.io/closure-library/api/goog.events.EventType.html

## Adding Event to Button Using Closure Library
    (ns so-doc.events
      (:require
       [goog.dom :as dom]
       [goog.events :as events]))
    
    
    (defn handle-click [event] ;an event object is passed to all events
      (js/alert "button pressed"))
    
    (events/listen
     (dom/getElement "button"); This is the dom element the event comes from
     (.-CLICK events/EventType); This is a string or array of strings with the event names. 
    ;;All event names can be found in the EventType enum
     handle-click ;function that should handle the event
     )

Google Closure does not support page-load events, and considers them to be not idiomatic. They recommend inserting scripts inline as soon as the content they to access has loaded.

## Using JavaScript Interop
    (ns so-doc.events)
    
    (enable-console-print!)
    
    (defn click-event []
      (println "Button clicked"))
    
    (defn load-event []
      (println "Page loaded!")
      (.addEventListener (.getElementById js/document "btn") "click" click-event false))
    
    (.addEventListener js/window "load" load-event false)

Like normal Javascript, this method requires browser specific handling. This will not work in Internet Explorer for instance.

Unlike Google Closure, JavaScript easily supports page-load events.

