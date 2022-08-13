---
title: "State Management with re-frame (httpsgithub.comDay8re-frame)"
slug: "state-management-with-re-frame-httpsgithubcomday8re-frame"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

It starts to become difficult when we think clojurescript as a functionally pure language, that holds state for it UI components. It is simply, not possible.

However, it is possible to separate out individual components as well as their states. We can do it by storing data/state in reagent/atom. But when there are lots of states and lots of dependencies, things quickly become confusing and we start to wish for a out of the box solution for our state management. This is where re-frame comes in.

## 1. Simple Dispatch event
We will look at a simple dispatch event with the example usage.

    (ns myapp.events                                                                                                                                           
      (:require [re-frame.core :refer [reg-event-db]]))                                                                                                                                                                                                                                                                                          
                                                                                                                                                                  
    (reg-event-db                                                                                                                                                 
     :enable-feature-toggle                                                                                                                                       
     (fn [db [_ _]]                                                                                                                                               
       (assoc-in db [:global :enable-feature-toggle] true)))                                                                                                      
                                     
This is creating a event called `:enable-feature-toggle` which will create a entry in db `{:global {:enable-feature-toggle true}}`. A *db* is like a global store for preserving the event outcomes. These outcomes can then be subscribed to, and used to modify the state of the element as we will see in the **Simple Subscribe Event** example. 

After creating the event we actually have to dispatch the event somewhere in our code/UI components to make some use of it. For example, if we want to do some action on click of a div, we can fire an on-click event.

    (ns myapp.components.page-header                                                                                                                           
      (:require [myapp.events]
                [re-frame.core :refer [dispatch]]))
    
    (defn cljs-header []
        [:div {:class "cljs-header"                                                                                                                               
               :on-click #(dispatch [:enable-feature-toggle])} "Click Me"])

As soon as we click on the div, the :enable-feature-toggle event will be fired and a new value will be set in the db.

