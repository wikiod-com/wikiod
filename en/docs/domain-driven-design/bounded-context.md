---
title: "Bounded Context"
slug: "bounded-context"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

A bounded context is subset of a large domain model. In larger domains, multiple teams will be working on multiple areas of the solution and this strategic pattern helps to share the work among the teams. The idea is to divide the big domain model in more focused areas, giving the team the freedom to solve each subset of the domain in the best way, even, with totally independent code, database and services. 

For example, it's possible to have one project addressing customer targeting, a second project targeting sales management and a third project managing the warehouse. The idea is to have independent teams working in parallel on different areas of the domain. There must be some kind of agreement on how to communicate between each bounded context.

## Bounded Contexts for an academic domain
Let's say you have an academic system. The bounded contexts would be like this:

* Admission of new undergraduate students
* Distribution of students on classrooms considering a schedule of the courses and occupation, size and type of classrooms
* Management of courses, hierarchies and predecessors of courses
* Financial management of course payments
* Professor management
* Grade processing
* Tax processing

The main idea is that those bounded contexts can be managed independently, by different teams and different companies. They can even be deployed as microservices, communicating with each others through application events in a service bus. 

