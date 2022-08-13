---
title : model-view-controller Tutorial
slug : model-view-controller-tutorial
weight : 9990
draft : false
images : []
type : docs
---

Model–view–controller (MVC) is an [architectural pattern](https://en.wikipedia.org/wiki/Architectural_pattern) in software design, [not a design pattern](http://stackoverflow.com/questions/4243187/whats-the-difference-between-design-patterns-and-architectural-patterns), which describes a way to structure our application and the responsibilities and interactions for each part in that structure. [<sup>\[1\]</sup>](http://code.tutsplus.com/tutorials/mvc-for-noobs--net-10488)

Historically, MVC was first described in 1979 by [Trygve Reenskaug](https://en.wikipedia.org/wiki/Trygve_Reenskaug) as a way to approach (desktop) GUI (graphical user interface) design. Reenskaug described his early Smalltalk and object oriented conceptual efforts as follows: [<sup>\[2\]</sup>](https://en.wikipedia.org/wiki/Trygve_Reenskaug)

> "MVC was conceived as a general solution to the problem of users
> controlling a large and complex data set. The hardest part was to hit
> upon good names for the different architectural components.
> Model-View-Editor was the first set. After long discussions,
> particularly with Adele Goldberg, we ended with the terms
> Model-View-Controller."

The wild popularization of MVC for web applications is due to its inclusion in two frameworks that became immensely popular: Struts and Ruby on Rails. These two environments marked the way for the hundreds of frameworks created later. [<sup>\[3\]</sup>](http://code.tutsplus.com/tutorials/mvc-for-noobs--net-10488)

**Advantages:**  
 1. It's immensely popular.
 2. It provides strong routing when used in conjunction with the Front Controller pattern. (web-based MVC)
 3. It achieves separation of concerns.
 4. It makes collaboration easier as there's less to be concerned with.
 5. It makes it easier to follow how complicated applications work, thereby reducing bugs when contributors contribute.

**Disadvantages:**
 1. Abstraction can overly-complicate relatively simple applications.
 2. The learning curve can be difficult as there's many concepts involved.
 3. Many frameworks describe themselves as MVC, yet differ in implementation, thus one developer will have different habits from the next.

