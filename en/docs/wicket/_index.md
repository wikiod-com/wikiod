---
title : wicket Tutorial
slug : wicket-tutorial
weight : 9980
draft : false
images : []
type : docs
---

Wicket is a component-based framework, which puts it in stark contrast to some of the earlier solutions to the sometimes monotonous task of web programming. Like other frameworks, Wicket builds on top of Sun's servlet API.

However, unlike frameworks like Struts or Spring MVC, the developer using Wicket is mostly removed from the request/response nature that is inherent with the web and Servlets. Instead of building controllers that must service many users and threads simultaneously, taking in requests, returning responses, and never storing any state, the Wicket developer thinks in terms of stateful components. Instead of creating a controller or action class, he or she creates a page, places components on it, and defines how each component reacts to user input.

Wicket uses plain XHTML for templating. Each component is bound to a named element in the XHTML and becomes responsible for rendering that element in the final output. The page is simply the top-level containing component and is paired with exactly one XHTML template.

Each component is backed by its own model, which represents the state of the component. The framework does not have knowledge of how components interact with their models, which are treated as opaque objects automatically serialized and persisted between requests. More complex models, however, may be made detachable and provide hooks to arrange their own storage and restoration at the beginning and end of each request cycle.

In Wicket, all server side state is automatically managed. You should never directly use an HttpSession object or similar wrapper to store state. Instead, state is associated with components. Each server-side page component holds a nested hierarchy of stateful components, where each component’s model is, in the end, a POJO (Plain Old Java Object).

