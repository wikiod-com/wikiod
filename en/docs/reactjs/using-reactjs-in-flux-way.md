---
title: "Using ReactJS in Flux way"
slug: "using-reactjs-in-flux-way"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

It comes very handy to use Flux approach, when your application with ReactJS on frontend is planned to grow, because of limited structures and a little bit of new code to make state changes in runtime more easing.

**Flux** is the application architecture that Facebook uses for building client-side web applications. It complements React's composable view components by utilizing a unidirectional data flow. It's more of a pattern rather than a formal framework, and you can start using Flux immediately without a lot of new code.

Flux applications have three major parts: *the dispatcher*, *the stores*, and *the views* (React components). These should not be confused with Model-View-Controller. Controllers do exist in a Flux application, but they are controller-views — views often found at the top of the hierarchy that retrieve data from the stores and pass this data down to their children. Additionally, action creators — dispatcher helper methods — are used to support a semantic API that describes all changes that are possible in the application. It can be useful to think of them as a fourth part of the Flux update cycle.

**Flux eschews MVC** in favor of a unidirectional data flow. When a user interacts with a React view, the view propagates an action through a central dispatcher, to the various stores that hold the application's data and business logic, which updates all of the views that are affected. This works especially well with React's declarative programming style, which allows the store to send updates without specifying how to transition views between states.

## Data Flow
> This is outline of comprehensive [Overview](https://facebook.github.io/flux/docs/overview.html).

Flux pattern assumes the use of unidirectional data flow.

 1. **Action** — simple object describing action `type` and other input data.

 2. **Dispatcher** — single action receiver and callbacks controller. Imagine it is central hub of your application.

 3. **Store** — contains the application state and logic. It registers callback in dispatcher and emits event to view when change to the data layer has occured.

 4. **View** — React component that receives change event and data from store. It causes re-rendering when something is changed.

    > As of Flux data flow, views may also **create actions** and pass them to dispatcher for user interactions.

## Reverted

To make it more clearer, we can start from the end.

 - Different React components (*views*) get data from different stores about made changes.

    > Few components may be called **controller-views**, cause they provide the glue code to get the data from the stores and to pass data down the chain of their descendants. Controller-views represent any significant section of the page.

 - *Stores* can be remarked as callbacks that compare action type and other input data for business logic of your application.

 - *Dispatcher* is common actions receiver and callbacks container.

 - *Actions* are nothing than simple objects with required `type` property.

    > Formerly, you'll want to use constants for action types and helper methods (called **action creators**).

