---
title: "For Loop"
slug: "for-loop"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## NgFor - Markup For Loop
The **NgFor** directive instantiates a template once per item from an iterable. The context for each instantiated template inherits from the outer context with the given loop variable set to the current item from the iterable.

To customize the default tracking algorithm, NgFor supports **trackBy** option. **trackBy** takes a function which has two arguments: index and item. If **trackBy** is given, Angular tracks changes by the return value of the function.

    <li *ngFor="let item of items; let i = index; trackBy: trackByFn">
        {{i}} - {{item.name}}
    </li>

**Additional Options**: 
NgFor provides several exported values that can be aliased to local variables:

 - **index** will be set to the current loop iteration for each template context.
 - **first** will be set to a boolean value indicating whether the item is the first one in the iteration.
 - **last** will be set to a boolean value indicating whether the item is the last one in the iteration.
 - **even** will be set to a boolean value indicating whether this item has an even index.
 - **odd** will be set to a boolean value indicating whether this item has an odd index.

