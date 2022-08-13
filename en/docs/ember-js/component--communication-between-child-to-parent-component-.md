---
title: "Component - communication between child to parent component."
slug: "component---communication-between-child-to-parent-component"
draft: false
images: []
weight: 9936
type: docs
toc: true
---

## Syntax
- `(yield` -- Allows you to export items from a component
- `(hash` -- Allows you to export a hash or object, since this is required to call child components within the parent's block. The requirement is that there is a `.` for the component to be created
- `(component` -- Creates the child component which can take anything in the parent's context. The component can be curried, since it is only called when the user uses it, so add as many attributes as you need, and the user can add the rest.
- `(action` -- Creates an action based on a function or a string pointing to a function in the `actions` hash of the parent component in this case.

For creating components that interact with a parent component, composable components are the best option, although they require Ember 2.3+.

## Composable Components
Inside `parent-component.hbs`

    {{yield (hash
       child=(
         component 'child-component'
         onaction=(action 'parentAction')
       )
    )}}

Inside `parent-component.js`

    export default Ember.Component.extend({
      actions: {
        // We pass this action to the child to call at it's discretion
        parentAction(childData) {
          alert('Data from child-component: ' + childData); 
        }
      }
    });

Inside `child-component.js`

    export default Ember.Component.extend({
      // On click we call the action passed down to us from the parent
      click() {
        let data = this.get('data');
        this.get('onaction')(data); 
      }
    });

Inside `usage.hbs`

    {{#parent-component as |ui|}}
      {{#each model as |item|}}
        {{ui.child data=item}}
      {{/each}}
    {{/parent-component}}

