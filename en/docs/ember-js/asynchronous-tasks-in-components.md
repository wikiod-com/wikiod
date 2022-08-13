---
title: "Asynchronous Tasks in Components"
slug: "asynchronous-tasks-in-components"
draft: false
images: []
weight: 9925
type: docs
toc: true
---

in `ember-concurrency` the extra setting of `error` is a work around to prevent thrown exceptions from bubbling up to Ember's `onerror` (since it is meant to be handled in the template). There is a [feature request](https://github.com/machty/ember-concurrency/issues/40) to handle this better.

## ember-concurrency task
An alternative community de facto standard is an addon called [ember-concurrency](http://ember-concurrency.com/) that makes a lot of the promise confusion go away.

It can be installed with the command `ember install ember-concurrency`.

## Pros ##

 - Intuitive reasoning of complex asynchronous code.
 - Offers a complete API for managing tasks.
 - Can be canceled.
 - Can be used directly in a component without the need of a proxy object.
 - Destructures promises inside the task function.
 - Can use JavaScript `try` / `catch` / `finally` blocks to manage asynchronous assignment, exceptions, and cleanup.
 - Tasks are automagically cancelled on `willDestroy` event, avoiding errors setting values on destroyed objects (e.g. after a timer)

## Cons ##

 - Not builtin – requires `ember install ember-concurrency`
 - Uses generator functions that can confuse developers used to promise chains.

## JavaScript ##

    import Ember from 'ember';
    import { task, timeout } from 'ember-concurrency';

    const { Component, set } = Ember;

    export default Component.extend({
      myTask: task(function * () {
        set(this, 'error', null);
        try {
          yield timeout(2000);
          return 'Foobar';
        } catch (err) {
          set(this, 'error', error);
        }
      }).keepLatest()
    });

## Template ##

    {{#if myTask.isIdle}}
      <button onclick={{perform myTask}}>
        Start Task
      </button>
    {{else}}
      Loading&hellip;
    {{/if}} 

    {{#if myTask.last.value}}
      Done. {{myTask.last.value}}
    {{/if}}

    {{#if error}}
      Something went wrong. {{error}}
    {{/if}}


## PromiseProxyMixin
Ember comes with a built in helper that will provide computed properties for the status of an asynchronous task.

## Pros ##

 - Built in – no need for an addon.
 - Can be managed in the life cycle of a component.
 - Provides convenient state properties that can drive the template logic.

## Cons ##

 - Must be wrapped in an `Ember.Object` and cannot be applied to an `Ember.Component` directly.
 - Creates a disconnect between the original promise chain and the destructing of the `content` value.
 - Is not very intuitive and can be difficult to reason with.
 - Cannot be cancelled.

## JavaScript ##

    import Ember from 'ember';

    const {
      Component, PromiseProxyMixin, get, set, computed,
      isPresent, run, RSVP: { Promise }
    } = Ember;

    const MyPromiseProxy = Ember.Object.extend(PromiseProxyMixin);

    export default Component({
      myProxy: computed('promise', {
        get() {
          const promise = get(this, 'promise');
          return isPresent(promise) ? MyPromiseProxy.create({promise}) : null;
        }
      }),

      actions: {
        performTask() {
          const fakeTask = new Promise((resolve) => {
            run.later(resolve, 'Foobar', 2000);
          });
          set(this, 'promise', fakeTask);
        }
      }
    });

## Template ##

    {{#if myProxy.isPending}}
      Loading&hellip;
    {{else}}
      <button onclick={{action "performTask"}}>
        Start Task
      </button>
    {{/if}}
    
    {{#if myProxy.isFulfilled}}
      Done. {{myProxy.content}}
    {{/if}}
    
    {{#if myProxy.isRejected}}
      Something went wrong. {{myProxy.reason}}
    {{/if}}



