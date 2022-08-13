---
title: "Computed Properties"
slug: "computed-properties"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

Data vs Computed Properties
-
The main use-case difference for the `data` and `computed` properties of a `Vue` instance is dependent on the potential state or probability of changing of the data. When deciding what category a certain object should be, these questions might help:

- Is this a constant value? (**data**)
- Does this have the possibility to change? (**computed**  or **data**)
- Is the value of it reliant on the value of other data? (**computed**)
- Does it need additional data or calculations to be complete before being used? (**computed**)
- Will the value only change under certain circumstances? (**data**)

## Basic Example
**Template**

    <div id="example">
      a={{ a }}, b={{ b }}
    </div>


**JavaScript**

    var vm = new Vue({
      el: '#example',
      data: {
        a: 1
      },
      computed: {
        // a computed getter
        b: function () {
          // `this` points to the vm instance
          return this.a + 1
        }
      }
    })

**Result**

    a=1, b=2

Here we have declared a computed property `b`. The function we provided will be used as the getter function for the property `vm.b`:

    console.log(vm.b) // -> 2
    vm.a = 2
    console.log(vm.b) // -> 3

The value of `vm.b` is always dependent on the value of `vm.a`.

You can data-bind to computed properties in templates just like a normal property. Vue is aware that `vm.b` depends on `vm.a`, so it will update any bindings that depends on `vm.b` when `vm.a` changes.

## Computed properties vs watch

**template**
<!-- language: lang-html -->
    <div id="demo">{{fullName}}</div>

**watch example**
<!-- language: lang-js -->    
    var vm = new Vue({
      el: '#demo',
      data: {
        firstName: 'Foo',
        lastName: 'Bar',
        fullName: 'Foo Bar'
      }
    })
    
    vm.$watch('firstName', function (val) {
      this.fullName = val + ' ' + this.lastName
    })
    
    vm.$watch('lastName', function (val) {
      this.fullName = this.firstName + ' ' + val
    })

**Computed example**
<!-- language: lang-js -->
    var vm = new Vue({
      el: '#demo',
      data: {
        firstName: 'Foo',
        lastName: 'Bar'
      },
      computed: {
        fullName: function () {
          return this.firstName + ' ' + this.lastName
        }
      }
    })

## Computed Setters
Computed properties will automatically be recomputed whenever any data on which the computation depends changes. However, if you need to manually change a computed property, Vue allows you to create a setter method to do this:

**Template** (from the basic example above):

    <div id="example">
      a={{ a }}, b={{ b }}
    </div>

**Javascript:**

    var vm = new Vue({
      el: '#example',
      data: {
        a: 1
      },
      computed: {
        b: {
          // getter
          get: function () {
            return this.a + 1
          },
          // setter
          set: function (newValue) {
            this.a = newValue - 1 
          }
        }
      }


You can now invoke either the getter or the setter:

    console.log(vm.b)       // -> 2
    vm.b = 4                // (setter) 
    console.log(vm.b)       // -> 4
    console.log(vm.a)       // -> 3

`vm.b = 4` will invoke the setter, and set this.a to 3; by extension, vm.b will evaluate to 4.

## Using computed setters for v-model
----

You might need a `v-model` on a computed property. Normally, the v-model won't update the computed property value.

The template:

    <div id="demo">
      <div class='inline-block card'>
        <div :class='{onlineMarker: true, online: status, offline: !status}'></div>  
        <p class='user-state'>User is {{ (status) ? 'online' : 'offline' }}</p>
      </div>
  
      <div class='margin-5'>
        <input type='checkbox' v-model='status'>Toggle status (This will show you as offline to others)
      </div>
    </div>

Styling:

    #demo {
      font-family: Helvetica;
      font-size: 12px;
    }
    .inline-block > * {
      display: inline-block;
    }
    .card {
      background: #ddd;
      padding:2px 10px;
      border-radius: 3px;
    }
    .onlineMarker {
      width: 10px;
      height: 10px;
      border-radius: 50%;
      transition: all 0.5s ease-out;
    }
    
    .online {
      background-color: #3C3;
    }
    
    .offline {
      background-color: #aaa;
    }
    
    .user-state {
      text-transform: uppercase;
      letter-spacing: 1px;
    }
    
    .margin-5 {
      margin: 5px;
    }

The component:

    var demo = new Vue({
        el: '#demo',
        data: {
            statusProxy: null
        },
        computed: {
            status: {
                get () {
                    return (this.statusProxy === null) ? true : this.statusProxy     
                }
            }
        }
    })

[fiddle][1]
Here you would see, clicking the radio button has no use at all, your status is still online.

    var demo = new Vue({
        el: '#demo',
        data: {
            statusProxy: null
        },
        computed: {
            status: {
                get () {
                    return (this.statusProxy === null) ? true : this.statusProxy     
                },
                set (val) {
                    this.statusProxy = val            
                }
            }
        }
    })

[fiddle][2]
And now you can see the toggle happens as the checkbox is checked/unchecked.


  [1]: http://jsfiddle.net/zrdn0yxr/
  [2]: http://jsfiddle.net/yyx990803/vjvMp/

