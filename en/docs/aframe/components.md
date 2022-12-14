---
title: "Components"
slug: "components"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

In the entity-component-system pattern, a component is a reusable and modular chunk of data that we plug into an entity to add appearance, behavior, and/or functionality. 

In A-Frame, components modify entities which are 3D objects in the scene. We mix and compose components together to build complex objects. They let us encapsulate three.js and JavaScript code into modules that we can use declaratively from HTML. Components are roughly analogous to CSS.

### Definition Lifecycle Handler Methods

With the schema being the anatomy, the lifecycle methods are the physiology;
the schema defines the shape of the data, the lifecycle handler methods *use*
the data to modify the entity. The handlers will usually interact with the
**Entity API**.

### Overview of Methods

| Method       | Description |
|--------------|-------------|
| init         | Called once when the component is initialized. Used to set up initial state and instantiate variables.|
| update       | Called both when the component is initialized and whenever any of the component's properties is updated (e.g, via *setAttribute*). Used to modify the entity. |
| remove       | Called when the component is removed from the entity (e.g., via *removeAttribute*) or when the entity is detached from the scene. Used to undo all previous modifications to the entity. |
| tick         | Called on each render loop or tick of the scene. Used for continuous changes or checks. |
| play         | Called whenever the scene or entity plays to add any background or dynamic behavior. Also called once when the component is initialized. Used to start or resume behavior. |
| pause        | Called whenever the scene or entity pauses to remove any background or dynamic behavior. Also called when the component is removed from the entity or when the entity is detached from the scene. Used to pause behavior. |
| updateSchema | Called whenever any of the component's properties is updated. Can be used to dynamically modify the schema. |

### Component Prototype Properties

Within the methods, we have access to the component prototype via `this`:

| Property        | Description                                                                                                                                   |
|-----------------|-----------------------------------------------------------------------------------------------------------------------------------------------|
| this.data       | Parsed component properties computed from the schema default values, mixins, and the entity's attributes.                                     |
| this.el         | Reference to the [entity][entity] as an HTML element.                                                                                         |
| this.el.sceneEl | Reference to the [scene][scene] as an HTML element.                                                                                           |
| this.id         | If the component can have [multiple instances][multiple], the ID of the individual instance of the component (e.g., `foo` from `sound__foo`). |


# METHODS

> **.init ()**

`.init ()` is called once at the beginning of the component's lifecycle.
An entity can call the component's `init` handler:

- When the component is statically set on the entity in the HTML file and the page is loaded.
- When the component is set on an attached entity via `setAttribute`.
- When the component is set on an unattached entity, and the entity is
  then attached to the scene via `appendChild`.

The `init` handler is often used to:

- Set up initial state and variables
- Bind methods
- Attach event listeners

For example, a cursor component's `init` would set state variables, bind
methods, and add event listeners:

<!-- language: lang-js -->

    AFRAME.registerComponent('cursor', {
      // ...
      init: function () {
        // Set up initial state and variables.
        this.intersection = null;
        // Bind methods.
        this.onIntersection = AFRAME.utils.bind(this.onIntersection, this);
        // Attach event listener.
        this.el.addEventListener('raycaster-intersection', this.onIntersection);
      }
      // ...
    });

---

> **.update (oldData)**

`.update (oldData)` is called whenever the component's properties change,
including at the beginning of the component's lifecycle. An entity can call a
component's `update` handler:

- After `init ()` is called, at the beginning of component's lifecycle.
- When the component's properties are updated with `.setAttribute`.

The `update` handler is often used to:

- Do most of the work in making modifications to the entity, using `this.data`.
- Modify the entity whenever one or more component properties change.

Granular modifications to the entity can be done by [diffing][diff] the current
dataset (`this.data`) with the previous dataset before the update (`oldData`).

A-Frame calls `.update()` both at the beginning of a component's lifecycle and every
time a component's data changes (e.g., as a result of `setAttribute`). The
update handler often uses `this.data` to modify the entity. The update handler
has access to the previous state of a component's data via its first argument.
We can use the previous data of a component to tell exactly which
properties changed to do granular updates.

For example, the **visible** component's `update` sets the visibility of
the entity.

<!-- language: lang-js -->

    AFRAME.registerComponent('visible', {
      /**
       * this.el is the entity element.
       * this.el.object3D is the three.js object of the entity.
       * this.data is the component's property or properties.
       */
      update: function (oldData) {
        this.el.object3D.visible = this.data;
      }
      // ...
    });

---

> **.remove ()**

`.remove ()` is called whenever the component is detached from the entity. An
entity can call a component's `remove` handler:

- When the component is removed from the entity via `removeAttribute`.
- When the entity is detached from the scene (e.g., `removeChild`).

The `remove` handler is often used to:

- Remove, undo, or clean up all of the component's modifications to the entity.
- Detach event listeners.

For example, when the [light component][light] is removed, the light component
will remove the light object that it had previously set on the entity, thus
removing it from the scene.

<!-- language: lang-js -->

    AFRAME.registerComponent('light', {
      // ...
      remove: function () {
        this.el.removeObject3D('light');
      }
      // ...
    });

---

> **.tick (time, timeDelta)**

`.tick ()` is called on each tick or frame of the scene's render loop. The scene
will call a component's `tick` handler:

- On each frame of the render loop.
- On the order of 60 to 120 times per second.
- If the entity or scene is not paused (e.g., the Inspector is open).
- If the entity is still attached to the scene.

The `tick` handler is often used to:

- Continuously modify the entity on each frame or on an interval.
- Poll for conditions.

The `tick` handler is provided the global uptime of the scene in milliseconds
(`time`) and the time difference in milliseconds since the last frame
(`timeDelta`). These can be used for interpolation or to only run parts of the
`tick` handler on a set interval.

For example, the **tracked controls component** will progress
the controller's animations, update the controller's position and rotation, and
check for button presses.

<!-- language: lang-js -->

    AFRAME.registerComponent('tracked-controls', {
      // ...
      tick: function (time, timeDelta) {
        this.updateMeshAnimation();
        this.updatePose();
        this.updateButtons();
      }
      // ...
    });

---

> **.pause ()**

`.pause ()` is called when the entity or scene pauses. The entity can call a
component's `pause` handler:

- Before the component is removed, before the `remove` handler is called.
- When the entity is paused with `Entity.pause ()`.
- When the scene is paused with `Scene.pause ()` (e.g., the Inspector is opened).

The `pause` handler is often used to:

- Remove event listeners.
- Remove any chances of dynamic behavior.

For example, the **sound component** will pause the sound and remove an
event listener that would have played a sound on an event:

<!-- language: lang-js -->

    AFRAME.registerComponent('sound', {
      // ...
      pause: function () {
        this.pauseSound();
        this.removeEventListener();
      }
      // ...
    });

---

> **.play ()**

`.play ()` is called when the entity or scene resumes. The entity can call
a component's `play` handler:

- When the component is first attached, after the `update` handler is called.
- When the entity was paused but then resumed with `Entity.play ()`.
- When the scene was paused but then resumed with `Scene.play ()`.

The `play` handler is often use to:

- Add event listeners.

For example, the **sound component** will play the sound and update the
event listener that would play a sound on an event:

<!-- language: lang-js -->

    AFRAME.registerComponent('sound', {
      // ...
      play: function () {
        if (this.data.autoplay) { this.playSound(); }
        this.updateEventListener();
      }
      // ...
    });

---

> **.updateSchema (data)**

`.updateSchema ()`, if defined, is called on every update in order to check if
the schema needs to be dynamically modified.

The `updateSchema` handler is often used to:

- Dynamically update or extend the schema, usually depending on the value of a property.

For example, the **geometry component** checks if the `primitive`
property changed to determine whether to update the schema for a different
type of geometry:

<!-- language: lang-js -->

    AFRAME.registerComponent('geometry', {
      // ...
      updateSchema: (newData) {
        if (newData.primitive !== this.data.primitive) {
          this.extendSchema(GEOMETRIES[newData.primitive].schema);
        }
      }
      // ...
    });

---

# COMPONENT PROTOTYPE METHODS

> **.flushToDOM ()**

To save on CPU time on stringification, A-Frame will only update in debug mode the component???s serialized representation in the actual DOM. Calling flushToDOM () will manually serialize the component???s data and update the DOM:

<!-- language: lang-js -->

    document.querySelector('[geometry]').components.geometry.flushToDOM();



## Register a custom A-Frame component
# AFRAME.registerComponent (name, definition)

Register an A-Frame component. We must register components before we use them anywhere in **<a-scene>**. Meaning from an HTML file, components should come in order before **<a-scene>**.

 - **{string} name** - Component name. The component???s public API as represented through an HTML attribute name.
 - **{Object} definition** - Component definition. Contains schema and lifecycle handler methods.

## Registering component in **foo** in your js file e.g foo-component.js

<!-- language: lang-js -->

    AFRAME.registerComponent('foo', {
      schema: {},
      init: function () {},
      update: function () {},
      tick: function () {},
      remove: function () {},
      pause: function () {},
      play: function () {}
    });

## Usage of **foo** component in your scene

<!-- language: lang-html -->

    <html>
      <head>
        <script src="aframe.min.js"></script>
        <script src="foo-component.js"></script>
      </head>
      <body>
        <a-scene>
          <a-entity foo></a-entity>
        </a-scene>
      </body>
    </html>

## Component HTML Form
A component holds a bucket of data in the form of one or more component properties. Components use this data to modify entities. Consider an engine component, we might define properties such as horsepower or cylinders.

HTML attributes represent component names and the value of those attributes represent component data.

## Single-Property Component

If a component is a single-property component, meaning its data consists of a single value, then in HTML, the component value looks like a normal HTML attribute:

<!-- language: lang-html -->

    <!-- `position` is the name of the position component. -->
    <!-- `1 2 3` is the data of the position component. -->
    <a-entity position="1 2 3"></a-entity>

## Multi-Property Component

If a component is a multi-property component, meaning the data is consists of multiple properties and values, then in HTML, the component value resembles inline CSS styles:

<!-- language: lang-html -->

    <!-- `light` is the name of the light component. -->
    <!-- The `type` property of the light is set to `point`. -->
    <!-- The `color` property of the light is set to `crimson`. -->
    <a-entity light="type: point; color: crimson"></a-entity>



## Defining compnent schema object
The schema is an object that defines and describes the property or properties of the component. The schema???s keys are the names of the property, and the schema???s values define the types and values of the property (in case of a multi-property component):

**Defining schema in your component**

<!-- language: lang-js -->

    AFRAME.registerComponent('bar', {
      schema: {
        color: {default: '#FFF'},
        size: {type: 'int', default: 5}
      }
    }

**Override defined schema defaults**

<!-- language: lang-html -->

    <a-scene>
      <a-entity bar="color: red; size: 20"></a-entity>
    </a-scene>

### Single-Property Schema

A component can either be a single-property component (consisting of one
anonymous value) or a multi-property component (consisting of multiple named
values). A-Frame will infer whether a component is single-property vs.
multi-property based on the structure of the schema.

A single-property component's schema contains `type` and/or `default` keys, and
the schema's values are plain values rather than objects:

<!-- language: lang-js -->

    AFRAME.registerComponent('foo', {
      schema: {type: 'int', default: 5}
    });

<!-- language: lang-html -->

    <a-scene>
      <a-entity foo="20"></a-entity>
    </a-scene>


## A-Frame's component schema property types

Property types primarily define how the schema parses incoming data from the
DOM for each property. The parsed data will then be available via the `data`
property on the component's prototype. Below are A-Frame's built-in property
types:

| Property Type   | Description | Default Value |
| --------------- | ------------| ------------- |
| **array**           | Parses comma-separated values to array (i.e., `"1, 2, 3" to ['1', '2', '3'])`. | [] |
| **asset**           | For URLs pointing to general assets. Can parse URL out of a string in the form of `url(<url>)`. If the value is an element ID selector (e.g., `#texture`), this property type will call `getElementById` and `getAttribute('src')` to return a URL. The `asset` property type may or may not change to handle XHRs or return MediaElements directly (e.g., `<img>` elements). | '' |
| **audio**           | Same parsing as the `asset` property type. Will possibly be used by the A-Frame Inspector to present audio assets. | '' |
| **boolean**         | Parses string to boolean (i.e., `"false"` to false, everything else truthy). | false |
| **color**           | Currently doesn't do any parsing. Primarily used by the A-Frame Inspector to present a color picker. Also, it is required to use color type for color animations to work. | #FFF |
| **int**             | Calls `parseInt` (e.g., `"124.5"` to `124`). | 0 |
| **map**             | Same parsing as the `asset` property type. Will possibly be used bt the A-Frame Inspector to present texture assets. | '' |
| **model**           | Same parsing as the `asset` property type. Will possibly be used bt the A-Frame Inspector to present model assets.   | '' |
| **number**          | Calls `parseFloat` (e.g., `'124.5'` to `'124.5'`). | 0 |
| **selector**        | Calls `querySelector` (e.g., `"#box"` to `<a-entity id="box">`). | null |
| **selectorAll**     | Calls `querySelectorAll` and converts `NodeList` to `Array` (e.g., `".boxes"` to [<a-entity class="boxes", ...]), | null |
| **string**          | Doesn't do any parsing. | '' |
| **vec2**            | Parses two numbers into an `{x, y}` object (e.g., `1 -2` to `{x: 1, y: -2}`. | {x: 0, y: 0} |
| **vec3**            | Parses three numbers into an `{x, y, z}` object (e.g., `1 -2 3` to `{x: 1, y: -2, z: 3}`. | {x: 0, y: 0, z: 0} |
| **vec4**            | Parses four numbers into an `{x, y, z, w}` object (e.g., `1 -2 3 -4.5` to `{x: 1, y: -2, z: 3, w: -4.5}`. | {x: 0, y: 0, z: 0, w: 0} |

#### Property Type Inference

The schema will try to infer a property type given only a default value:

<!-- language: lang-js -->

    schema: {default: 10}  // type: "number"
    schema: {default: "foo"}  // type: "string"
    schema: {default: [1, 2, 3]}  // type: "array"

The schema will set a default value if not provided, given the property type:

<!-- language: lang-js -->

    schema: {type: 'number'}  // default: 0
    schema: {type: 'string'}  // default: ''
    schema: {type: 'vec3'}  // default: {x: 0, y: 0, z: 0}

#### Custom Property Type

We can also define our own property type or parser by providing a `parse`
function in place of a `type`:

<!-- language: lang-js -->

    schema: {
      // Parse slash-delimited string to an array 
      // (e.g., `foo="myProperty: a/b"` to `['a', 'b']`).
      myProperty: {
        default: [],
        parse: function (value) {
          return value.split('/');
        }
      }
    }



## Accessing a Component???s Members and Methods
A component???s members and methods can be accessed through the entity from the **.components** object. Look up the component from the entity???s map of components, and we???ll have access to the component???s internals. Consider this example component:

<!-- language: lang-js -->

    AFRAME.registerComponent('foo', {
      init: function () {
        this.bar = 'baz';
      },
      qux: function () {
        // ...
      }
    });

Let???s access the **bar** member and **qux** method:

<!-- language: lang-js -->

    var fooComponent = document.querySelector('[foo]').components.foo;
    console.log(fooComponent.bar);
    fooComponent.qux();

