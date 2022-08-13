---
title: "Primitives"
slug: "primitives"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Primitives are just `<a-entity>`s under the hood. This means primitives have the same API as entities such as positioning, rotating, scaling, and attaching components.
A-Frame provides a handful of elements such as `<a-box>` or `<a-sky>` called primitives that wrap the entity-component pattern to make it appealing for beginners.
. Developers can create their own primitives as well.

### Under the Hood

Primitives act as a convenience layer (i.e., syntactic sugar) primarily for
newcomers. Keep in mind for now that primitives are `<a-entity>`s under the
hood that:

- Have a semantic name (e.g., `<a-box>`)
- Have a preset bundle of components with default values
- Map or proxy HTML attributes to [component][component] data

[assemblage]: http://vasir.net/blog/game-development/how-to-build-entity-component-system-in-javascript
[prefab]: http://docs.unity3d.com/Manual/Prefabs.html

Primitives are similar to [prefabs in Unity][prefab]. Some literature on the
entity-component-system pattern refer to them as [assemblages][assemblage].
They abstract the core entity-component API to:

- Pre-compose useful components together with prescribed defaults
- Act as a shorthand for complex-but-common types of entities (e.g., `<a-sky>`)
- Provide a familiar interface for beginners since A-Frame takes HTML in a new direction

Under the hood, this `<a-box>` primitive:

<!-- language: lang-html -->

    <a-box color="red" width="3"></a-box>


represents this entity-component form:

<!-- language: lang-html -->

    <a-entity geometry="primitive: box; width: 3" material="color: red"></a-entity>


`<a-box>` defaults the `geometry.primitive` property to `box`. And the
primitive maps the HTML `width` attribute to the underlying `geometry.width`
property as well as the HTML `color` attribute to the underlying
`material.color` property.

## Registering a Primitive
We can register our own primitives (i.e., register an element) using
`AFRAME.registerPrimitive(name, definition)`. `definition` is a JavaScript
object defining these properties:

| Property | Description |
| --- | --- |
| **defaultComponents** | Object specifying default components of the primitive. The keys are the components' names and the values are the components' default data.  |
| **mappings**          | Object specifying mapping between HTML attribute name and component property names. Whenever the HTML attribute name is updated, the primitive will update the corresponding component property. The component property is defined using a dot syntax `${componentName}.${propertyName}`. | 

For example, below is A-Frame's registration for the `<a-box>` primitive:

<!-- language: lang-js -->

    var extendDeep = AFRAME.utils.extendDeep;
    
    // The mesh mixin provides common material properties for creating mesh-based primitives.
    // This makes the material component a default component and maps all the base material properties.
    var meshMixin = AFRAME.primitives.getMeshMixin();
    
    AFRAME.registerPrimitive('a-box', extendDeep({}, meshMixin, {
      // Preset default components. These components and component properties will be attached to the entity out-of-the-box.
      defaultComponents: {
        geometry: {primitive: 'box'}
      },
    
      // Defined mappings from HTML attributes to component properties (using dots as delimiters).
      // If we set `depth="5"` in HTML, then the primitive will automatically set `geometry="depth: 5"`.
      mappings: {
        depth: 'geometry.depth',
        height: 'geometry.height',
        width: 'geometry.width'
      }
    }));

Which we can use then

<!-- language: lang-html -->

    <a-box depth="1.5" height="1.5" width="1.5"></a-box>

represents this entity-component form:

<!-- language: lang-html -->

    <a-entity geometry="primitive: box; depth: 1.5; height: 1.5; width:1.5;"></a-entity>

