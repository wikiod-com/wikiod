---
title: "cursors"
slug: "cursors"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

The cursor component lets us interact with entities through clicking and gazing.

## Syntax
 - &lt;a-entity cursor="">&lt;/a-cursor>
 - &lt;a-cursor>&lt;/a-cursor>

## Parameters
| Property    | Description 
| ----------- | ------------------
| fuse        | Whether cursor is fuse-based. Default value: `false` on desktop, `true` on mobile
| fuseTimeout | How long to wait (in milliseconds) before triggering a fuse-based click event. Default value: 1500



The cursor is a specific application of the [raycaster component][1] in that it

- Listens for mouse clicks and gaze-based fuses
- Captures only the first intersected entity
- Emits special mouse and hover events (e.g., relating to mouse down/up/enter/leave)
- Has more states for hovering.

When the mouse clicks, the closest visible entity intersecting the cursor, if any, will emit a click event. Note the cursor component only applies the raycasting behavior. To provide a shape or appearance to the cursor, you could apply the geometry and material components.

----------

## Events

Event  |   Description
------ | -------------
click  |  Emitted on both cursor and intersected entity if a currently intersected entity is clicked (whether by mouse or by fuse).
fusing  |  Emitted on both cursor and intersected entity when fuse-based cursor starts counting down.
mousedown |   Emitted on both cursor and intersected entity (if any) on mousedown on the canvas element.
mouseenter |   Emitted on both cursor and intersected entity (if any) when cursor intersects with an entity.
mouseleave  |  Emitted on both cursor and intersected entity (if any) when cursor no longer intersects with previously intersected entity.
mouseup   | Emitted on both cursor and intersected entity (if any) on mouseup on the canvas element.

  [1]: https://www.wikiod.com/aframe/raycasters-component

## Default cursor
For example, we can create a ring-shaped cursor fixed to the center of the screen. To fix the cursor to the screen so the cursor is always present no matter where we look, we place it as a child of the active camera entity. We pull it in front of the camera by placing it on the negative Z axis. When the cursor clicks on the box, we can listen to the click event.

<!-- language: html -->

    <a-entity camera>
      <a-entity cursor="fuse: true; fuseTimeout: 500"
                position="0 0 -1"
                geometry="primitive: ring; radiusInner: 0.02; radiusOuter: 0.03"
                material="color: black; shader: flat">
      </a-entity>
    </a-entity>

    <a-entity id="box" cursor-listener geometry="primitive: box" material="color: blue">
    </a-entity>


<!-- language: js -->

    // Component to change to random color on click.
    AFRAME.registerComponent('cursor-listener', {
      init: function () {
        var COLORS = ['red', 'green', 'blue'];
        this.el.addEventListener('click', function (evt) {
          var randomIndex = Math.floor(Math.random() * COLORS.length);
          this.setAttribute('material', 'color', COLORS[randomIndex]);
          console.log('I was clicked at: ', evt.detail.intersection.point);
        });
      }
    });

## Gaze-Based Interactions with cursor Component
We’ll first go over gaze-based interactions. Gaze-based interactions rely on rotating our heads and looking at objects to interact with them. This type of interaction is for headsets without a controller. Even with a rotation-only controller (Daydream, GearVR), the interaction is still similar. Since A-Frame provides mouse-drag controls by default, gaze-based can sort of be used on desktop to preview the interaction by dragging the camera rotation.

To add gaze-based interaction, we need to add or implement a component. A-Frame comes with a cursor component that provides gaze-based interaction if attached to the camera:

1. Explicitly define <a-camera> entity. Previously, A-Frame was providing the default camera.
2. Add `a-cursor` entity as a child element underneath the camera entity.
3. Optionally, configure the raycaster used by the cursor.


    <a-scene>
      <a-camera>
        <a-cursor></a-cursor>
        <!-- Or <a-entity cursor></a-entity> -->
      </a-camera>
    </a-scene>

## a-cursor primitive
The cursor primitive is a reticle that allows for clicking and basic interactivity with a scene on devices that do not have a hand controller. The default appearance is a ring geometry. The cursor is usually placed as a child of the camera.

    <a-scene>
      <a-camera>
        <a-cursor></a-cursor>
      </a-camera>
      <a-box></a-box>
    </a-scene>

## Fuse-Based Cursor
Also known as gaze-based cursor. If we set the cursor to be fuse-based, the cursor will trigger a click if the user gazes at an entity for a set amount of time. Imagine a laser strapped to the user’s head, and the laser extends out into the scene. If the user stares at an entity long enough (i.e., the fuseTimeout), then the cursor will trigger a click.

The advantage of fuse-based interactions for VR is that it does not require extra input devices other than the headset. The fuse-based cursor is primarily intended for Google Cardboard applications. The disadvantage of fuse-based interactions is that it requires the user to turn their head a lot.

## Configuring the Cursor through the Raycaster Component
The cursor builds on top of and depends on the raycaster component. If we want to customize the raycasting pieces of the cursor, we can do by changing the raycaster component properties. Say we want set a max distance, check for intersections less frequently, and set which objects are clickable:

    <a-entity cursor raycaster="far: 20; interval: 1000; objects: .clickable"></a-entity>

## Adding Visual Feedback
To add visual feedback to the cursor to show when the cursor is clicking or fusing, we can use the animation system. When the cursor intersects the entity, it will emit an event, and the animation system will pick up event with the begin attribute:

    <a-entity cursor="fuse: true; fuseTimeout: 500"
              position="0 0 -1"
              geometry="primitive: ring"
              material="color: black; shader: flat">
      <a-animation begin="click" easing="ease-in" attribute="scale"
                   fill="backwards" from="0.1 0.1 0.1" to="1 1 1"></a-animation>
      <a-animation begin="cursor-fusing" easing="ease-in" attribute="scale"
                   fill="forwards" from="1 1 1" to="0.1 0.1 0.1"></a-animation>
    </a-entity>

## Mouse cursor
**Note:** For this example you need to add an external npm package.

If you want to use a mouse cursor of your computer, you need to add [`aframe-mouse-cursor-component`][1]. After if you must include the script using this code:

    import 'aframe';
    import 'aframe-mouse-cursor-component';

    // or this 

    require('aframe');
    require('aframe-mouse-cursor-component');

And on your camera you need to add the `mouse-cursor` component.

    <a-scene>
      <a-entity camera look-controls mouse-cursor>
    </a-scene>

  [1]: https://github.com/mayognaise/aframe-mouse-cursor-component


