---
title: "Controls (component)"
slug: "controls-component"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

Controllers are vital for immersing people into a VR application. The potential of VR is not met without them, namely controllers that provide six degrees of freedom (6DoF). With controllers, people can reach out and around the scene and interact with objects with their hands.

A-Frame provides components for controllers across the spectrum as supported by their respective WebVR browsers through the Gamepad Web API. There are components for Vive, Oculus Touch, Daydream, and GearVR controllers.



It's possible that you must enable gamepadextentions. You could do that using this steps:

* **On Chrome:** browse to `chrome://flags`
* **On Firefox:** browse to `about:config`
* **On IE:** Go to Group Policy Editor on your desktop
* **On Opera:** browse to `opera:config`
* **On Edge:** browse to `about:flags`

## Hand controls
<!-- if version <A-Frame 0.x> [gte 0.3] -->

A-Frame provides an implementation for supporting multiple types of 6DoF controllers (Vive, Oculus Touch) via the hand-controls component. The hand-controls component is primarily for 6DoF controllers since it’s geared towards room scale interactions such as grabbing objects. The hand-controls component works on top of both Vive and Oculus Touch controllers by:

- Setting both the vive-controls and oculus-touch-controls component

- Overriding the controller models with a simple hand model

- Mapping Vive-specific and Oculus Touch-specific events to hand events and gestures (e.g., gripdown and triggerdown to thumbup)

To add the hand-controls component:

    <a-entity hand-controls="left"></a-entity>
    <a-entity hand-controls="right"></a-entity>

Unfortunately, there is not yet a 3DoF controller component that abstracts well all the types of 3DoF controllers (i.e., Daydream, GearVR). We could create a custom controller that works with both controllers. It would be fairly easy to cover since 3DoF controllers do not offer much potential for interaction (i.e., only rotational tracking with a touchpad).

The hand-controls gives tracked hands (using a prescribed model) with animated gestures. hand-controls wraps the vive-controls and oculus-touch-controls components, which in turn wrap the tracked-controls component. The component gives extra events and handles hand animations and poses.

    <a-entity hand-controls="left"></a-entity>
    <a-entity hand-controls="right"></a-entity>

<!-- end version if -->

## Tracked controls
<!-- if version <A-Frame 0.x> [gte 0.3] -->

The tracked-controls component is A-Frame’s base controller component that provides the foundation for all of A-Frame’s controller components. The tracked-controls component:

- Grabs a Gamepad object from the Gamepad API given an ID or prefix.

- Applies pose (position and orientation) from the Gamepad API to read controller motion.

- Looks for changes in the Gamepad object’s button values to provide events when buttons are pressed or touched and when axis and touchpads are changed (i.e. `axischanged`, `buttonchanged`, `buttondown`, `buttonup`, `touchstart`, `touchend`).

All of A-Frame’s controller components build on top of the `tracked-controls` component by:

- Setting the tracked-controls component on the entity with the appropriate Gamepad ID (e.g., Oculus Touch (Right)). For example, the vive-controls component does 

      el.setAttribute('tracked-controls', {idPrefix: 'OpenVR'})

  `tracked-controls` will then connect to the appropriate Gamepad object to provide pose and events for the entity.

- Abstracting the events provided by tracked-controls. tracked-controls events are low-level; it’d difficult for us to tell which buttons were pressed based off of those events alone because we’d have to know the button mappings beforehand. Controller components can know the mappings beforehand for their respective controllers and provide more semantic events such as `triggerdown` or `xbuttonup`.

- Providing a model. `tracked-controls` alone does not provide any appearance. Controller components can provide a model that shows visual feedback, gestures, and animations when buttons are pressed or touched.
The controller components following are only activated if they detect the controller is found and seen as connected in the Gamepad API.

The `tracked-controls` component interfaces with tracked controllers. tracked-controls uses the Gamepad API to handle tracked controllers, and is abstracted by the hand-controls component as well as the vive-controls and oculus-touch-controls components. This component elects the appropriate controller, applies pose to the entity, observes buttons state and emits appropriate events.

Note that due to recent browser-specific changes, Vive controllers may be returned by the Gamepad API with id values of either _"OpenVR Gamepad"_ or _"OpenVR Controller"_, so using `idPrefix` for Vive / OpenVR controllers is recommended.

    <a-entity tracked-controls="controller: 0; idPrefix: OpenVR"></a-entity>

<!-- end version if -->

## 3Dof and 6Dof controllers
# Adding 3DoF Controllers

Controllers with 3 degrees of freedom (3DoF) are limited to rotational tracking. 3DoF controllers have no positional tracking meaning we can’t reach out nor move our hand back-and-forth or up-and-down. Having a controller with only 3DoF is like having a hand and wrist without an arm. Read more about degrees of freedom for VR.

The 3DoF controller components provide rotational tracking, a default model matching the real-life hardware, and events to abstract the button mappings. The controllers for Google Daydream and Samsung GearVR have 3DoF, and both support only one controller for one hand.

<!-- if version <A-frame 0.x> [gte 0.6] -->

## Daydream controllers

The daydream-controls component interfaces with the Google Daydream controllers. It wraps the tracked-controls component while adding button mappings, events, and a Daydream controller model that highlights the touched and/or pressed buttons (trackpad).

Match Daydream controller if present, regardless of hand. 

    <a-entity daydream-controls></a-entity>

Match Daydream controller if present and for specified hand.

    <a-entity daydream-controls="hand: left"></a-entity>
    <a-entity daydream-controls="hand: right"></a-entity>

## GearVR-controllers

The gearvr-controls component interfaces with the Samsung/Oculus Gear VR controllers. It wraps the tracked-controls component while adding button mappings, events, and a Gear VR controller model that highlights the touched and/or pressed buttons (trackpad, trigger).

    <!-- Match Gear VR controller if present, regardless of hand. -->
    <a-entity gearvr-controls></a-entity>
    <!-- Match Gear VR controller if present and for specified hand. -->
    <a-entity gearvr-controls="hand: left"></a-entity>
    <a-entity gearvr-controls="hand: right"></a-entity>

<!-- end version if -->

# Adding 6DoF Controllers

Controllers with 6 degrees of freedom (6DoF) have both rotational and positional tracking. Unlike controllers with 3DoF which are constrained to orientation, controllers with 6DoF are able to move freely in 3D space. 6DoF allows us to reach forward, behind our backs, move our hands across our body or close to our face. Having 6DoF is like reality where we have both hands and arms. 6DoF also applies to the headset and additional trackers (e.g., feet, props). Having 6DoF is a minimum for providing a truly immersive VR experience.

The 6DoF controller components provide full tracking, a default model matching the real-life hardware, and events to abstract the button mappings. HTC Vive and Oculus Rift with Touch provide 6DoF and controllers for both hands. HTC Vive also provides trackers for tracking additional objects in the real world into VR.

<!-- if version <A-Frame 0.x> [gte 0.3] -->

## Vive controllers

The vive-controls component interfaces with the HTC Vive controllers/wands. It wraps the tracked-controls component while adding button mappings, events, and a Vive controller model that highlights the pressed buttons (trigger, grip, menu, system) and trackpad.

    <a-entity vive-controls="hand: left"></a-entity>
    <a-entity vive-controls="hand: right"></a-entity>

<!-- end version if -->

<!-- if version <A-Frame 0.x> [gte 0.5] -->

## Oculus touch controllers

The oculus-touch-controls component interfaces with the Oculus Touch controllers. It wraps the tracked-controls component while adding button mappings, events, and a Touch controller model.

    <a-entity oculus-touch-controls="hand: left"></a-entity>
    <a-entity oculus-touch-controls="hand: right"></a-entity>

<!-- end version if -->

## Wasd controls
The wasd-controls component controls an entity with the <kbd>W</kbd>, <kbd>A</kbd>,
 <kbd>S</kbd> and <kbd>D</kbd> or arrow keyboard keys. The `wasd-controls` component is commonly attached to an entity with the camera component.

    <a-entity camera look-controls wasd-controls></a-entity>

For azerty keyboards, you could use <kbd>Z</kbd>, <kbd>Q</kbd>, <kbd>S</kbd> and <kbd>D</kbd> keys

## Look controls
<!-- language-all: html -->

The look-controls component:

- Rotates the entity when we rotate a VR head-mounted display (HMD).
- Rotates the entity when we click-drag mouse.
- Rotates the entity when we touch-drag the touchscreen.

The look-controls component is usually used alongside the camera component.

    <a-entity camera look-controls></a-entity>

# Caveats

If you want to create your own component for look controls, you will have to copy and paste the HMD-tracking bits into your component. In the future, we may have a system for people to more easily create their controls.

# Adding gaze to cursor

For this you need to add a cursor component to your camera

    <a-scene>
      <a-camera>
        <a-cursor></a-cursor>
        <!-- Or <a-entity cursor></a-entity> -->
      </a-camera>
    </a-scene>

More information you could find on the [cursor (component)][1] topic.


  [1]: https://www.wikiod.com/aframe

## Mouse control
Mouse controls are only supported outside the VR modus and could be use for games without a HMD. For more information about mouse controls, you could find in the [mouse cursor][1] example.

    <a-scene>
      <a-entity camera look-controls mouse-cursor>
    </a-scene>

  [1]: https://www.wikiod.com/aframe/cursors

