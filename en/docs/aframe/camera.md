---
title: "Camera"
slug: "camera"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

The camera component defines from which perspective the user views the scene. The camera is commonly paired with controls components that allow input devices to move and rotate the camera.

## Syntax
 - &lt;a-entity camera>&lt;/a-entity>
 - &lt;a-camera>&lt;/a-camera>

## Parameters
Property   | Description
---------- | -------------
active     | Whether the camera is the active camera in a scene with more than one camera.
far        | Camera frustum far clipping plane.
fov        | Field of view (in degrees).
near       | Camera frustum near clipping plane.
userHeight | How much height to add to the camera when not in VR mode. The default camera has this set to 1.6 (meters, to represent average eye level.).
zoom       | Zoom factor of the camera.

When not in VR mode, `userHeight` translates the camera up to approximate average height of human eye level. The injected camera has this set to 1.6 (meters). When entering VR, this height offset is _removed_ such that we used absolute position returned from the VR headset. The offset is convenient for experiences that work both in and out of VR, as well as making experiences look decent from a desktop screen as opposed to clipping the ground if the headset was resting on the ground.

When exiting VR, the camera will restore its rotation to its rotation before it entered VR. This is so when we exit VR, the rotation of the camera is back to normal for a desktop screen.

## Default camera
A camera situated at the average height of human eye level (1.6 meters or 1.75 yard or 5.25 feet).

    <a-entity camera="userHeight: 1.6" look-controls></a-entity>

## Changing the Active Camera
When the active property gets toggled, the component will notify the camera system to change the current camera used by the renderer:

    var secondCameraEl = document.querySelector('#second-camera');
    secondCameraEl.setAttribute('camera', 'active', true);

## Fixing Entities to the Camera
To fix entities onto the camera such that they stay within view no matter where the user looks, you can attach those entities as a child of the camera. Use cases might be a heads-up display (HUD).

    <a-entity camera look-controls>
      <a-entity geometry="primitive: plane; height: 0.2; width: 0.2" position="0 0 -1"
                material="color: gray; opacity: 0.5"></a-entity>
    </a-entity>

Note that you should use HUDs sparingly as they cause irritation and eye strain in VR. Consider integrating menus into the fabric of the world itself. If you do create a HUD, make sure that the HUD is more in the center of the field of view such that the user does not have to strain their eyes to read it.

## a-camera primitive
The camera primitive determines what the user sees. We can change the viewport by modifying the camera entity’s position and rotation.

Note that by default, the camera origin will be at 0 1.6 0 in desktop mode and 0 0 0 in VR mode. Read about the camera.userHeight property.

    <a-scene>
      <a-box></a-box>
      <a-camera></a-camera>
    </a-scene>

## Manually Positioning the Camera
To position the camera, set the position on a wrapper <a-entity>. Don’t set the position directly on the camera primitive because controls will quickly override the set position:

    <a-entity position="0 0 5">
      <a-camera></a-camera>
    </a-entity>

