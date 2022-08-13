---
title: "Scene <a-scene>"
slug: "scene-a-scene"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

A scene is represented by the `<a-scene>` element. The scene is the global root object, and all entities are contained within the scene.

The scene inherits from the Entity class so it inherits all of its properties, its methods, the ability to attach components, and the behavior to wait for all of its child nodes (e.g., `<a-assets>` and `<a-entity>`) to load before kicking off the render loop.

## Parameters
| Parameter | Details |
| ------ | ------ |
| **behaviors**   | Array of components with tick methods that will be run on every frame. |
| **camera** | Active three.js camera. |
| **canvas** | Reference to the canvas element. |
| **isMobile** | Whether or not environment is detected to be mobile. |
| **object3D** | THREE.Scene object. |
| **renderer** | Active THREE.WebGLRenderer. |
| **renderStarted** | Whether scene is rendering. |
| **effect** | Renderer for VR created by passing active renderer into THREE.VREffect. |
| **systems** | Instantiated systems. |
| **time** | Global uptime of scene in seconds. |

# METHODS

| Name    | Description                                                                                                            |
|---------|------------------------------------------------------------------------------------------------------------------------|
| enterVR | Switch to stereo render and push content to the headset. Needs to be called within a user-generated event handler like `click`. the first time a page enters VR. |
| exitVR  | Switch to mono renderer and stops presenting content on the headset. |
| reload  | Revert the scene to its original state. |

# EVENTS

| Name         | Description                         |
|--------------|-------------------------------------|
| enter-vr     | User has entered VR and headset started presenting content. |
| exit-vr      | User has exited VR and headset stopped presenting content.  |
| loaded       | All nodes have loaded. |
| renderstart | Render loop has started. |

## Attaching Scene Components
Components can be attached to the scene as well as entities. A-Frame ships with a few components to configure the scene:

Component          | Details
------------------ | --------------
embedded           | Remove fullscreen styles from the canvas.
fog                | Add fog.
keyboard-shortcuts | Toggle keyboard shortcuts.
inspector          | Inject the A-Frame Inspector.
stats              | Toggle performance stats.
vr-mode-ui         | Toggle UI for entering and exiting VR.
debug              | Enables component-to-DOM serialization.



## Using embedded scenes
If you want to use WebVR content mixed with HTML content, for example when you're making a extended showcase key content, you could use the `embedded` tag. When you're using this, it's possible to look around inside 360° content using the gyroscope of your smartphone or click and drag on computer.

    <script src="https://aframe.io/releases/0.5.0/aframe.min.js"></script>
    <div class="vrcontent">
      <a-scene embedded>
        <a-assets>
          <img id="sky" src="https://c1.staticflickr.com/5/4248/34705881091_37b5cf2d28_o.jpg" alt="" />
        </a-assets>
    
        <a-sky src="#sky"></a-sky>
      </a-scene>
    </div>
    
    <div class="overlay">
      <button class="calltoaction">Click me!</button>
    </div>
    
    <div class="content">
      <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit. Deleniti animi aliquid architecto quibusdam ipsum, debitis dolor mollitia. Quidem, cumque quos porro doloribus iure dolore illum, qui rem asperiores unde laboriosam.Dolorum tempora quam eveniet ea recusandae deserunt, velit similique. Cum sunt rerum beatae officiis qui sed molestiae et ullam quasi, harum maxime vel, aspernatur quidem molestias. Provident quae illo harum?Sunt expedita, repellat saepe vel accusamus odio. Alias, obcaecati harum earum inventore asperiores quaerat, sit autem nostrum. Sunt illo numquam, temporibus pariatur optio nam, expedita necessitatibus aliquid nemo maxime nisi. Praesentium corporis, ea sunt asperiores, recusandae animi, rem doloribus, possimus cum laudantium libero. Maiores a, iusto aspernatur reiciendis ratione sunt nisi, rem, quasi temporibus ullam non. Neque repellat facilis illo.Quibusdam reiciendis sunt tempora fuga deleniti, molestias temporibus doloremque. Nam sed consequatur consectetur ut tempora a nesciunt, perspiciatis dolorem reprehenderit modi enim at veritatis, excepturi voluptate quod, voluptatibus voluptas. Cum.Debitis, nesciunt, repellat voluptatem sapiente incidunt quidem asperiores reprehenderit vero quisquam placeat sunt voluptatibus velit. Consectetur atque voluptates, repellendus facere sequi ea totam quia quis non incidunt. Soluta, aut, provident. Eos sequi itaque dolorem atque ex id maiores dolor eaque libero iste deserunt ea voluptate minima cum laboriosam, qui animi, fuga suscipit necessitatibus vero, autem blanditiis, totam nulla. Quo, et. Quisquam commodi voluptatum dolorem aspernatur, distinctio et ullam laborum laboriosam quo nisi, praesentium quaerat ab excepturi. Illum harum doloremque, accusantium, beatae culpa assumenda laboriosam, quos mollitia aperiam dolorem praesentium minus!</p>
    </div>

## Debug
The debug component enables component-to-DOM serialization.

    <a-scene debug></a-scene>

# Component-to-DOM Serialization

By default, for performance reasons, A-Frame does not update the DOM with component data. If we open the browser’s DOM inspector, we will see only the component names (and not the values) are visible.

    <a-entity geometry material position rotation></a-entity>

A-Frame stores the component data in memory. Updating the DOM takes CPU time for converting internal component data to strings. If we want to see the DOM update for debugging purposes, we can attach the debug component to the scene. Components will check for an enabled debug component before trying to serialize to the DOM. Then we will be able to view component data in the DOM:

    <a-entity geometry="primitive: box" material="color: red" position="1 2 3" rotation="0 180 0"></a-entity>

Make sure that this component is not active in production.

# Manually Serializing to DOM

To manually serialize to DOM, use Entity.flushToDOM or Component.flushToDOM:

    document.querySelector('a-entity').components.position.flushToDOM();  // Flush a component.
    document.querySelector('a-entity').flushToDOM();  // Flush an entity.
    document.querySelector('a-entity').flushToDOM(true);  // Flush an entity and its children.
    document.querySelector('a-scene').flushToDOM(true);  // Flush every entity.

## Running Content Scripts on the Scene
The recommended way is to write a component, and attach it to the scene element.  
The scene and its children will be initialized before this component.

<!-- language: lang-js -->

    AFRAME.registerComponent('do-something', {
      init: function () {
        var sceneEl = this.el;
      }
    });

<!-- language: lang-html -->

    <a-scene do-something></a-scene>

If for some particular reason you prefer not to write a dedicated component you need to wait for the scene to finish initializing and attaching:

<!-- language: lang-js -->

    var scene = document.querySelector('a-scene');
    
    if (scene.hasLoaded) {
      run();
    } else {
      scene.addEventListener('loaded', run);
    }
    
    function run () {
      var entity = scene.querySelector('a-entity');
      entity.setAttribute('material', 'color', 'red');
    }

