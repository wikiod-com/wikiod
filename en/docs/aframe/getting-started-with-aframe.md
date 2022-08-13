---
title: "Getting started with aframe"
slug: "getting-started-with-aframe"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Getting started
A-Frame can be developed from a plain HTML file without having to install anything! A great way to try out A-Frame to remix the starter example on Glitch, an online code editor that instantly hosts and deploys for free. Or create an `.html` file and include A-Frame in the `head`:

    <html>
      <head>
        <script src="https://aframe.io/releases/0.5.0/aframe.min.js"></script>
      </head>
      <body>
        <a-scene>
          <a-box position="-1 0.5 -3" rotation="0 45 0" color="#4CC3D9"></a-box>
          <a-sphere position="0 1.25 -5" radius="1.25" color="#EF2D5E"></a-sphere>
          <a-cylinder position="1 0.75 -3" radius="0.5" height="1.5" color="#FFC65D"></a-cylinder>
          <a-plane position="0 0 -4" rotation="-90 0 0" width="4" height="4" color="#7BC8A4"></a-plane>
          <a-sky color="#ECECEC"></a-sky>
        </a-scene>
      </body>
    </html>

# Include the JS Build

To include A-Frame into an HTML file, we drop a `script` tag pointing to the CDN build:

    <head>
      <script src="https://aframe.io/releases/0.5.0/aframe.min.js"></script>
    </head>

# Install from npm

We can also install A-Frame through npm:

    $ npm install aframe

Then we can bundle A-Frame into our application. For example, with Browserify or Webpack:

    require('aframe');

If you use npm, you can use angle, a command line interface for A-Frame. angle can initialize a scene template with a single command:

    npm install -g angle && angle initscene


## Features
# VR Made Simple

Just drop in a `script` tag and `a-scene`. A-Frame will handle 3D boilerplate, VR setup, and default controls. Nothing to install, no build steps.

# Declarative HTML

HTML is easy to read, understand, and copy-and-paste. Being based on top of HTML, A-Frame is accessible to everyone: web developers, VR enthusiasts, artists, designers, educators, makers, kids.

# Cross-Platform VR

Build VR applications for Vive, Rift, Daydream, GearVR, and Cardboard with support for all respective controllers. Don’t have a headset or controllers? No problem! A-Frame still works on standard desktop and smartphones.

# Entity-Component Architecture

A-Frame is a powerful three.js framework, providing a declarative, composable, reusable entity-component structure.js. HTML is just the tip of the iceberg; developers have unlimited access to JavaScript, DOM APIs, three.js, WebVR, and WebGL.

# Performance

A-Frame is optimized from the ground up for WebVR. While A-Frame uses the DOM, its elements don’t touch the browser layout engine. 3D object updates are all done in memory with little overhead under a single requestAnimationFrame call. For reference, see A-Painter, a Tilt Brush clone built in A-Frame that runs like native (90+ FPS).

# Tool Agnostic

Since the Web was built on the notion of the HTML, A-Frame is compatible with most libraries, frameworks, and tools including React, Preact, Vue.js, Angular, d3.js, Ember.js, jQuery.

# Visual Inspector

A-Frame provides a handy built-in visual 3D inspector. Open up any A-Frame scene, hit <kbd>ctrl</kbd> + <kbd>alt</kbd> + <kbd>i</kbd>, and fly around to peek behind the hood!

[![Visual inspector][1]][1]

# Registry

Take powerful components that developers have published and plug them in straight from HTML. Similar to the Unity Asset Store, the A-Frame Registry collects and curates these components for easy discovery.

# Components

Hit the ground running with A-Frame’s core components such as geometries, materials, lights, animations, models, raycasters, shadows, positional audio, text, and Vive / Touch / Daydream / GearVR / Cardboard controls. Get even further with community components such as particle systems, physics, multiuser, oceans, mountains, speech recognition, motion capture, teleportation, super hands, and augmented reality.


  [1]: https://i.stack.imgur.com/fUH4K.png

## Getting started for AR


