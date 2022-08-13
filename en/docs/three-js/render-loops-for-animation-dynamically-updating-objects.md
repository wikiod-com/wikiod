---
title: "Render Loops for Animation Dynamically updating objects"
slug: "render-loops-for-animation-dynamically-updating-objects"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

This document describes some common ways to add animation directly into your Three.js scenes. While there are libraries and frameworks that can add dynamic movement to your scene (tweens, physics, etc), it is helpful to understand how you can do this yourself simply with a few lines of code.

The core concept of animation is updating an object's properties (rotation and translation, usually) in small amounts over a period of time. For example, if you translate an object by increasing the X position by 0.1 every tenth of a second, it will be 1 unit further on the X axis in 1 second, but the viewer will perceive it as having smoothly moved to that position over that time instead of jumping directly to the new position.

To assist us, we create a _render loop_ in the script.

    var render = function () {
        requestAnimationFrame( render );
        //update some properties here
        renderer.render(scene, camera);
    }

In the spinning cube example above, we use this idea - small incremental updates - to change the rotation of the cube every time a new frame of animation is requested. By incrementing the `rotation.x` and `rotation.y` properties of the `cube` object on every frame, the cube appears to spin on those two axes.

As another example, it's not uncommon to separate your needed update into other functions, where you can do additional calculations and checks while keeping the render loop uncluttered. For example, the render loop below calls four different update functions, each one intended to update a separate object (or an array of objects, in the case of `updatePoints()`) in the scene.

    //render loop
    function render() {
        requestAnimationFrame( render );
        updateGrid();
        updateCube();
        updateCamera();
        updatePoints(pList);
        renderer.render( scene, camera);
    }
    render();

You may notice in examples online that the camera controls are also part of the render loop.

    controls = new THREE.OrbitControls( camera, renderer.domElement );
    controls.enableDamping = true;
    controls.dampingFactor = 0.25;
    controls.enableZoom = true;
    controls.autoRotate = true;

    var render = function () {
        requestAnimationFrame( render );
        controls.update();
        renderer.render(scene, camera);
    };

This is because the script for controlling the camera is doing the same thing; updating it over time. The changes might be caused by user input such as a mouse position, or something programmatic like following a path. In either case though, we are just animating the camera as well.

## Spinning Cube
    var scene = new THREE.Scene();  
    var camera = new THREE.PerspectiveCamera( 75, window.innerWidth/window.innerHeight, 0.1, 1000 ); 

    var renderer = new THREE.WebGLRenderer(); 
    renderer.setSize( window.innerWidth, window.innerHeight ); 
    document.body.appendChild( renderer.domElement );

    var geometry = new THREE.BoxGeometry( 1, 1, 1 );
    var material = new THREE.MeshBasicMaterial( { color: 0x00ff00 } );
    var cube = new THREE.Mesh( geometry, material );
    scene.add( cube );

    camera.position.z = 5;
    
    //Create an render loop to allow animation
    var render = function () {
        requestAnimationFrame( render );

        cube.rotation.x += 0.1;
        cube.rotation.y += 0.1;

        renderer.render(scene, camera);
    };

    render();

