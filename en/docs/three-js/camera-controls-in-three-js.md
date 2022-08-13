---
title: "Camera Controls in Three.js"
slug: "camera-controls-in-threejs"
draft: false
images: []
weight: 9937
type: docs
toc: true
---

This document outlines how you can easily add some existing Camera Controls to your scene, as well as provide guidance on creating custom controls.

Note, the pre-made control scripts can be found in the `/examples/js/controls` folder of the library.

## Orbit Controls
An Orbit Camera is one that allows the user to rotate around a central point, but while keeping a particular axis locked. This is extremely popular because it prevents the scene from getting "tilted" off-axis. This version locks the Y (vertical) axis, and allows users to Orbit, Zoom, and Pan with the left, middle, and right mouse buttons (or specific touch events).

# index.html

    <html>
        <head>
            <title>Three.js Orbit Controller Example</title>
            <script src="/javascripts/three.js"></script>
            <script src="/javascripts/OrbitControls.js"></script>
        </head>
        <body>
            <script src="javascripts/scene.js"></script>
        </body>
    </html>

# scene.js
    var scene, renderer, camera;
    var cube;
    var controls;
    
    init();
    animate();
    
    function init()
    {
        renderer = new THREE.WebGLRenderer( {antialias:true} );
        var width = window.innerWidth;
        var height = window.innerHeight;
        renderer.setSize (width, height);
        document.body.appendChild (renderer.domElement);
    
        scene = new THREE.Scene();
        
        var cubeGeometry = new THREE.BoxGeometry (10,10,10);
        var cubeMaterial = new THREE.MeshBasicMaterial ({color: 0x1ec876});
        cube = new THREE.Mesh (cubeGeometry, cubeMaterial);
    
        cube.position.set (0, 0, 0);
        scene.add (cube);
    
        camera = new THREE.PerspectiveCamera (45, width/height, 1, 10000);
        camera.position.y = 160;
        camera.position.z = 400;
        camera.lookAt (new THREE.Vector3(0,0,0));
    
        controls = new THREE.OrbitControls (camera, renderer.domElement);
        
        var gridXZ = new THREE.GridHelper(100, 10);
        gridXZ.setColors( new THREE.Color(0xff0000), new THREE.Color(0xffffff) );
        scene.add(gridXZ);
    
    }
    
    function animate()
    {
        controls.update();
        requestAnimationFrame ( animate );  
        renderer.render (scene, camera);
    }

The OrbitControls script has a several settings that can be modified. The code is well documented, so look in [OrbitControls.js][1] to see those. As an example, here is a code snippet showing a few of those being modified on a new OrbitControls object.

    controls = new THREE.OrbitControls( camera, renderer.domElement );
    controls.enableDamping = true;
    controls.dampingFactor = 0.25;
    controls.enableZoom = true;
    controls.autoRotate = true;


  [1]: https://github.com/mrdoob/three.js/blob/master/examples/js/controls/OrbitControls.js

## Custom Camera Control - Mouse-based Sliding
Here's an example of a custom camera controller. This reads the position of the mouse within the client window, and then slides the camera around as if it were following the mouse on the window.

# index.html

    <html>
        <head>
            <title>Three.js Custom Mouse Camera Control Example</title>
            <script src="/javascripts/three.js"></script>
        </head>
        <body>
            <script src="javascripts/scene.js"></script>
        </body>
    </html>

# scene.js

    var scene, renderer, camera;
    var cube;
    var cameraCenter = new THREE.Vector3();
    var cameraHorzLimit = 50;
    var cameraVertLimit = 50;
    var mouse = new THREE.Vector2();
    
    init();
    animate();
    
    function init()
    {
        renderer = new THREE.WebGLRenderer( {antialias:true} );
        var width = window.innerWidth;
        var height = window.innerHeight;
        renderer.setSize (width, height);
        document.body.appendChild (renderer.domElement);
      
        scene = new THREE.Scene();
        
        var cubeGeometry = new THREE.BoxGeometry (10,10,10);
        var cubeMaterial = new THREE.MeshBasicMaterial ({color: 0x1ec876});
        cube = new THREE.Mesh (cubeGeometry, cubeMaterial);
    
        cube.position.set (0, 0, 0);
        scene.add (cube);
    
        camera = new THREE.PerspectiveCamera (45, width/height, 1, 10000);
        camera.position.y = 160;
        camera.position.z = 400;
        camera.lookAt (new THREE.Vector3(0,0,0));
        cameraCenter.x = camera.position.x;
        cameraCenter.y = camera.position.y;
    
        //set up mouse stuff
        document.addEventListener('mousemove', onDocumentMouseMove, false);
        window.addEventListener('resize', onWindowResize, false);
        
        var gridXZ = new THREE.GridHelper(100, 10);
        gridXZ.setColors( new THREE.Color(0xff0000), new THREE.Color(0xffffff) );
        scene.add(gridXZ);
    }
    
    function onWindowResize ()
    {
        camera.aspect = window.innerWidth / window.innerHeight;
        camera.updateProjectionMatrix();
        renderer.setSize (window.innerWidth, window.innerHeight);
    }
    
    function animate()
    {   
        updateCamera();
        requestAnimationFrame ( animate );  
        renderer.render (scene, camera);
    }
    
    function updateCamera() {
        //offset the camera x/y based on the mouse's position in the window
        camera.position.x = cameraCenter.x + (cameraHorzLimit * mouse.x);
        camera.position.y = cameraCenter.y + (cameraVertLimit * mouse.y);
    }
    
    function onDocumentMouseMove(event) {
        event.preventDefault();
        mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
        mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;
    }
    
    function onWindowResize() {
        camera.aspect = window.innerWidth / window.innerHeight;
        camera.updateProjectionMatrix();
        renderer.setSize(window.innerWidth, window.innerHeight);
    }

As you can see, here we are merely updating the Camera position during the rendering's `animate` phase, like we could for any object in the scene. In this case, we are simply re-positioning the camera at a point offset from it's original X and Y coordinates. This could just as easily be the X and Z coordinates, or a point along a path, or something completely different not even related to the mouse's position at all.

