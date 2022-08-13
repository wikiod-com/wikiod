---
title: "Getting started with three.js"
slug: "getting-started-with-threejs"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
 - You can install [three.js](https://threejs.org/) via npm:


    npm install three


 - You can add it from a CDN to your HTML:


     <script src="https://cdnjs.cloudflare.com/ajax/libs/three.js/r83/three.js"></script>

 - You can use the [three.js editor](https://threejs.org/editor/) to give it a try and download the project as an example or starting point.





## Simple Boilerplate : spinning cube and orbit controls with damping
This is the basic HTML file that can be used as a boilerplate when starting a project. This boilerplate uses orbit controls with damping (camera that can move around an object with deceleration effect) and creates a spinning cube.

    <!DOCTYPE html>
    <html>
        <head>
            <title>Three.js Boilerplate</title>

            <!--This is important to get a correct canvas size on mobile-->
            <meta name='viewport' content='width=device-width, user-scalable=no'/>

            <style>
                body{
                    margin:0;
                    overflow:hidden;
                }

                /*
                  Next 2 paragraphs are a good practice. 
                  In IE/Edge you have to provide the cursor images.
                */
                canvas{
                    cursor:grab;
                    cursor:-webkit-grab;
                    cursor:-moz-grab;
                }
                canvas:active{
                    cursor:grabbing;
                    cursor:-webkit-grabbing;
                    cursor:-moz-grabbing;
                }
            </style>
        </head>
        <body>
            
            <script src='three.js/build/three.js'></script>
            <script src='three.js/examples/js/controls/OrbitControls.js'></script>

            <script>
                var scene, renderer, camera, controls, cube;
            
                init();

                function init () {
                    renderer = new THREE.WebGLRenderer();
                    
                    //this is to get the correct pixel detail on portable devices
                    renderer.setPixelRatio( window.devicePixelRatio );

                    //and this sets the canvas' size.
                    renderer.setSize( window.innerWidth, window.innerHeight );
                    document.body.appendChild( renderer.domElement );

                    scene = new THREE.Scene();

                    camera = new THREE.PerspectiveCamera( 
                        70,                                         //FOV
                        window.innerWidth / window.innerHeight,     //aspect
                        1,                                          //near clipping plane
                        100                                         //far clipping plane
                    );
                    camera.position.set( 1, 3, 5 );

                    controls = new THREE.OrbitControls( camera, renderer.domElement );
                    controls.rotateSpeed = .07;
                    controls.enableDamping = true;
                    controls.dampingFactor = .05;

                    window.addEventListener( 'resize', function () {
                        camera.aspect = window.innerWidth / window.innerHeight;
                        camera.updateProjectionMatrix();
                        renderer.setSize( window.innerWidth, window.innerHeight );
                    }, false );

                    cube = new THREE.Mesh(
                        new THREE.BoxGeometry( 1, 1, 1 ),
                        new THREE.MeshBasicMaterial()
                       );
                    scene.add( cube );

                    animate();
                }

                function animate () {
                    requestAnimationFrame( animate );
                    controls.update();
                    renderer.render( scene, camera );
                    
                    cube.rotation.x += 0.01;
                }
            </script>
        </body>
    </html>

## Hello world!
The example is taken from [threejs website](https://threejs.org/docs/index.html#manual/introduction/Creating-a-scene).

You may want to [download three.js](http://threejs.org/build/three.js) and change the script source below.

There are many more advanced examples under this link.

HTML:

    <html>
    <head>
        <meta charset=utf-8>
        <title>My first Three.js app</title>
        <style>
            body { margin: 0; }
            canvas { width: 100%; height: 100% }
        </style>
    </head>
    <body>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/three.js/r83/three.js"></script>
        <script>
            // Our JavaScript will go here.
        </script>
    </body>
</html>

The basic scene with a static cube in JavaScript:

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

To actually see anything, we need a Render() loop:

    function render() {
        requestAnimationFrame( render );
        renderer.render( scene, camera );
    }
    render();

