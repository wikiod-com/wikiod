---
title: "Geometries"
slug: "geometries"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

Examples work as of three.js R79 (revision 79).

## THREE.BoxGeometry
THREE.BoxGeometry builds boxes such as cuboids and cubes.

# Cubes #

Cubes created using THREE.BoxGeometry would use the same length for all sides.

JavaScript

    //Creates scene and camera
    
    var scene = new THREE.Scene();
    var camera = new THREE.PerspectiveCamera( 75, window.innerWidth / window.innerHeight, 0.1, 1000 );
    
    //Creates renderer and adds it to the DOM
    
    var renderer = new THREE.WebGLRenderer();
    renderer.setSize( window.innerWidth, window.innerHeight );
    document.body.appendChild( renderer.domElement );
    
    //The Box!
    
    //BoxGeometry (makes a geometry)
    var geometry = new THREE.BoxGeometry( 1, 1, 1 );
    //Material to apply to the cube (green)
    var material = new THREE.MeshBasicMaterial( { color: 0x00ff00 } );
    //Applies material to BoxGeometry
    var cube = new THREE.Mesh( geometry, material );
    //Adds cube to the scene
    scene.add( cube );
    
    //Sets camera's distance away from cube (using this explanation only for simplicity's sake - in reality this actually sets the 'depth' of the camera's position)
    
    camera.position.z = 5;
    
    //Rendering
    
    function render() {
      requestAnimationFrame( render );
      renderer.render( scene, camera );
    }
    render();

Notice the 'render' function. This renders the cube 60 times a second.

Full Code (with HTML)

    <!DOCTYPE html>
    <html>
      
      <head>
        <title>THREE.BoxGeometry</title>
        <script src="http://threejs.org/build/three.js"></script>
      </head>
    
      <body>
    
        <script>
          //Above JavaScript goes here
        </script>
    
      </body>
      
    </html>

# Cuboids #

The line `var geometry = new THREE.BoxGeometry( 1, 1, 1 );` gives us a cube. To make a cuboid, just change the parameters - they define the length, height and depth of the cube respectively.

Example:

    ...
    //Longer cuboid
    var geometry = new THREE.BoxGeometry( 2, 1, 1 );
    ...

# More (proving the cube is three-dimensional) #

The cube may seem to be just a square. To prove that it is, without doubt, three-dimensional, add the following lines of code to the 'render' function:

    ...
    cube.rotation.x += 0.05;
    cube.rotation.y += 0.05;
    ...

And watch as the merry cube spins round... and round... and round...

# Colourful #

Not for the faint-hearted...

The uniform colour for the entire cube is... green. Boring. To make each face a different colour, we've to dig to the geometry's faces.

    var geometry = new THREE.BoxGeometry(3, 3, 3, 1, 1, 1);

    /*Right of spawn face*/
    geometry.faces[0].color = new THREE.Color(0xd9d9d9);
    geometry.faces[1].color = new THREE.Color(0xd9d9d9);

    /*Left of spawn face*/
    geometry.faces[2].color = new THREE.Color(0x2196f3);
    geometry.faces[3].color = new THREE.Color(0x2196f3);

    /*Above spawn face*/
    geometry.faces[4].color = new THREE.Color(0xffffff);
    geometry.faces[5].color = new THREE.Color(0xffffff);

    /*Below spawn face*/
    geometry.faces[6].color = new THREE.Color(1, 0, 0);
    geometry.faces[7].color = new THREE.Color(1, 0, 0);

    /*Spawn face*/
    geometry.faces[8].color = new THREE.Color(0, 1, 0);
    geometry.faces[9].color = new THREE.Color(0, 1, 0);

    /*Opposite spawn face*/
    geometry.faces[10].color = new THREE.Color(0, 0, 1);
    geometry.faces[11].color = new THREE.Color(0, 0, 1);

    var material = new THREE.MeshBasicMaterial( {color: 0xffffff, vertexColors: THREE.FaceColors} );
    var cube = new THREE.Mesh(geometry, material);

NOTE: The method of colouring the faces is not the best method, but it works well (enough).

# Notes #

**Where's the `canvas` in the HTML document's body?**

There is no need to add a `canvas` to the body manually. The following three lines

    var renderer = new THREE.WebGLRenderer();
    renderer.setSize( window.innerWidth, window.innerHeight );
    document.body.appendChild( renderer.domElement );

create the renderer, its `canvas` and add the `canvas` to the DOM.

## THREE.CylinderGeometry
THREE.CylinderGeometry build cylinders.

# Cylinder #

Continuing from the previous example, the code to create the box could be replaced with the below.

    //Makes a new cylinder with
    // - a circle of radius 5 on top (1st parameter)
    // - a circle of radius 5 on the bottom (2nd parameter)
    // - a height of 20 (3rd parameter)
    // - 32 segments around its circumference (4th parameter)
    var geometry = new THREE.CylinderGeometry( 5, 5, 20, 32 );
    //Yellow
    var material = new THREE.MeshBasicMaterial( {color: 0xffff00} );
    var cylinder = new THREE.Mesh( geometry, material );
    scene.add( cylinder );

To build from scratch, here's the code.

    //Creates scene and camera
    
    var scene = new THREE.Scene();
    var camera = new THREE.PerspectiveCamera( 75, window.innerWidth / window.innerHeight, 0.1, 1000 );
    
    //Creates renderer and adds it to the DOM
    
    var renderer = new THREE.WebGLRenderer();
    renderer.setSize( window.innerWidth, window.innerHeight );
    document.body.appendChild( renderer.domElement );
    
    //The Cylinder!
    
    var geometry = new THREE.CylinderGeometry( 5, 5, 20, 32 );
    //Yellow
    var material = new THREE.MeshBasicMaterial( {color: 0xffff00} );
    var cylinder = new THREE.Mesh( geometry, material );
    scene.add( cylinder );
    
    //Sets camera's distance away from cube (using this explanation only for simplicity's sake - in reality this actually sets the 'depth' of the camera's position)
    
    camera.position.z = 30;
    
    //Rendering
    
    function render() {
      requestAnimationFrame( render );
      renderer.render( scene, camera );
    }
    render();

# More (proving the cylinder is three-dimensional) #

The cylinder may seem to be just... two-dimensional. To prove that it is, without doubt, three-dimensional, add the following lines of code to the 'render' function:

    ...
    cylinder.rotation.x += 0.05;
    cylinder.rotation.z += 0.05;
    ...

And the happy bright cylinder would spin randomly, amidst a dark, black background...

