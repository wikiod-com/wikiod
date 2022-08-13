---
title: "Object Picking"
slug: "object-picking"
draft: false
images: []
weight: 9940
type: docs
toc: true
---

## Object picking / Raycasting
Raycasting means throwing a ray from the mouse position on the screen to the scene, this is how threejs determines what object you want to click on if you have implemented it. Threejs gets that information using an [octree][1], but still in production you may not want to compute the result at each frame or on the `mousemove` event, but rather on the `click` event for a more accessible app with low requirements.

    var raycaster, mouse = { x : 0, y : 0 };

    init();

    function init () {

        //Usual setup code here.

        raycaster = new THREE.Raycaster();
        renderer.domElement.addEventListener( 'click', raycast, false );

        //Next setup code there.

    }

    function raycast ( e ) {

        //1. sets the mouse position with a coordinate system where the center
        //   of the screen is the origin
        mouse.x = ( e.clientX / window.innerWidth ) * 2 - 1;
        mouse.y = - ( e.clientY / window.innerHeight ) * 2 + 1;

        //2. set the picking ray from the camera position and mouse coordinates
        raycaster.setFromCamera( mouse, camera );    

        //3. compute intersections
        var intersects = raycaster.intersectObjects( scene.children );

        for ( var i = 0; i < intersects.length; i++ ) {
            console.log( intersects[ i ] ); 
            /*
                An intersection has the following properties :
                    - object : intersected object (THREE.Mesh)
                    - distance : distance from camera to intersection (number)
                    - face : intersected face (THREE.Face3)
                    - faceIndex : intersected face index (number)
                    - point : intersection point (THREE.Vector3)
                    - uv : intersection point in the object's UV coordinates (THREE.Vector2)
            */
        }

    }
>CAUTION! You may lose your time gazing at the blank screen if you don't read the next part.

If you want to detect the light helper, set the second parameter of `raycaster.intersectObjects( scene.children );` to true.

It means `raycaster.intersectObjects( scene.children , true);`

The raycast code will only detect the light helper.

If you want it to detect normal objects as well as light helper, you need to copy the above raycast function again. See this [question](http://stackoverflow.com/questions/39760790/how-can-i-listen-to-onclick-event-spotlight-helper-and-attach-the-transformcontr/39823545#39823545).

The full raycast code is

    function raycast ( e ) {
    // Step 1: Detect light helper
        //1. sets the mouse position with a coordinate system where the center
        //   of the screen is the origin
        mouse.x = ( e.clientX / window.innerWidth ) * 2 - 1;
        mouse.y = - ( e.clientY / window.innerHeight ) * 2 + 1;
    
        //2. set the picking ray from the camera position and mouse coordinates
        raycaster.setFromCamera( mouse, camera );    
    
        //3. compute intersections (note the 2nd parameter)
        var intersects = raycaster.intersectObjects( scene.children, true );
    
        for ( var i = 0; i < intersects.length; i++ ) {
            console.log( intersects[ i ] ); 
            /*
                An intersection has the following properties :
                    - object : intersected object (THREE.Mesh)
                    - distance : distance from camera to intersection (number)
                    - face : intersected face (THREE.Face3)
                    - faceIndex : intersected face index (number)
                    - point : intersection point (THREE.Vector3)
                    - uv : intersection point in the object's UV coordinates (THREE.Vector2)
            */
        }
    // Step 2: Detect normal objects
        //1. sets the mouse position with a coordinate system where the center
        //   of the screen is the origin
        mouse.x = ( e.clientX / window.innerWidth ) * 2 - 1;
        mouse.y = - ( e.clientY / window.innerHeight ) * 2 + 1;
    
        //2. set the picking ray from the camera position and mouse coordinates
        raycaster.setFromCamera( mouse, camera );    
    
        //3. compute intersections (no 2nd parameter true anymore)
        var intersects = raycaster.intersectObjects( scene.children );
    
        for ( var i = 0; i < intersects.length; i++ ) {
            console.log( intersects[ i ] ); 
            /*
                An intersection has the following properties :
                    - object : intersected object (THREE.Mesh)
                    - distance : distance from camera to intersection (number)
                    - face : intersected face (THREE.Face3)
                    - faceIndex : intersected face index (number)
                    - point : intersection point (THREE.Vector3)
                    - uv : intersection point in the object's UV coordinates (THREE.Vector2)
            */
        }
    
    }

  [1]: https://en.wikipedia.org/wiki/Octree

## Object Picking / GPU
Object picking using Raycasting might be a heavy task for your CPU depending on your setup (for example if you don't have an octree like setup) and number of objects in the scene.

If you don't need the world coordinates under the mouse cursor but only to identify the object under it you can use GPU picking.
 
Short explanation, GPU can be a powerful tool for computation but you need to know how to get the results back. The idea is, if you render the objects with a color that represents their id, you can read the color of the pixel under the cursor and findout the id of the object that is picked. Remember RGB is just a hex value so there is a conversion exists between id (integer) and color (hex).

1) Create a new scene and a new rendering target for your object


    var pickingScene = new THREE.Scene();
    var pickingTexture = new THREE.WebGLRenderTarget(renderer.domElement.clientWidth, renderer.domElement.clientHeight);
        pickingTexture.texture.minFilter = THREE.LinearFilter;

2) Create a new shader Material for object picking;


    var vs3D = `
    attribute vec3 idcolor;
    varying vec3 vidcolor;
    void main(){
    vidcolor = idcolor;
    gl_Position = projectionMatrix * modelViewMatrix * vec4( position, 1.0);
    }`;
    
    var fs3D = `
    varying vec3 vidcolor;
    void main(void) {
    gl_FragColor = vec4(vidcolor,1.0);
    }`;
    
    var pickingMaterial = new THREE.ShaderMaterial(
        {
            vertexShader: vs3D,
            fragmentShader: fs3D,
            transparent: false,
            side: THREE.DoubleSide
        });

3) Add your mesh/line geometries a new attribute that represents their id in RGB, create the pickingObject using the same geometry and add it to the picking scene, and add the actual mesh to a id->object dictionary

    
    var selectionObjects = [];

    for(var i=0; i<myMeshes.length; i++){
        var mesh = myMeshes[i];
        var positions = mesh.geometry.attributes["position"].array;
        var idColor = new Float32Array(positions.length);
    
        var color = new THREE.Color();
        color.setHex(mesh.id);
    
        for (var j=0; j< positions.length; j+=3){
            idColor[j] = color.r;
            idColor[j+1] = color.g;
            idColor[j+2] = color.b;
        }
    
        mesh.geometry.addAttribute('idcolor', new THREE.BufferAttribute(idColor, 3));

        var pickingObject = new THREE.Mesh(mesh.geometry, pickingMaterial);
        
        pickingScene.add(pickingObject);
        selectionObjects[mesh.id] = mesh;
    }
   
4) Finally, on your mouse click handler


    renderer.render(pickingScene, camera, pickingTexture);
    var pixelBuffer = new Uint8Array(4);
    renderer.readRenderTargetPixels(pickingTexture, event.pageX, pickingTexture.height - event.pageY, 1, 1, pixelBuffer);
    var id = (pixelBuffer[0] << 16) | (pixelBuffer[1] << 8) | (pixelBuffer[2]);
    
    if (id>0){
        //this is the id of the picked object
    }else{
        //it's 0. clicked on an empty space
    }

    

   

    

