---
title: "Meshes"
slug: "meshes"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

A Three.js [Mesh](https://threejs.org/docs/?q=mesh#Reference/Objects/Mesh) is a base class that inherits from [Object3d](https://threejs.org/docs/?q=geomet#Reference/Core/Object3D) and is used to instantiate polygonal objects by combining a [Geometry](https://threejs.org/docs/?q=geomet#Reference/Core/Geometry) with a [Material](https://threejs.org/docs/?q=mater#Reference/Materials/Material). `Mesh` is also the base class for the more advanced `MorphAnimMesh` and `SkinnedMesh` classes.

## Syntax
- new THREE.Mesh(geometry, material);

Both the geometry and material are optional and will default to `BufferGeometry` and `MeshBasicMaterial` respectively if they are not provided in the constructor.

## Render a cube mesh with a box geometry and a basic material
    var scene = new THREE.Scene();
    var camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 50);
    camera.position.z = 25;
    
    var renderer = new THREE.WebGLRenderer();
    renderer.setSize(window.innerWidth, window.innerHeight);
    document.body.appendChild(renderer.domElement);
    
    var geometry = new THREE.BoxGeometry(1, 1, 1);
    var material = new THREE.MeshBasicMaterial({ color: 0x00ff00 });
    var cubeMesh = new THREE.Mesh(geometry, material);
    scene.add(cubeMesh);

    var render = function () {
        requestAnimationFrame(render);
    
        renderer.render(scene, camera);
    };
    
    render();

