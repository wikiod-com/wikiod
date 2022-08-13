---
title: "Add An Image In Phaser"
slug: "add-an-image-in-phaser"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Adding an image file (preferable a png) to your game as an "Image" object in Phaser.

## Syntax
 - game.load.image(  name:string, file:string,);
 - game.add.image( x:number, y:number, name:string);



 - An Image object is a good choice for things in your game that don't
   use frame animations and don't otherwise need to be a Sprite.
 - By default the anchor point for an image in the upper left corner, but you can change it like this: `image.anchor.setTo(0.5, 0.5);`

## Create And Add To Screen
You first must create a "Game" object in Phaser.

    var game = new Phaser.Game(800, 600, Phaser.AUTO, 'phaser-example', { preload: preload, create: create });

In the preload callback function load the image.


    function preload() {
    
        game.load.image('thing', 'assets/thing-image.png');
    
    }

| Parameter | Details (Game.add.image)|
| ------ | ------ |
| name   | the name used to reference the image in the game.add.image method.   |
| file   | path to the asset file (relative to the root directory for the project. 


Then in the create function use the "add" method of the game object to create the Image object and it to the screen.



    function create() {
  
      var image = game.add.image(100, 100, 'thing');

    }




| Parameter | Details (Game.add.image)|
| ------ | ------ |
| x   | the x coordinates where the image should be added.   |
| y   | the y coordinate where the image should be added.   |
| name | the name of the image assigned in the game.load.image method. |



