---
title: "Game Development Basics"
slug: "game-development-basics"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

[![enter image description here][1]][1]

**basics** of game development.
-------------------------------

*Note*, this set of tutorials/articles contains many concepts which may provided as separated topics before. we have to refreshing them in the mind and learning a bit of implementing most critical parts of a video-game via actionscript-3.


  [1]: https://i.stack.imgur.com/CUIsz.png

## isometric character animating + movement


## **① the concepts used in this Example :** ##

| Class | Usage |
| ------ | ------ |
| `URLRequest` + `Loader` + `Event`  | Loading atlas map (sprite) from external path.   |
|`BitmapData` + `Sprite` + `beginBitmapFill` +<br>`Matrix` + `stageWidth & stageHeight`| drawing loaded resources to bitmapdata, <br>using tiled bitmaps, drawing with transformation.|
|`MovieClip` + `scrollRect` + `Bitmap` + `Rectangle`| creating and cliping character movieclip<br> using Bitmap as timeline.|
|`KeyboardEvent`| detecting user inputs |
|`Event.EXIT_FRAME`| implementing game Loop function|
----------
## **② Resources :**  <sup> (no permission for using this resources for a commercial purposes)</sup>
[![character pose][1]][1]
[![grass tile][2]][2]

## **③ Code and Comments :** ##

> **Note:** FPS 15 Used for this tutorial, its recommended, but if need
> more, you must modify some part of code by your self.

at first we must download our resources from external urls.

    const src_grass_tile_url:String = "https://i.stack.imgur.com/sjJFS.png";
    const src_character_atlas_url:String = "https://i.stack.imgur.com/B7ztZ.png";
    
    var loader:Loader = new Loader();
    loader.contentLoaderInfo.addEventListener(Event.COMPLETE, setGround);
    loader.load(new URLRequest(src_grass_tile_url));


----------


setGround will be caled once `src_grass_tile_url` is loaded and ready for use. in follow implementing setGround to get resource and draw it as the game background

    function setGround(e:Event):void {
        /* drawing ground */
        /* loader is a displayObject, so we can simply draw it into the bitmap data*/
        /* create an instance of Bitmapdata with same width and height as our window*/
        /* (also set transparent to false because grass image, does not contains any transparent pixel) */
        var grass_bmd:BitmapData = new BitmapData(loader.width, loader.height, false, 0x0);
        /* time to draw */
        grass_bmd.draw(loader); // drawing loader into the bitmapData
        /* now we have to draw a tiled version of grass_bmd inside a displayObject Sprite to displaying 
           BitmapData on stage */
        var grass_sprite:Sprite = new Sprite();
        // for drawing a bitmap inside sprite, we must use <beginBitmapFill> with graphic property of the sprite
        // then draw a full size rectangle with that Fill-Data
        // there is a repeat mode argument with true default value so we dont set it true again.
        // use a matrix for scalling grass Image during draw to be more cute!
        var mx:Matrix = new Matrix();
        mx.scale(2, 2);
        grass_sprite.graphics.beginBitmapFill(grass_bmd, mx);
        grass_sprite.graphics.drawRect(0, 0, stage.stageWidth, stage.stageHeight);
        // now add sprite to displayobjectcontainer to be displayed
        stage.addChild(grass_sprite);
        
        // well done, ground is ready, now we must initialize our character
        // first, load its data, i just re-use my loader for loading new image, but with another complete handler (setCharacter)
        // so remove existing handler, then add new one
        loader.contentLoaderInfo.removeEventListener(Event.COMPLETE, setGround);
        loader.contentLoaderInfo.addEventListener(Event.COMPLETE, setCharacter);
        loader.load(new URLRequest(src_character_atlas_url));
    }
the code is well damn commented, after we have done with ground, its time to implementing character. character also contains a resource which must be loaded with same way. so at the end of `setGround` we are heading to the `setCharacter` which is another complete call back.

    function setCharacter(e:Event):void {
        // let assuming that what is really character!
        // a set of images inside a single image!
        // that images are frames of our character (also provides movement for different directions)
        // first load this
        var character_bmd:BitmapData = new BitmapData(loader.width, loader.height, true, 0x0); // note character is transparent
        character_bmd.draw(loader);
        // take a look at sprite sheet, how many frames you see?
        // 42 frames, so we can get width of a single frame
        const frame_width:uint = character_bmd.width / 42; // 41 pixels
        // as i show you above, to displaying a BitmapData, we have to draw it using a DisplayObject,
        // another way is creating a Bitmap and setting its bitmapdata
        var character_bmp:Bitmap = new Bitmap(character_bmd);
        // but its not enough yet, a movieClip is necessary to cliping and animating this bitmap (as a child of itself)
        var character_mc:MovieClip = new MovieClip();
        character_mc.addChild(character_bmp);
        character_bmp.name = "sprite_sheet"; // setting a name to character_bmp, for future accessing
        character_mc.scrollRect = new Rectangle(0, 0, frame_width, character_bmd.height); // cliping movieclip, to dusplaying only one frame
        character_mc.name = "character"; // setting a name to character_mc, for future accessing
        stage.addChild(character_mc); // adding it to stage.
        // well done, we have a character, but its static yet! 2 steps remaining. 1 controlling 2 animating
        // at first setting a control handler for moving character in 8 directions.
        stage.addEventListener(KeyboardEvent.KEY_DOWN, keyDown);
        stage.addEventListener(KeyboardEvent.KEY_UP, keyUp);
    }

now character is ready to controll. its displayed well and ready for controlling.
so keyboard events attached and listening for arrow keys as following code :


    // we storing key stats inside <keys> Object
    var keys:Object = {u:false, d:false, l:false, r:false};
    function keyDown(e:KeyboardEvent):void {
        switch (e.keyCode) {
            case 38: //up
                keys.u = true;
                break;
            case 40: //down
                keys.d = true;
                break;
            case 37: //left
                keys.l = true;
                break;
            case 39: //right
                keys.r = true;
                break;
        }
    }
    function keyUp(e:KeyboardEvent):void {
        switch (e.keyCode) {
            case 38: //up
                keys.u = false;
                break;
            case 40: //down
                keys.d = false;
                break;
            case 37: //left
                keys.l = false;
                break;
            case 39: //right
                keys.r = false;
                break;
        }
    }


----------

`keys:Object` stores 4's boolean variable per each arrow key, moving proccess must be done inside the update (Loop) function of the game, so we must pass keyboard stats to it.
lets implementing **Loop** function.

    // initialize game Loop function for updating game objects
    addEventListener(Event.EXIT_FRAME, loop);

    // speed of character movement
    const speed:Number = 5;
    // this function will be called on each frame, with same rate as your project fps
    function loop(e:Event):void {
        if (keys.u) stage.getChildByName("character").y -= speed;
        else if (keys.d) stage.getChildByName("character").y += speed;
        if (keys.l) stage.getChildByName("character").x -= speed;
        else if (keys.r) stage.getChildByName("character").x += speed;
    }

speed is a helper constant, defines velocity of character.
above code presents a simple 8 direction movement with this priority low:
`Up > Down` `Left > Right`. so if up and down arrow are pressed in same time, the character only moves to *up* (not freezing).

**well Done!!!  only one step remaining, animation, the most important part of this tutorial**

what is really animation? a set of keyframes which contains atleast one frame  
lets creating our keyframs Object, which contains name of keyframes  
and also some data about starting and ending frame of this keyframe  
**Note**, in isometric games, each keyframe contains 8 direction (can be reduced to 5 with use of flipping)

    var keyframs:Object = {
        idle: {up:[0,0], up_right:[1,1], right:[2,2], down_right:[3,3], down:[4,4]}, // [2,2] means start frame is 2 and end frame is 2
        run: {up:[5,10], up_right:[11,16], right:[17,22], down_right:[23,28], down:[29,34]}
    };

we should ignore remaining frames, this example only provides idle and run animation  
for example the start frame of idle animation with direction right, is: <keyframs.idle.right[0]>  
now lets implementing Animator function

    var current_frame:uint;
    function animate(keyframe:Array):void {
        // how it works
        // just called with a keyframe with direction (each frame),
        // if keyframe is what is already playing, its just moved to next frame and got updated (or begning frame for loop)
        // other wise, just moved to begining frame of new keyframe
        if (current_frame >= keyframe[0] && current_frame <= keyframe[1]) { // check if in bound
            current_frame++;
            if (current_frame > keyframe[1]) // play back if reached
                current_frame = keyframe[0];
        } else {
            current_frame = keyframe[0]; // start new keyframe from begining
        }
        // moving Bitmap inside character MovieClip
        var character:MovieClip = stage.getChildByName("character") as MovieClip;
        var sprite_sheet:Bitmap = character.getChildByName("sprite_sheet") as Bitmap;
        sprite_sheet.x = -1 * current_frame * character.width;
    }

read comments of above function, however main job of this function is moving *sprite_sheet* `Bitmap` inside *character* `MovieClip`.

we know that every update should be done inside the Loop function, so we will invoke this function from Loop with related keyframes. this is the updated Loop function :

    // speed of character movement
    const speed:Number = 8;
    var last_keyStat:Object = {u:false, d:false, l:false, r:false}; // used to getting a backup of previous keyboard stat for detecting correct idle direction
    // this function will be called on each frame, with same rate as your project fps
    function loop(e:Event):void {
        if (keys.u) stage.getChildByName("character").y -= speed;
        else if (keys.d) stage.getChildByName("character").y += speed;
        if (keys.l) stage.getChildByName("character").x -= speed;
        else if (keys.r) stage.getChildByName("character").x += speed;
        
        // animation detection
        if (keys.u && keys.l) { animate(keyframs.run.up_right); flip(true); }
        else if (keys.u && keys.r) { animate(keyframs.run.up_right); flip(false); }
        else if (keys.d && keys.l) { animate(keyframs.run.down_right); flip(true); }
        else if (keys.d && keys.r) { animate(keyframs.run.down_right); flip(false); }
        else if (keys.u) { animate(keyframs.run.up); flip(false); }
        else if (keys.d) { animate(keyframs.run.down); flip(false); }
        else if (keys.l) { animate(keyframs.run.right); flip(true); }
        else if (keys.r) { animate(keyframs.run.right); flip(false); }
        else {
            // if character dont move, so play idle animation
            // what is the best practice to detecting idle direction?
            // my suggestion is to sotring previous keyboard stats to determining which idle direction is correct
            // do any better thing if possible (absolutely is possible)
            // i just simply copy it from above, and replaced (keys) with (last_keyStat) and (run) with (idle)
            if (last_keyStat.u && last_keyStat.l) { animate(keyframs.idle.up_right); flip(true); }
            else if (last_keyStat.u && last_keyStat.r) { animate(keyframs.idle.up_right); flip(false); }
            else if (last_keyStat.d && last_keyStat.l) { animate(keyframs.idle.down_right); flip(true); }
            else if (last_keyStat.d && last_keyStat.r) { animate(keyframs.idle.down_right); flip(false); }
            else if (last_keyStat.u) { animate(keyframs.idle.up); flip(false); }
            else if (last_keyStat.d) { animate(keyframs.idle.down); flip(false); }
            else if (last_keyStat.l) { animate(keyframs.idle.right); flip(true); }
            else if (last_keyStat.r) { animate(keyframs.idle.right); flip(false); }
        }
        // update last_keyStat backup
        last_keyStat.u = keys.u;
        last_keyStat.d = keys.d;
        last_keyStat.l = keys.l;
        last_keyStat.r = keys.r;
    }

read comments, we just simply detecting a true keyframe through keyboard stats.  then also do same thing for detecting idle animation. for idle animations we have no key input to using for detecting which direction character is on, so a simle helper variable could be handy to storing previous state of keyboard (last_keyStat).


----------


also there is a new function `flip` which is another helper function used for simulating missing animations (left + up_left + down_left) also this funcion do some fixes which is commented below:

    // usage of flip function is because of Movieclip registration point, its a fix
    // as the registration point of MovieClip is not placed in center, when flipping animation (for non existing directions inside spritesheet)
    // character location changes with an unwanted value equal its width, so we have to prevent this and push it back or forward during flip
    function flip(left:Boolean):void {
        var character:MovieClip = stage.getChildByName("character") as MovieClip;
        if (left) {
            if (character.scaleX != -1) {
                character.scaleX = -1;
                character.x += character.width; // comment this line to see what happen without this fix
            }
        } else {
            if (character.scaleX != 1) {
                character.scaleX = 1;
                character.x -= character.width; // comment this line to see what happen without this fix
            }
        }
    }

**our work is ending here. special thanks for Editor's which making this tutorial more undrestandable. also Here is a Live demo of this tutorial plus an external link of complete code.**

## **④ External References:** ##

 - [**full code**][3]
 - [**Live Demo**][4]


  [1]: https://i.stack.imgur.com/B7ztZ.png
  [2]: https://i.stack.imgur.com/sjJFS.png
  [3]: https://gist.github.com/payamsbr/4575067be0f51bdd6a2a5211f4000f31
  [4]: http://www.fastswf.com/I3apID8

