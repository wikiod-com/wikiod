---
title: "Loading External Files"
slug: "loading-external-files"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

There are some cases where your application cannot manipulate the contents of assets loaded from an external resource (e.g. transform images). I can't remember for certain what they are, but I am fairly sure it's related to cross domain content loading.

## Loading External Images/SWFs With The Loader
1.  Create a Loader object:

        var loader:Loader = new Loader();   //import 

2.  Add listeners on the loader.  Standard ones are complete and io/security errors

        loader.contentLoaderInfo.addEventListener(Event.COMPLETE, loadComplete); //when the loader is done loading, call this function
        loader.contentLoaderInfo.addEventListener(IOErrorEvent.IO_ERROR, loadIOError); //if the file isn't found
        loader.contentLoaderInfo.addEventListener(SecurityErrorEvent.SECURITY_ERROR, loadSecurityError); //if the file isn't allowed to be loaded

3. Load the desired file:

        loader.load(new URLRequest("image.png"));

3. Create Your Event Handlers:

        function loadComplete(e:Event):void {
            //load complete
            //the loader is actually a display object itself, so you can just add it to the display list
            addChild(loader) 
            //or addChild(loader.content) to add the root content of what was loaded;
        }
            
        function loadIOError(e:IOErrorEvent):void {
            //the file failed to load, 
        }
           
        function loadSecurityError(e:SecurityError):void {
            //the file wasn't allowed to load
        }

>Loading with the [Loader][1] class is asynchronous. This means after you call `loader.load` the application will continue running while the file loads.  Your loader content isn't available until the loader dispatches the `Event.COMPLETE` event.

-----------

imports needed:

    import flash.display.Loader;
    import flash.events.Event;
    import flash.events.IOErrorEvent;
    import flash.events.SecurityErrorEvent;
    import flash.net.URLRequest;


  [1]: http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/display/Loader.html

## Loading a text file with FileStream (AIR runtime only)
A simple example on how to read a UTF text file synchronously.

    import flash.filesystem.File;
    import flash.filesystem.FileMode;
    import flash.filesystem.FileStream;

-----------

    //First, get a reference to the file you want to load
    var myFile:File = File.documentsDirectory.resolvePath("lifestory.txt");
    
    //Create a FileStream object
    fileStream = new FileStream(); 
    
    //open the file
    fileStream.open(myFile, FileMode.READ);

    //read the data and assign it to a local variable
    var fileText:String = fileStream.readUTF();

    //close the current filestream
    fileStream.close();

