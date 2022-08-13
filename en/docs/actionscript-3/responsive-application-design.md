---
title: "Responsive Application Design"
slug: "responsive-application-design"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Basic Responsive Application
    package
    {
        import flash.display.Sprite;
        import flash.display.StageAlign;
        import flash.display.StageScaleMode;
        import flash.events.Event;
    
        public class Main extends Sprite 
        {        
            //Document Class Main Constructor
            public function Main() 
            {
                //Sometimes stage isn't available yet, so if not, wait for it before proceeding
                if (!stage) {
                    addEventListener(Event.ADDED_TO_STAGE, stageReady);
                }else {
                    stageReady();
                }
            }
            
            protected function stageReady(e:Event = null):void {
                //align the stage to the top left corner of the window/container
                stage.align = StageAlign.TOP_LEFT;
                //don't scale the content when the window resizes
                stage.scaleMode = StageScaleMode.NO_SCALE;
                
                //listen for when the window is resized
                stage.addEventListener(Event.RESIZE, stageResized);
            }
            
            protected function stageResized(e:Event):void {
                //use stage.stageWdith & stage.stageHeight to repostion and resize items
            }   
        }
    }

## Performing lengthy processes and not get unresponsive application
There are situations when you need to calculate something really large in your Flash application, while not interrupting the user's experience. For this, you need to devise your lengthy process as a multi-step process with saved state between iterations. For example, you need to perform a background update of a lot of internal objects, but if you desire to update them all at once with a simple `for each (var o in objects) { o.update(); }`, Flash briefly (or not as briefly) becomes unresponsive to the user. So, you need to perform one or several updates per frame. 

    private var processing:Boolean;      // are we in the middle of processing
    private var lastIndex:int;           // where did we finish last time
    var objects:Vector.<UpdatingObject>; // the total list of objects to update
    function startProcess():Boolean {
        if (processing) return false; // already processing - please wait
        startProcessing=true;
        lastIndex=0;
        if (!hasEventListener(Event.ENTER_FRAME,iterate)) 
            addEventListener(Event.ENTER_FRAME,iterate); // enable iterating via listener
    }
    private function iterate(e:Event):void {
        if (!processing) return; // not processing - skip listener
        objects[lastIndex].update(); // perform a quantum of the big process
        lastIndex++; // advance in the big process
        if (lastIndex==objects.length) {
            processing=false; // finished, clear flag
        }
    }

Advanced processing can include using `getTimer()` to check elapsed time and allowing another iteration if time didn't run out, splitting `update()` into several functions if updating is too long, displaying progress elsewhere, adjusting iteration process to adapt to changes of the list of objects to process, and many more.

