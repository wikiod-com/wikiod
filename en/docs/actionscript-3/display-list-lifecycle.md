---
title: "Display List Lifecycle"
slug: "display-list-lifecycle"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Frame-based animation in Flash and AIR implement the following lifecycle:

- `Event.ENTER_FRAME` is dispatched
- Constructor code of children display objects are executed
- `Event.FRAME_CONSTRUCTED` is dispatched
- Frame actions in the `MovieClip` symbol is executed
- Frame actions in children `MovieClip` symbols are executed
- `Event.EXIT_FRAME` is dispatched
- `Event.RENDER` is dispatched



## Added and removed from stage lifecycle
    package {
    import flash.display.Sprite;
    import flash.events.Event;

    public class Viewport extends Sprite {
    
        /** Constructor */
        public function Viewport() {
            super();

            // Listen for added to stage event
            addEventListener(Event.ADDED_TO_STAGE, addedToStageHandler);
        }
    
        /** Added to stage handler */
        protected function addedToStageHandler(event:Event):void {
            // Remove added to stage event listener
            removeEventListener(Event.ADDED_TO_STAGE, addedToStageHandler);
    
            // Listen for removed from stage event
            addEventListener(Event.REMOVED_FROM_STAGE, removedFromStageHandler);
        }

        /** Removed from stage handler */
        protected function removedFromStageHandler(event:Event):void {
            // Remove removed from stage event listener
            removeEventListener(Event.REMOVED_FROM_STAGE, removedFromStageHandler);
    
            // Listen for added to stage event
            addEventListener(Event.ADDED_TO_STAGE, addedToStageHandler);
        }
    
        /** Dispose */
        public function dispose():void {
            // Remove added to stage event listener
            removeEventListener(Event.ADDED_TO_STAGE, addedToStageHandler);
    
            // Remove removed from stage event listener
            removeEventListener(Event.REMOVED_FROM_STAGE, removedFromStageHandler);
        }
    
    }
    }

