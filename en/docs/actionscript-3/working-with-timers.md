---
title: "Working with Timers"
slug: "working-with-timers"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Countdown timer example
    package {
    import flash.events.TimerEvent;
    import flash.utils.Timer;
    
    public class CountdownTimer extends Timer {
    
        public var time:Number = 0;
    
        public function CountdownTimer(time:Number = Number.NEGATIVE_INFINITY, delay:Number = 1000) {
            super(delay, repeatCount);
    
            if (!isNaN(time))
                this.time = time;
    
            repeatCount = Math.ceil(time / delay);
    
            addEventListener(TimerEvent.TIMER, timerHandler);
            addEventListener(TimerEvent.TIMER_COMPLETE, timerCompleteHandler);
        }
    
        override public function start():void {
            super.start();
        }
    
        protected function timerHandler(event:TimerEvent):void {
            time -= delay;
        }
    
        protected function timerCompleteHandler(event:TimerEvent):void {
        }
    
        override public function stop():void {
            super.stop();
        }
    
        public function dispose():void {
            removeEventListener(TimerEvent.TIMER, timerHandler);
            removeEventListener(TimerEvent.TIMER_COMPLETE, timerCompleteHandler);
        }
    
    }
    }

This `CountdownTimer` extends `Timer`, and is used exactly the same, except that time counts down.

Example usage:

    var timer:CountdownTimer = new CountdownTimer(5000);
    timer.addEventListener(TimerEvent.TIMER, timerHandler);
    timer.addEventListener(TimerEvent.TIMER_COMPLETE, completeHandler);
    timer.start();

    function timerHandler(event:TimerEvent):void {
        trace("Time remaining: " + event.target.time);
    }

    function completeHandler(event:TimerEvent):void {
        trace("Timer complete");
    }

Above example would output:

    [trace] Time remaining: 4000
    [trace] Time remaining: 3000
    [trace] Time remaining: 2000
    [trace] Time remaining: 1000
    [trace] Time remaining: 0
    [trace] Timer complete

## Random timer example
    package {
        import flash.events.TimerEvent;
        import flash.events.TimerEvent;
        import flash.utils.Timer;
    
        public class RandomTimer extends Timer {
    
            public var minimumDelay:Number;
            public var maximumDelay:Number;
            private var _count:uint = 0;
            private var _repeatCount:int = 0;
    
            public function RandomTimer(min:Number, max:Number, repeatCount:int = 0) {
                super(delay, repeatCount);
    
                minimumDelay = min;
                maximumDelay = max;
                _repeatCount = repeatCount;
            }
    
            override public function start():void {
                delay = nextDelay();
                addEventListener(TimerEvent.TIMER, timerHandler);
                super.start();
            }
    
            private function nextDelay():Number {
                return (minimumDelay + (Math.random() * (maximumDelay - minimumDelay)));
            }
    
            override public function stop():void {
                removeEventListener(TimerEvent.TIMER, timerHandler);
                super.stop();
            }
    
            protected function timerHandler(event:TimerEvent):void {
                _count++;
                if ((_repeatCount > 0) && (_count >= _repeatCount)) {
                    stop();
                    dispatchEvent(new TimerEvent(TimerEvent.TIMER_COMPLETE));
                }
                delay = nextDelay();
            }
            
            override public function reset():void {
                _count = 0;
                super.reset();
            }
        }
    }

This `RandomTimer` extends `Timer`, and is used exactly the same, except that it dispatches at random intervals.

Example usage, dispatching randomly between 1 and 5-seconds:

    var t:int = getTimer();

    var timer:RandomTimer = new RandomTimer(1000, 5000);
    timer.addEventListener(TimerEvent.TIMER, timerHandler);
    timer.start();

    function timerHandler(event:TimerEvent):void {
        trace("Time since last dispatch: " + (getTimer() - t));
        t = getTimer();
    }

Above example would output:

    [trace] Time since last dispatch: 1374
    [trace] Time since last dispatch: 2459
    [trace] Time since last dispatch: 3582
    [trace] Time since last dispatch: 1335
    [trace] Time since last dispatch: 4249

## Intervals and timeouts
    import flash.utils.*;
    var intervalId:uint=setInterval(schroedingerCat,1000); 
    // execute a function once per second and gather interval ID
    trace("Cat's been closed in the box.");
    function schroedingerCat():void {
        if (Math.random()<0.04) {
            clearInterval(intervalId); // stop repeating by ID
            trace("Cat's dead.");
            return;
        }
        trace("Cat's still alive...");
    }

    var bombId:uint;
    function plantBomb(seconds:Number):uint {
        trace("The bomb has been planted, and will blow in "+seconds.toFixed(3)+" seconds!");
        var id:uint=setTimeout(boom,seconds*1000); // parameter is in milliseconds
        return id;
    }
    function defuseBomb(id:uint):void {
        clearTimeout(id);
        trace("Bomb with id",id,"defused!");
    }
    function boom():void {
        trace("BOOM!");
    }

[`setInterval()`](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/utils/package.html#setInterval%28%29) is used to perform repeated tasks asynchronously as specified intervals. Internal `Timer` object is used, the returned value of type `uint` is its internal ID, by which you can access and stop the repeating by calling `clearInterval()`. `setTimeout()` and `clearTimeout()` work similarly, but the call to supplied function is done only once. You can supply additional arguments to both set functions, these will be passed to the function in order. The number of arguments and their type is not checked at compile time, so should you supply a weird combination of arguments, or a function that requires them and receives none, an error "Error #1063: Argument count mismatch" is raised. 

You can perform both activities of `setInterval` and `setTimeout` with regular `Timer` objects, by either using 0 or 1 for `repeatCount` property, 0 for indefinite repeats, 1 for one.

