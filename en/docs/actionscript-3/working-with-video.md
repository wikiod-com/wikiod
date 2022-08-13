---
title: "Working with Video"
slug: "working-with-video"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Load and play external video file
-------------
**reference** : **[`NetConnection`](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/net/NetConnection.html)** , **[`NetStream`](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/net/NetStream.html)** , **[`Video`](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/media/Video.html)**

**related topics** : **[Working with Sound](https://www.wikiod.com/actionscript-3/working-with-sound)**

-------------

Basic example of playing an external video file (FLV, MP4, F4V). Code will also play M4A audio files.

    var nc:NetConnection = new NetConnection();
    nc.connect(null);
    
    var ns:NetStream = new NetStream(nc);
    
    var myVideo:Video = new Video();
    addChild(myVideo);
    
    myVideo.attachNetStream(ns);
    
    ns.play("http://www.yourwebsite.com/somefile.mp4");

<br>

Notice the code used a **[`nc.connect.null`](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/net/NetConnection.html#connect())**? This is because, in this case, there is no need to create a two-way peer to peer connection (eg: as expected in a video chat app) since we are playing a stored file.

By setting a **[`nc.connect.null`](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/net/NetConnection.html#connect())** it is required to provide a link to a file that is either  on a web server or one that is local (same location/folder) to the running SWF.

 - For a **web** file use :
   `ns.play("http://www.yourwebsite.com/somefile.mp4");`
 - For a **local** file use :
   `ns.play("somefile.mp4");`

## With NetStatusEvent
    package {
        import flash.events.NetStatusEvent;
        import flash.net.NetStream;
        import flash.net.NetConnection;
        import flash.events.Event;
        import flash.media.Video;
        import flash.display.Sprite;
        public class VideoWithNetStatus extends Sprite {
            private var video:Video = new Video();
            private var nc:NetConnection;
            private var ns:NetStream;
            
            public function VideoWithNetStatus() {
                nc = new NetConnection();
                nc.addEventListener(NetStatusEvent.NET_STATUS, onStatus);
                nc.connect(null);//or media server url
            }
     
             private function onStatus(e:NetStatusEvent):void{
                 switch(e.info.code){
                     case 'NetConnection.Connect.Success':
                         connectStream();
                     break;
                     default:
                         trace(e.info.code);//to see any unhadled events   
                 }
             }
             private function connectStream():void{
                 ns = new NetStream(nc);
                 ns.addEventListener(NetStatusEvent.NET_STATUS, onStatus);
                 addChild(video);
                 video.attachNetStream(ns);
                 ns.play('url/to/video.flv');
             }
        }
    }

