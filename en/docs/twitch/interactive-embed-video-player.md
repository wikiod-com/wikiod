---
title: "Interactive Embed Video Player"
slug: "interactive-embed-video-player"
draft: false
images: []
weight: 9885
type: docs
toc: true
---

## LIVE Streaming Video Player
Basic implementation:

    <script src= "http://player.twitch.tv/js/embed/v1.js"></script>
    <div id="PLAYER_DIV_ID"></div>
    <script type="text/javascript">
        var options = {
            width: 854,
            height: 480,
            channel: "monstercat", 
        };
        var player = new Twitch.Player("PLAYER_DIV_ID", options);
        player.setVolume(0.5);
    </script>

----

With controls hidden:

    <script src= "http://player.twitch.tv/js/embed/v1.js"></script>
    <div id="PLAYER_DIV_ID"></div>
    <script type="text/javascript">
        var options = {
            width: 854,
            height: 480,
            channel: "monstercat", 
            controls: false,
        };
        var player = new Twitch.Player("PLAYER_DIV_ID", options);
        player.setVolume(0.5);
    </script>


## Recorded (not live) Video Player
    <script src= "http://player.twitch.tv/js/embed/v1.js"></script>
    <div id="PLAYER_DIV_ID"></div>
    <script type="text/javascript">
        var options = {
            width: 854,
            height: 480,
            video: "v53336925", 
        };
        var player = new Twitch.Player("PLAYER_DIV_ID", options);
        player.setVolume(0.5);
    </script>

The above snippet will play the following video:
[twitch.tv/general_mittenz/**v/53336925**][1]

[1]: https://www.twitch.tv/general_mittenz/v/53336925

## Start with a Muted Player
    <script src= "http://player.twitch.tv/js/embed/v1.js"></script>
    <div id="{PLAYER_DIV_ID}"></div>
    <script type="text/javascript">
        var options = {
            width: 854,
            height: 480,
            channel: "{CHANNEL}"    
        };
        var player = new Twitch.Player("{PLAYER_DIV_ID}", options);
        player.setMuted(true);
    </script>

