---
title: "AVPlayer and AVPlayerViewController"
slug: "avplayer-and-avplayerviewcontroller"
draft: false
images: []
weight: 9790
type: docs
toc: true
---

import AVKit, import AVFoundation.

## Playing Media Using AVPlayerViewController
Objective-C
-----------

    NSURL *url = [[NSURL alloc] initWithString:@"YOUR URL"]; // url can be remote or local

    AVPlayer *player = [AVPlayer playerWithURL:url];    
    // create a player view controller

    AVPlayerViewController *controller = [[AVPlayerViewController alloc] init];
    [self presentViewController:controller animated:YES completion:nil];
    controller.player = player;
    [player play];

Swift
-----

    let player = AVPlayer(URL: url) // url can be remote or local
    
    let playerViewController = AVPlayerViewController()
    // creating a player view controller
    playerViewController.player = player
    self.presentViewController(playerViewController, animated: true) {
    
        playerViewController.player!.play()
    }



## Playing Media using AVPlayer and AVPlayerLayer
Objective C
-----------

    NSURL *url = [NSURL URLWithString:@"YOUR URL"];
    AVPlayer *player = [AVPlayer playerWithURL:videoURL];
    AVPlayerLayer *playerLayer = [AVPlayerLayer playerLayerWithPlayer:player];
    playerLayer.frame = self.view.bounds;
    [self.view.layer addSublayer:playerLayer];
    [player play];

Swift
-----

    let url = NSURL(string: "YOUR URL")
    let player = AVPlayer(URL: videoURL!)
    let playerLayer = AVPlayerLayer(player: player)
    playerLayer.frame = self.view.bounds
    self.view.layer.addSublayer(playerLayer)
    player.play()




## AVPlayer Example
AVPlayer *avPlayer = [AVPlayer playerWithURL:[NSURL URLWithString:@"YOUR URL"]];
        
        AVPlayerViewController *avPlayerCtrl = [[AVPlayerViewController alloc] init];
        avPlayerCtrl.view.frame = self.view.frame;
        avPlayerCtrl.player = avPlayer;
        avPlayerCtrl.delegate = self;
        [avPlayer play];
        [self presentViewController:avPlayerCtrl animated:YES completion:nil

