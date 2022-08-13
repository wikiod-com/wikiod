---
title: "Adding background music and sound effects"
slug: "adding-background-music-and-sound-effects"
draft: false
images: []
weight: 9909
type: docs
toc: true
---

Try to play music in '.wav' instead of '.mp3'.In '.mp3' the music lags.

## Example to add music in pygame
    import pygame
    file = 'some.mp3'
    pygame.init()
    pygame.mixer.init()
    pygame.mixer.music.load(file)
    pygame.mixer.music.play(-1) # If the loops is -1 then the music will repeat indefinitely.


## Example to add music playlist in pygame
    import pygame
    import time
    
    pygame.mixer.init()
    pygame.display.init()
    
    screen = pygame.display.set_mode ( ( 420 , 240 ) )
    
    playlist = list()
    playlist.append ( "music3.mp3" )
    playlist.append ( "music2.mp3" )
    playlist.append ( "music1.mp3" )
    
    pygame.mixer.music.load ( playlist.pop() )  # Get the first track from the playlist
    pygame.mixer.music.queue ( playlist.pop() ) # Queue the 2nd song
    pygame.mixer.music.set_endevent ( pygame.USEREVENT )    # Setup the end track event
    pygame.mixer.music.play()           # Play the music
    
    running = True
    while running:
       for event in pygame.event.get():
          if event.type == pygame.USEREVENT:    # A track has ended
             if len ( playlist ) > 0:       # If there are more tracks in the queue...
                pygame.mixer.music.queue ( playlist.pop() ) # Q

