---
title: "Creating a window in pygame - pygame.display.set_mode()"
slug: "creating-a-window-in-pygame---pygamedisplayset_mode"
draft: false
images: []
weight: 9903
type: docs
toc: true
---

## Syntax
- pygame.display.set_mode(resolution=(0,0), flags=0, depth=0) # Returns a pygame.Surface representing the window on screen
- flags = pygame.FULLSCREEN | pygame.OPENGL # Flags can be combined using the "|" (bitwise OR or "pipe") character.

## Parameters
| parameter | explaination|
| ---- | ---- |
| resolution | a pair of numbers representing the width and height of the window |
| flags | additional options that change the type of window - see "Remarks" for avaliable flags |
| depth | amount of bits used for color



* Possible values for the `flag` arguments are:

| flag | description |
 --- | --- 
| pygame.FULLSCREEN | window is  fullscreen|
| pygame.RESIZABLE | window is resizeable
| pygame.NOFRAME | window has no border or controls
| pygame.DOUBLEBUF | use double buffer - recommended for `HWSURFACE` or `OPENGL`
| pygame.HWSURFACE | window is hardware accelerated, only possible in combination with `FULLSCREEN`
| pygame.OPENGL | window is renderable by OpenGL

Other remarks:

 * Pygame can currently only handle one single window at a time. Creating a second window by calling `pygame.display.set_mode((x,y))` a second time will close the first window.
 * Changing the `depths` argument is almost never required - pygame will select the best one by itself. In case a depth that is not supported by the system is set, pygame will emulate this depth, which can be very slow.

* Things that are drawn onto the surface returned by `pygame.display.set_mode()` are not immediately visible on screen - the display first has to be flipped using `pygame.display.update()` or `pygame.display.flip()`.

## Create a pygame window
This creates a window in fullscreen with size 500x500 pixels:
    
    pygame.init()
    screen = pygame.display.set_mode((500, 500), pygame.FULLSCREEN)

`screen` represents from now on the window on screen; it is a pygame.Surface object. Anything that should be come visible to the user has to be drawn onto it using `screen.blit`.


