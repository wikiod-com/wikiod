---
title: "Creating a pygame window"
slug: "creating-a-pygame-window"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

If you want to have other colours as the background, then name a new variable such as `red = (255,0,0)` and change the `display.fill(black)` to `display.fill(red)`. You can create colours by storing them in a variable and checking their RGB values from the internet. 

## Creating the pygame window
    import pygame
    
    background_colour = (255,255,255) # For the background color of your window
    (width, height) = (300, 200) # Dimension of the window
    
    screen = pygame.display.set_mode((width, height)) # Making of the screen
    pygame.display.set_caption('Tutorial 1') # Name for the window
    screen.fill(background_colour) #This syntax fills the background colour
    
    pygame.display.flip()
    
    running = True
    while running:
      for event in pygame.event.get():
        if event.type == pygame.QUIT:
          running = False
          pygame.quit()

