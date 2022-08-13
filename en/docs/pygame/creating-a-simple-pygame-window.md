---
title: "Creating a simple pygame window"
slug: "creating-a-simple-pygame-window"
draft: false
images: []
weight: 9931
type: docs
toc: true
---

## The complete code
    import pygame

    pygame.init()

    WIDTH = 300
    HEIGHT = 200
    SCREEN = pygame.display.set_mode((WIDTH, HEIGHT))

    pygame.display.set_caption('My Game')

    WHITE = (255, 255, 255)
    BLACK = (0, 0, 0)
    RED = (255, 0, 0)
    GREEN = (0, 255, 0)
    BLUE = (0, 0, 255)
    YELLOW = (255, 255, 255)
 
    SCREEN.fill(RED)
    pygame.display.flip()

    is_running = True
    while is_running:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                is_running = False

    pygame.quit()



**Importing and initialising pygame**

As we do with any module in python we need to import pygame:
    
    import pygame

We then initialise all the imported pygame modules:
 
    pygame.init()

This is used to initialise all of the pygame modules. Without this the modules would not work

**Defining Constants**

We then define some constants here:

    WIDTH = 300
    HEIGHT = 200
    SCREEN = pygame.display.set_mode((WIDTH, HEIGHT))

The `WIDTH` and `HEIGHT` constants are used to create a window, which would have a width of 300 pixels and a height of 200 pixels.
The function used in `SCREEN`, `pygame.display.set_mode((WIDTH, HEIGHT))`, will set the mode of the display and return a [Surface object][1]. Note how the parameters for this function are the `WIDTH` and `HEIGHT` constants defined earlier.

**Setting the name of the window**

We then use this function to change the name of the window to My Game:

    pygame.display.set_caption('My Game')

**Defining colours**

Afterwards we define 6 colours that can be used in our window:

    WHITE = (255, 255, 255)
    BLACK = (0, 0, 0)
    RED = (255, 0, 0)
    GREEN = (0, 255, 0)
    BLUE = (0, 0, 255)
    YELLOW = (255, 255, 255)
When defining colours we put in 3 values that range between 0 and 255. The [pygame.Color][2] class normally goes by this format:

    COLOUR = (r, g, b, a)
Where the r parameter sets the red value of the colour, the g parameter sets the green value of the colour and the b parameter sets the blue value of the colour. The a parameter sets the alpha value of the colour.

We then give this command:

    SCREEN.fill(RED)

This is a [pygame.Surface.fill][3] function which fills the Surface object, our screen, with the colour red.

**Using [pygame.display.flip()][4]**

We then use this function

    pygame.display.flip()
This basically makes everything we have drawn on the screen Surface become visible and updates the contents of the entire display. Without this line, the user wouldn't see anything on their pygame screen.

**The game loop**

The next few lines are what's called a "game loop".

To start this off we make a variable and make it True:

    is_running = True

So that we can start off our while loop:

    while is_running:

 which will be running throughout the whole game.

In it's most basic form, pygame has "events" which takes user input, for example a button press or mouse click. Pygame handles these events through an event queue. We can get these events from the event queue with this for loop:

    for event in pygame.event.get(): 

Which basically goes through a list of events, our event queue. These are the next 2 lines:

    if event.type == pygame.QUIT:
        is_running = False    
This will make it so that when the user presses the exit button in the top corner, the event with the type `pygame.QUIT` occurs. 

This then ends the while loop, as `is_running` is now `False` and the script moves on to the final line:

    pygame.quit()
Which uninitialises the pygame modules.

   


  [1]: http://www.pygame.org/docs/ref/surface.html
  [2]: http://www.pygame.org/docs/ref/color.html
  [3]: http://www.pygame.org/docs/ref/surface.html#pygame.Surface.fill
  [4]: http://www.pygame.org/docs/ref/display.html#pygame.display.flip

