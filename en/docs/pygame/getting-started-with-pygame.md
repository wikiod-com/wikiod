---
title: "Getting started with pygame"
slug: "getting-started-with-pygame"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## A simple 'game'
----------


## Import and initialize ##

Every module needs to be imported and pygame is no exception. Although we need to call the function `pygame.init()` for all imported modules in pygame to be initialized properly. If we forget this some modules won't work. The function also returns a tuple of all successfully and failed initializations (it won't raise an error if a module fails to initialize).


    import pygame
    successes, failures = pygame.init()
    print("{0} successes and {1} failures".format(successes, failures))


----------


## Create necessities ##

We also need to create a display. Pygame have already created a (hidden) display, so all we need to do is to set the mode of the display (in this example we only set the resolution). It's also a good idea to create a clock to make sure our program updates at a fixed speed (otherwise it would run at different speed depending on how fast the computer is).
    
    screen = pygame.display.set_mode((720, 480))  # Notice the tuple! It's not 2 arguments.
    clock = pygame.time.Clock()
    FPS = 60  # This variable will define how many frames we update per second.

For a bit of readability later in our code we'll create two color constants, which represent a tuple of Red, Green and Blue (RGB). The values goes from 0 (no light) to 255 (full light).
    
    BLACK = (0, 0, 0)
    WHITE = (255, 255, 255)

In pygame we usually use a *Surface* to represent the appearance of an object, and a *Rect* (rectangle) to represent the position of an object. A *Surface* is like a blank sheet of paper which contain colors or images. If you're making a class you should name the attributes *image* and *rect* since many functions will look for and use those attributes. Such classes would benefit by inherit the `pygame.sprite.Sprite` class for reasons you can read up on [here][1].
  
    rect = pygame.Rect((0, 0), (32, 32))  # First tuple is position, second is size.
    image = pygame.Surface((32, 32))  # The tuple represent size.
    image.fill(WHITE)  # We fill our surface with a nice white color (by default black).
    


----------


## The game loop ##

Now we have everything set for our game loop. This is a loop that will run for the entire game, where we handle events and updates the screen and positions of our objects.

First we'll make sure our loop executes at a given *FPS*. We defined the *FPS* and created our clock in the beginning of the program. The following code will make sure our program sleeps enough time to make our loop repeat the amount we defined our *FPS* to be. In this example, 60 times per second.

    clock.tick(FPS)

Then we'll handle events. An event is basically a user action, such as moving the mouse or pressing a key. Pygame will register all these events in a queue which we get by calling `pygame.event.get()`. We can iterate over this and check if there's an event that we'd like to handle. Events have a *type* attribute which we can check against constants in the pygame module to determine what type of event it is.

    for event in pygame.event.get():
        if event.type == pygame.QUIT:  # The user pressed the close button in the top corner of the window.
            quit()
            # Close the program. Other methods like 'raise SystemExit' or 'sys.exit()'.
            # Calling 'pygame.quit()' won't close the program! It will just uninitialize the modules.


We can also check `if event.type == pygame.KEYDOWN` to see if the user has pressed a key down. In that case the event has an attribute *key* which we can check to see which key it represents.

    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            quit()
        elif event.type == pygame.KEYDOWN:
            if event.key == pygame.K_w:
                rect.move_ip(0, -2)  # Changes the rect's position.
            elif event.key == pygame.K_s:
                rect.move_ip(0, 2)
            elif event.key == pygame.K_a:
                rect.move_ip(-2, 0)
            elif event.key == pygame.K_d:
                rect.move_ip(2, 0)

Now we need to display our image. First we might want to clear our screen from previous rendering. We do so by filling our entire screen with black (remove the code to see why we want to clear it). Then we need to *blit* our *image* to the screen. Blitting essentially means copying the *image* to another surface (in our case, the screen). Lastly we *flip* or *update* the screen. 

When we're blitting we're not actually displaying anything to the user. Imagine it as the computer on one side and the user on the other. The computer draws (*blits*) on it's side of the screen, *flips* it towards the user, and then repeats.

    screen.fill(BLACK)
    screen.blit(image, rect)
    pygame.display.update()  # Or 'pygame.display.flip()'.

Now we have a basic game! Quite boring, yes, but the essentials are there! Combined this with your current Python knowledge and you should be able to create something awesome.


----------


## Complete code ##

    import pygame
    successes, failures = pygame.init()
    print("{0} successes and {1} failures".format(successes, failures))
    
    
    screen = pygame.display.set_mode((720, 480))
    clock = pygame.time.Clock()
    FPS = 60  # Frames per second.
    
    BLACK = (0, 0, 0)
    WHITE = (255, 255, 255)
    # RED = (255, 0, 0), GREEN = (0, 255, 0), BLUE = (0, 0, 255).
    
    rect = pygame.Rect((0, 0), (32, 32))
    image = pygame.Surface((32, 32))
    image .fill(WHITE)  
    
    while True:
        clock.tick(FPS)

        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                quit()
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_w:
                    rect.move_ip(0, -2)
                elif event.key == pygame.K_s:
                    rect.move_ip(0, 2)
                elif event.key == pygame.K_a:
                    rect.move_ip(-2, 0)
                elif event.key == pygame.K_d:
                    rect.move_ip(2, 0)

        screen.fill(BLACK)
        screen.blit(image, rect)
        pygame.display.update()  # Or pygame.display.flip()


----------


## Slightly improved game mechanics ##

Note that the program checks for when we press the key and not for when we're holding the key down. To fix this we could introduce a *velocity* variable. We can create a player class to keep it more organized. To avoid frame dependent movement (if we would change the FPS to 30 the objects would move at half the speed) we introduce time dependent movement by passing the time between ticks to our movable objects.

    import pygame
    
    successes, failures = pygame.init()
    print("Initializing pygame: {0} successes and {1} failures.".format(successes, failures))
    
    screen = pygame.display.set_mode((720, 480))
    clock = pygame.time.Clock()
    FPS = 60
    
    BLACK = (0, 0, 0)
    WHITE = (255, 255, 255)
    
    
    class Player(pygame.sprite.Sprite):
        def __init__(self):
            super().__init__()
            self.image = pygame.Surface((32, 32))
            self.image.fill(WHITE)
            self.rect = self.image.get_rect()  # Get rect of some size as 'image'.
            self.velocity = [0, 0]
    
        def update(self):
            self.rect.move_ip(*self.velocity)
    
    
    player = Player()
    running = True
    while running:
        dt = clock.tick(FPS) / 1000  # Returns milliseconds between each call to 'tick'. The convert time to seconds.
        screen.fill(BLACK)  # Fill the screen with background color.
    
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_w:
                    player.velocity[1] = -200 * dt  # 200 pixels per second
                elif event.key == pygame.K_s:
                    player.velocity[1] = 200 * dt
                elif event.key == pygame.K_a:
                    player.velocity[0] = -200 * dt
                elif event.key == pygame.K_d:
                    player.velocity[0] = 200 * dt
            elif event.type == pygame.KEYUP:
                if event.key == pygame.K_w or event.key == pygame.K_s:
                    player.velocity[1] = 0
                elif event.key == pygame.K_a or event.key == pygame.K_d:
                    player.velocity[0] = 0
    
        player.update()
    
        screen.blit(player.image, player.rect)
        pygame.display.update()  # Or pygame.display.flip()
    
    print("Exited the game loop. Game will quit...")
    quit()  # Not actually necessary since the script will exit anyway.

There are still many things that should be improved about this code. I'd recommend you to read the pygame [tutorial][2] and this [talk][3] by Richard Jones for more in depth.


  [1]: http://www.pygame.org/docs/tut/SpriteIntro.html
  [2]: http://www.pygame.org/docs/
  [3]: https://www.youtube.com/watch?v=bMt47wvK6u0&list=PL4Yp6gRH-R1Birdm-Gs-SdBFWLUC1q3Fa&index=4


## Installing pygame
On windows
------------

1) Navigate to http://www.lfd.uci.edu/~gohlke/pythonlibs/#pygame – an unofficial site providing windows binaries of open-source python packages for the official [CPython](https://en.wikipedia.org/wiki/CPython) distribution by *Christoph Gohlke*.

2) Download the appropriate pygame `.whl` file according to your installed python version. (The file is named something like `pygame -`*`<pygame version> - <python version>`*`- win32.whl`)

3) Run

        pip install your-pygame-package.whl

   inside your terminal, bash or consol.<br>
**Note:** if `pip` is not found in `PATH` try to run `python -m pip install your-pygame-package.whl`

4) Check if you can import pygame as a python module

       import pygame

   If you do not get an error, you have correctly installed pygame on your computer :)

On linux
------------
1) Open your terminal and run

       sudo apt-get install python-pygame

   **Note:** This will install pygame for python2
2) Try to import pygame inside

       import pygame

   If you do not get an error, you have correctly installed pygame on your linux system :)

On macOS
--------

There are two ways to install it on mac:

**Method 1**

Go to the [Pygame downloads page][1] and download the mac installer. Run it, and it should install Pygame on your Mac.

**Method 2**

Install [Homebrew][2]:

    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

Then use Homebrew to install Python 2.7.12 and Pygame:

    brew install python; brew install homebrew/python/pygame

Now run Python in your terminal and try `import pygame`. If it does not say anything, it's installed successfully.

  [1]: http://www.pygame.org/download.shtml
  [2]: http://brew.sh

## Importing pygame and drawing on a display
**Getting Started** 
===============

You must do the following to get started with Pygame:

    import pygame
This opens a window of size 640,480, and stores it in a variable called screen.

**Setting up a window name**
-----------------------
Setting up a name for the pygame window requires the following syntax:

    pygame.display.set_caption('Name')

**About the screen**
----------------

 - The point (0,0) is at the upper left hand corner of the screen.
 - x coordinates increase from left to right, y coordinates increase from top to bottom.That is the right side coordinates on the Cartesian plane is positive and left side is negative.However , the upper side coordinates on the Cartesian plane is negative on top and positive on bottom.(**Note**: This is considered if the points are taken from the origin.)

**Updating the Screen**
-------------------
Changes you make to the screen—e.g. filling it with color, or drawing on it, do not show up immediately!  
So how to do it?   
You have to call this function:

    pygame.display.update()

**Colors**
------
The coloring in pygame works on RGB mode.  
The code for coloring is:  

    color_Name = (r,g,b)

 - R stands for red.
 - G stands for green
 - B stands for blue.
 - All three should be integers between 0 and 255, with 255 being brightest, and 0 being darkest

**Drawing**
----------

 1. To draw lines  

        pygame.draw.lines(screen, color, closed, pointlist, thickness)
 2. To draw rectangle

        pygame.draw.rect(screen, color, (x,y,width,height), thickness)
3. To draw circle 

       pygame.draw.circle(screen, color, (x,y), radius, thickness)
**Setting everything into a loop**
-------------------------------
To make a loop use the following code:

    running = True
    while running:
      for event in pygame.event.get():
        if event.type == pygame.QUIT:
          running = False
          pygame.quit()

# Drawing a rectangle on pygame window (code)

    import pygame
    background_colour = (255,255,255) # White color
    (width, height) = (300, 200) # Screen size
    color=(0,0,0) #For retangle
    screen = pygame.display.set_mode((width, height)) #Setting Screen
    pygame.display.set_caption('Drawing') #Window Name
    screen.fill(background_colour)#Fills white to screen
    pygame.draw.rect(screen, color, (100,50,30,40), 1) #Drawing the rectangle
    pygame.display.update()
    
    #Loop
    running = True
    while running:
      for event in pygame.event.get():
        if event.type == pygame.QUIT:
          running = False
          pygame.quit()








