---
title: "The essentials"
slug: "the-essentials"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Drawing and a basic animation
This program draws some shapes and '*hello world!*' and let an image go to every corner of the window.
# the complete code:

    import pygame,sys
    from pygame.locals import *

    pygame.init()

    FPS = 30 #frames per second setting
    fpsClock = pygame.time.Clock()

    #set up the window
    screen = pygame.display.set_mode((500,400), 0, 32)
    pygame.display.set_caption('drawing')

    #set up the colors
    black = (  0,   0,   0)
    white = (255, 255, 255)
    red   = (255,   0,   0)
    green = (  0, 255,   0)
    blue  = (  0,   0, 255)

    imageImg  = pygame.image.load('baddie.png')
    imagex = 320
    imagey = 220
    direction = 'left'

    fontObj = pygame.font.Font('freesansbold.ttf', 32)
    text = fontObj.render('Hello World!', True, green, blue)
    rect = text.get_rect()
    rect.center = (200, 150)

    # the main game loop
    while True:
        screen.fill(white)

        # draw a green polygon onto the surface
        pygame.draw.polygon(screen, green, ((146, 0), (291, 106), (236, 277), (56, 277), (0, 106)))

        # draw some blue lines onto the surface
        pygame.draw.line(screen, blue, (60, 60), (120, 60), 4)
        pygame.draw.line(screen, blue, (120, 60), (60, 120))
        pygame.draw.line(screen, blue, (60, 120), (120, 120), 4)

        # draw a blue circle onto the surface
        pygame.draw.circle(screen, blue, (300, 50), 100, 0)

        # draw a red ellipse onto the surface
        pygame.draw.ellipse(screen, red, (300, 250, 80,80), 1)

        # draw a red rectangle onto the surface
        pygame.draw.rect(screen,red, (200, 150, 100, 50))

        # draw the text onto the surface
        screen.blit(text, rect)

        if direction == 'right':
            imagex += 5
            if imagex == 320:
                direction = 'down'
        elif direction == 'down':
            imagey += 5
            if imagey == 220:
                direction = 'left'
        elif direction == 'left':
            imagex -= 5
            if imagex == 20:
               direction = 'up'
        elif direction == 'up':
            imagey -= 5
            if imagey == 20:
               direction = 'right'
        screen.blit(imageImg, (imagex, imagey))

    for event in pygame.event.get():
        if event.type == QUIT:
            pygame.quit()
            sys.exit()

    pygame.display.update()
    fpsClock.tick(FPS)

# setting up pygame and the window:
    import pygame,sys
    from pygame.locals import *

    pygame.init()

    #set up the window
    screen = pygame.display.set_mode((500,400), 0, 32)
    pygame.display.set_caption('drawing')

# drawing the white background:
In this function you define the color of the background.
    
    screen.fill(white)

# drawing the green polygon:
Here you define the display surface, the color and the position of every corner of the polygon (x and y coordinates), you can do that clockwise and counterclockwise.

    pygame.draw.polygon(screen, green, ((146, 0), (291, 106), (236, 277), (56, 277), (0, 106)))

# drawing the blue lines:
In this function you define the display surface, the color, the first and the last point and the width of the line (if you don't give a width, it's just 1).

    pygame.draw.line(screen, blue, (60, 60), (120, 60), 4)
    pygame.draw.line(screen, blue, (120, 60), (60, 120))
    pygame.draw.line(screen, blue, (60, 120), (120, 120), 4)

# drawing the blue circle:
In this function you define the display surface, the color,  the position, the radius and the width of the circle (if you give a 0 for the width, it's a plain circle).

    pygame.draw.circle(screen, blue, (300, 50), 100, 0)

# drawing the ellipse:
In this function you define the display surface, the color, the position, the horizontal size and the vertical size and the width.

    pygame.draw.ellipse(screen, red, (300, 250, 80,80), 1)

# drawing the rectangle:
In this function you define the display surface, the color, the position and the horizontal and the vertical size.

    pygame.draw.rect(screen,red, (200, 150, 100, 50))

# defining the text:
First you define the type and the size of your text with this function:

    fontObj = pygame.font.Font('freesansbold.ttf', 32)

Then you define the actual text, if the text is bold, the color and, if you want, the color of the marking. You can do that with this function:

    text = fontObj.render('Hello World!', True, green, blue)

If you want to mark your text, you have to tell pygame that with this function:

    rect = text.get_rect()

And if you want to define the position of the center of the text you can do that with this function:

    rect.center = (200, 150)

# drawing the text:
If you defined a marking and/or the center:

    screen.blit(text, rect)

Otherwise you have to define the position of the text so draw the text this way:

    screen.blit(text, (100,50))

# defining the image:
Here you define which image you want to use (if you do it this way, the image file has to be in the same directory as your program file), the start position (x and y) and the direction of the image.

    image  = pygame.image.load('image.png')
    baddiex = 320
    baddiey = 220
    direction = 'left'

# animating the image:
With this part of the code we check the direction of the image, if it reached a corner, if so, change the direction, if not, draw the image 5 pixels further in the same direction.

    if direction == 'right':
        imagex += 5
        if imagex == 360:
            direction = 'down'
    elif direction == 'down':
        imagey += 5
        if imagey == 260:
            direction = 'left'
    elif direction == 'left':
        imagex -= 5
        if imagex == 20:
           direction = 'up'
    elif direction == 'up':
        imagey -= 5
        if imagey == 20:
           direction = 'right'
    screen.blit(imageImg, (imagex, imagey))

**note: My image is 20x20 pixels, I use** `if imagex == 360:` **and** `if imagey == 260:` **because then my image is 20 pixels from the edge of the window, just like the other 2 corners. If your image has a different size, you'll probably have to change those numbers.**

# checking for quit:
Here we check if you closed the pygame window, and if so, close the window, if you don't write this somewhere in your program, you probably won't be able to close the window.

    for event in pygame.event.get():
        if event.type == QUIT:
            pygame.quit()
            sys.exit()

# updating the screen:
With this function you update the screen so everything you have drawn becomes visible.

    pygame.display.update()

# FPS setting:
With this function you tell pygame to sleep enough so your FPS setting is respected.

    fpsClock.tick(FPS)


## Using with PIL
When you have to use both PIL and Pygame because missing functionalities in both of them, you need a way to convert between Pygame Surfaces and PIL Images, preferably without writing them to the disk.

For that you can use "tostring" and "fromstring" functions provided in both libraries.

Conversion from PIL to Pygame:

    strFormat = 'RGBA'
    raw_str = image.tostring("raw", strFormat)
    surface = pygame.image.fromstring(raw_str, image.size, strFormat)

Conversion from Pygame to PIL:

    strFormat = 'RGBA'
    raw_str = pygame.image.tostring(surface, strFormat, False)
    image = Image.frombytes(strFormat, surface.get_size(), raw_str)

