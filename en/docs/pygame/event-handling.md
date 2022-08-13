---
title: "Event handling"
slug: "event-handling"
draft: false
images: []
weight: 9659
type: docs
toc: true
---

## Event loop
Pygame will register all events from the user into an event queue which can be received with the code `pygame.event.get()`. Every element in this queue is an `Event` object and they'll all have the attribute `type`, which is an integer representing what kind of event it is. In the pygame module there are predefined integer constants representing the type. Except for this attribute, events have different attributes.


| Constant name | Attributes |
| ------ | ------ |
| QUIT   | none   |
| ACTIVEEVENT   | gain, state   |
| KEYDOWN   | unicode, key, mod   |
| KEYUP   | key, mod    |
| MOUSEMOTION   |  pos, rel, buttons   |
|  MOUSEBUTTONUP   |  pos, button   |
| MOUSEBUTTONDOWN   | pos, button  |
|  JOYAXISMOTION   |joy, axis, value  |
| JOYBALLMOTION   |  joy, ball, rel   |
| JOYHATMOTION   |  joy, hat, value   |
| JOYBUTTONUP   |  joy, button    |
| JOYBUTTONDOWN   | joy, button   |
| VIDEORESIZE   |  size, w, h   |
| VIDEOEXPOSE   | none   |
| USEREVENT   | code   |


Example
-------

To handle our events we simply loop through the queue, check what type it is (with the help of the predefined constants in the pygame module) and then perform some action. This code will check if the user has pressed the close button on the top corner of the display, and if so terminate the program.

    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            # Close the program any way you want, or troll users who want to close your program.
            raise SystemExit
        

**ATTENTION**: You have to call the event queue regularly when using pygame! Apart from fetching the available events, calling the event queue is also the way of pygame to interact with the operating system internally. If the event queue isn't called regularly, your operating system will assume that your program doesn't work correctly anymore and possibly make it look like the program crashed (in Windows the window becomes white). If you don't want to do anything with the events you should call `pygame.event.pump()` every game loop to make pygame process the events internally.


----------


Keyboard events
===============

There are two types of key events in pygame: `KEYDOWN` and `KEYUP`. These events have an attribute `key` which is an integer representing a key on the keyboard. The pygame module has predefined integer constants representing all the common keys. The constants are named with a capital `K`, an underscore and the name of the key. For example, <kbd><-</kbd> is named `K_BACKSPACE`, <kbd>a</kbd> is named `K_a` and <kbd>F4</kbd> is namned `K_F4`.

Example
-------

This code will check if the user has pressed <kbd>w</kbd>, <kbd>a</kbd>, <kbd>s</kbd> or <kbd>d</kbd>. 

    for event in pygame.event.get():
        if event.type == pygame.QUIT:  # Usually wise to be able to close your program.
            raise SystemExit
        elif event.type == pygame.KEYDOWN:
            if event.key == pygame.K_w:
                print("Player moved up!")
            elif event.key == pygame.K_a:
                print("Player moved left!")
            elif event.key == pygame.K_s:
                print("Player moved down!")
            elif event.key == pygame.K_d:
                print("Player moved right!")

Modifiers
---------

There is no integer constant for capital letters. Instead, key events have another attribute called `mod`, which is the modifiers (<kbd>shift</kbd>, <kbd>ctrl</kbd>, <kbd>alt</kbd> etc.) being pressed simultaneously as the key. The `mod` attribute is an integer representing the modifier being pressed. Each modifier's integer value are stored in the pygame module under the name of `KMOD_` and their name. For example, <kbd>Left shift</kbd> is named `KMOD_LSHIFT`, <kbd>Tab</kbd> is named `KMOD_TAB` and <kbd>Ctrl</kbd> is named `KMOD_CTRL`.

Example
-------

This code will check wether the user pressed <kbd>a</kbd>, <kbd>Left shift</kbd> + <kbd>a</kbd> or <kbd>Caps</kbd> + <kbd>a</kbd>.

    for event in pygame.event.get():
        if event.type == pygame.QUIT:  # It's still wise to be able to close your program.
            raise SystemExit
        elif event.type == pygame.KEYDOWN:
            if event.key == pygame.K_a:
                if event.mod == 0:  # No modifier.
                    print("You pressed 'a'")
                elif event.mod == pygame.KMOD_LSHIFT or event.mod == pygame.KMOD_CAPS:
                    print("You pressed 'A'")
                else:
                    print("You pressed 'a' with another modifier than right shift or caps.")


----------


Mouse events
============

There are three types of mouse events in pygame `MOUSEMOTION`, `MOUSEBUTTONDOWN` and `MOUSEBUTTONUP`. Pygame will register these events when a display mode has been set. 


**`MOUSEMOTION`** is received when the user moves his or her mouse in the display. It has the attributes `buttons`, `pos` and `rel`.
 
 - *`buttons`* is a tuple representing if the mouse buttons (`left`, `mouse-wheel`, `right`) are being pressed or not.
 - *`pos`* is the absolute position (`x`, `y`) of the cursor in pixels.
 - *`rel`* is the position relative to the previous position (`rel_x`, `rel_y`) in pixels.


**`MOUSEBUTTONDOWN`** and **`MOUSEBUTTONUP`** is received when the user presses or releases a mouse button. They have the attributes `button` and `pos`.

 - *`button`* is an integer representing the button being pressed. `1` for left button, 2 for mouse-wheel and `3` for right button.
 - *`pos`* is the absolute position of the mouse (`x`, `y`) when the user pressed the mouse button.

Example
-------

Here's a short example using some of the attributes of each mouse event:

    for event in pygame.event.get():
        if event.type == pygame.QUIT:  # Close your program if the user wants to quit.
            raise SystemExit
        elif event.type == pygame.MOUSEMOTION:
            if event.rel[0] > 0:  # 'rel' is a tuple (x, y). 'rel[0]' is the x-value.
                print("You're moving the mouse to the right")
            elif event.rel[1] > 0:  # pygame start y=0 at the top of the display, so higher y-values are further down.
                print("You're moving the mouse down")
        elif event.type == pygame.MOUSEBUTTONDOWN:
            if event.button == 1:
                print("You pressed the left mouse button")
            elif event.button == 3:
                print("You pressed the right mouse button")
        elif event.type == pygame.MOUSEBUTTONUP:
            print("You released the mouse button")

Since there are no predefined constants for the mouse button attribute in the pygame module, here's the values for each:

| Button| Value |
| ------ | ------ |
| Left mouse button   | 1   |
| Mouse wheel button   | 2   |
| Right mouse button   | 3   |
| Mouse wheel scroll up   | 4   |
| Mouse wheel scroll down   | 5   |

Scrolling the mouse button will generate `pygame.MOUSEBUTTONDOWN` and `pygame.MOUSEBUTTONUP` events.

## State checking
It's possible to call functions from the `pygame.key` and `pygame.mouse` module to receive the state of the key and mouse. However, it's not the recommended way to process events in pygame since there are some flaws with it:

 - You'll receive the states when the function is called, which means you might miss events between calls if the user is pressing buttons fast.
 - You cannot determine the order of the events,.
 - You still need to to call one of pygame's event functions for pygame to internally interact with the operating system, otherwise it'll warn that program has become unresponsive. The functions you can call are:

   - `pygame.event.get()` to get all events or event types (by passing the types as an argument) from the queue.
   - `pygame.event.poll()` to get one single event from the queue.
   - `pygame.event.wait()` to wait for one single event from the queue.
   - `pygame.event.clear()` to clear all events in the queue.
   - `pygame.event.pump()` to allow pygame to handle internal actions (is called implicitly by the functions above).


----------


Keyboard events
===============

The key module has a function `pygame.key.get_pressed()` which returns a list of the state of all keys. The list contains `0` for all the keys which are not pressed and `1` for all keys that are pressed. Its index in the list is defined by constants in the pygame module, all prefixed with `K_` and the key name.

    pygame.event.pump()  # Allow pygame to handle internal actions.
    key = pygame.key.get_pressed()
    if key[pygame.K_a]:
        print("You pressed 'a'")
    if key[pygame.K_F1]:
        print("You pressed 'F1'")
    if key[pygame.K_LSHIFT]:
        print("You pressed 'left shift'")
    if key[pygame.K_q]:  # Press 'q' to exit the program
        quit()

If you want to check for a single key press instead of if the key is held down, you can store the previous state of all keys in a temporary variable and check if the value changes:

    pygame.event.pump()  # Allow pygame to handle internal actions.
    key = pygame.key.get_pressed()
    if key[pygame.K_q] and not previous_key[pygame.K_q]:
        print("You pressed 'q'")
    if key[pygame.K_p] and not previous_key[pygame.K_p]:
        print("You pressed 'p'")
    previous_key = key

The statement evaluates to true only when the current key is pressed down and the previous key isn't pressed down. To check whether the user released the key you only have to switch the `not` keyword (`if not key[pygame.K_q] and previous_key[pygame.K_q]`). For this to work properly, you have to set the variable `previous_key = pygame.key.get_pressed()` before the game loop, otherwise you'll receive a `NameError`.

Mouse events
============

The mouse module has functions that lets us check and set the position of the mouse as well as check the buttons pressed. The function `pygame.mouse.get_pressed()` returns a tuple tuple representing if the mouse buttons (left, mouse-wheel, right) is being pressed or not.

    pygame.event.pump()  # Allow pygame to handle internal actions.
    mouse_pos = pygame.mouse.get_pos()
    mouse_buttons = pygame.mouse.get_pressed()
    if mouse_pos[0] > 100:
        pygame.mouse.set_pos(10, mouse_pos[1])  # Reset the mouse's x-position to 10.
        print("YOU SHALL NOT PASS!")
    if mouse_buttons[2]:
        print("I'm right, right?")
    if mouse_buttons[0]:  # Press left mouse button to exit.
        print("Program left")
        quit()

