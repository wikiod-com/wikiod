---
title: "PICO-8"
slug: "pico-8"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

The PICO-8 is a fantasy console programmed in embedded Lua. It already has [good documentation](http://www.lexaloffle.com/pico-8.php?page=manual). Use this topic to demonstrate undocumented or under-documented features.

## Mouse input
Although it's not officially supported, you can use [mouse input](http://www.lexaloffle.com/bbs/?tid=3549) in your games: 

    function _update60()
      x = stat(32)
      y = stat(33)
      
      if (x>0 and x<=128 and
          y>0 and y<=128) 
      then
         
        -- left button
        if (band(stat(34),1)==1) then
          ball_x=x
          ball_y=y
        end
      end
        
      -- right button
      if (band(stat(34),2)==2) then
        ball_c+=1
        ball_c%=16
      end
      
      -- middle button
      if (band(stat(34),4)==4) then
        ball_r+=1
        ball_r%=64
      end
    end
    
    function _init()
      ball_x=63
      ball_y=63
      ball_c=10
      ball_r=1
    end
    
    function _draw()
      cls()
      print(stat(34),1,1)
      circ(ball_x,ball_y,ball_r,ball_c)
      pset(x,y,7) -- white
    end




## Game modes
If you want a title screen or an endgame screen, consider setting up a mode switching mechanism:

    function _init()
      mode = 1
    end
    
    function _update()
      if (mode == 1) then
        if (btnp(5)) mode = 2
      elseif (mode == 2) then
        if (btnp(5)) mode = 3
      end
    end
    
    function _draw()
      cls()
      if (mode == 1) then
        title()
      elseif (mode == 2) then
        print("press 'x' to win")
      else
        end_screen()
      end 
    end
    
    function title()
      print("press 'x' to start game")
    end
    
    function end_screen()
      print("a winner is you")
    end

## Game loop
It's entirely possible to use PICO-8 as an [interactive shell](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop), but you probably want to tap into the game loop. In order to do that, you must create at least one of these callback functions:

* `_update()`
* `_update60()` (after [v0.1.8](http://www.lexaloffle.com/bbs/?tid=3706))
* `_draw()`

A minimal "game" might simply draw something on the screen: 

    function _draw()
      cls()
      print("a winner is you")
    end

If you define `_update60()`, the game loop tries to run at 60fps and ignores `update()` (which runs at 30fps). Either update function is called before `_draw()`. If the system detects dropped frames, it'll skip the draw function every other frame, so it's best to keep game logic and player input in the update function:

    function _init()
      x = 63
      y = 63
    
      cls()  
    end
    
    function _update()
      local dx = 0 dy = 0
    
      if (btn(0)) dx-=1
      if (btn(1)) dx+=1 
      if (btn(2)) dy-=1 
      if (btn(3)) dy+=1 
    
      x+=dx
      y+=dy
      x%=128
      y%=128
    end
    
    function _draw()
      pset(x,y)
    end

The `_init()` function is, strictly speaking, optional as commands outside of any function are run at startup. But it's a handy way to reset the game to initial conditions without rebooting the cartridge:

    if (btn(4)) _init()


