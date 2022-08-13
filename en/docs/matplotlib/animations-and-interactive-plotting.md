---
title: "Animations and interactive plotting"
slug: "animations-and-interactive-plotting"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

With python matplotlib you can properly make animated graphs.

## Basic animation with FuncAnimation
The [matplotlib.animation][1] package offer some classes for creating animations. [`FuncAnimation`][2] creates animations by repeatedly calling a function. Here we use a function `animate()` that changes the coordinates of a point on the graph of a sine function.

```
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

TWOPI = 2*np.pi

fig, ax = plt.subplots()

t = np.arange(0.0, TWOPI, 0.001)
s = np.sin(t)
l = plt.plot(t, s)

ax = plt.axis([0,TWOPI,-1,1])

redDot, = plt.plot([0], [np.sin(0)], 'ro')

def animate(i):
    redDot.set_data(i, np.sin(i))
    return redDot,

# create animation using the animate() function
myAnimation = animation.FuncAnimation(fig, animate, frames=np.arange(0.0, TWOPI, 0.1), \
                                      interval=10, blit=True, repeat=True)

plt.show()
```

[![enter image description here][3]][3]


  [1]: http://matplotlib.org/api/animation_api.html
  [2]: http://matplotlib.org/api/animation_api.html#matplotlib.animation.FuncAnimation
  [3]: http://i.stack.imgur.com/g1ayM.gif

## Save animation to gif
In this example we use the `save` method to save an `Animation` object using ImageMagick. 

```
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from matplotlib import rcParams

# make sure the full paths for ImageMagick and ffmpeg are configured
rcParams['animation.convert_path'] = r'C:\Program Files\ImageMagick\convert'
rcParams['animation.ffmpeg_path'] = r'C:\Program Files\ffmpeg\bin\ffmpeg.exe'

TWOPI = 2*np.pi

fig, ax = plt.subplots()

t = np.arange(0.0, TWOPI, 0.001)
s = np.sin(t)
l = plt.plot(t, s)

ax = plt.axis([0,TWOPI,-1,1])

redDot, = plt.plot([0], [np.sin(0)], 'ro')

def animate(i):
    redDot.set_data(i, np.sin(i))
    return redDot,

# create animation using the animate() function with no repeat
myAnimation = animation.FuncAnimation(fig, animate, frames=np.arange(0.0, TWOPI, 0.1), \
                                      interval=10, blit=True, repeat=False)

# save animation at 30 frames per second 
myAnimation.save('myAnimation.gif', writer='imagemagick', fps=30)
```

## Interactive controls with matplotlib.widgets
For interacting with plots Matplotlib offers GUI neutral [widgets][1]. Widgets require a `matplotlib.axes.Axes` object.

Here's a slider widget demo that ùpdates the amplitude of a sine curve. The update function is triggered by the slider's `on_changed()` event.

```
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from matplotlib.widgets import Slider

TWOPI = 2*np.pi

fig, ax = plt.subplots()

t = np.arange(0.0, TWOPI, 0.001)
initial_amp = .5
s = initial_amp*np.sin(t)
l, = plt.plot(t, s, lw=2)

ax = plt.axis([0,TWOPI,-1,1])

axamp = plt.axes([0.25, .03, 0.50, 0.02])
# Slider
samp = Slider(axamp, 'Amp', 0, 1, valinit=initial_amp)

def update(val):
    # amp is the current value of the slider
    amp = samp.val
    # update curve
    l.set_ydata(amp*np.sin(t))
    # redraw canvas while idle
    fig.canvas.draw_idle()

# call update function on slider value change
samp.on_changed(update)

plt.show()
```

[![enter image description here][2]][2]
Other available widgets: 

 - [AxesWidget][3]
 - [Button][4]
 - [CheckButtons][5]
 - [Cursor][6]
 - [EllipseSelector][7]
 - [Lasso][8]
 - [LassoSelector][9]
 - [LockDraw][10]
 - [MultiCursor][11]
 - [RadioButtons][12]
 - [RectangleSelector][13]
 - [SpanSelector][14]
 - [SubplotTool][15]
 - [ToolHandles][16]


  [1]: http://matplotlib.org/api/widgets_api.html#module-matplotlib.widgets
  [2]: https://i.stack.imgur.com/TJms6.gif
  [3]: http://matplotlib.org/api/widgets_api.html#matplotlib.widgets.AxesWidget
  [4]: http://matplotlib.org/api/widgets_api.html#matplotlib.widgets.Button
  [5]: http://matplotlib.org/api/widgets_api.html#matplotlib.widgets.CheckButtons
  [6]: http://matplotlib.org/api/widgets_api.html#matplotlib.widgets.Cursor
  [7]: http://matplotlib.org/api/widgets_api.html#matplotlib.widgets.EllipseSelector
  [8]: http://matplotlib.org/api/widgets_api.html#matplotlib.widgets.Lasso
  [9]: http://matplotlib.org/api/widgets_api.html#matplotlib.widgets.LassoSelector
  [10]: http://matplotlib.org/api/widgets_api.html#matplotlib.widgets.LockDraw
  [11]: http://matplotlib.org/api/widgets_api.html#matplotlib.widgets.MultiCursor
  [12]: http://matplotlib.org/api/widgets_api.html#matplotlib.widgets.RadioButtons
  [13]: http://matplotlib.org/api/widgets_api.html#matplotlib.widgets.RectangleSelector
  [14]: http://matplotlib.org/api/widgets_api.html#matplotlib.widgets.SpanSelector
  [15]: http://matplotlib.org/api/widgets_api.html#matplotlib.widgets.SubplotTool
  [16]: http://matplotlib.org/api/widgets_api.html#matplotlib.widgets.ToolHandles

## Plot live data from pipe with matplotlib
This can be usefull when you want to visualize incoming data in real-time. This data could, for example, come from a microcontroller that is continuously sampling an analog signal.

In this example we will get our data from a named pipe (also known as a fifo). For this example, the data in the pipe should be numbers separted by newline characters, but you can adapt this to your liking.

Example data:

    100
    123.5
    1589

[More information on named pipes][1]

We will also be using the datatype deque, from the standard library collections.
A deque object works quite a lot like a list. But with a deque object it is quite easy to append something to it while still keeping the deque object at a fixed length. This allows us to keep the x axis at a fixed length instead of always growing and squishing the graph together.
[More information on deque objects][2]

Choosing the right backend is vital for performance. Check what backends work on your operating system, and choose a fast one. For me only qt4agg and the default backend worked, but the default one was too slow.
[More information on backends in matplotlib][3]

This example is based on [the matplotlib example of plotting random data][4].

None of the ' characters in this code are meant to be removed.

    import matplotlib
    import collections
    #selecting the right backend, change qt4agg to your desired backend
    matplotlib.use('qt4agg')
    import matplotlib.pyplot as plt
    import matplotlib.animation as animation

    #command to open the pipe
    datapipe = open('path to your pipe','r')
    
    #amount of data to be displayed at once, this is the size of the x axis
    #increasing this amount also makes plotting slightly slower
    data_amount = 1000
    
    #set the size of the deque object
    datalist = collections.deque([0]*data_amount,data_amount)
    
    #configure the graph itself
    fig, ax = plt.subplots()
    line, = ax.plot([0,]*data_amount)

    #size of the y axis is set here
    ax.set_ylim(0,256)
    
    def update(data):
            line.set_ydata(data)
            return line,
    
    def data_gen():
        while True:
            """
            We read two data points in at once, to improve speed
            You can read more at once to increase speed
            Or you can read just one at a time for improved animation smoothness
            data from the pipe comes in as a string,
            and is seperated with a newline character,
            which is why we use respectively eval and rstrip.
            """
            datalist.append(eval((datapipe.readline()).rstrip('\n')))
            datalist.append(eval((datapipe.readline()).rstrip('\n')))
            yield datalist
    
    ani = animation.FuncAnimation(fig,update,data_gen,interval=0, blit=True)
    plt.show()

If your plot starts to get delayed after a while, try adding more of the datalist.append data, so that more lines get read each frame. Or choose a faster backend if you can.

This worked with 150hz data from a pipe on my 1.7ghz i3 4005u.


  [1]: https://en.wikipedia.org/wiki/Named_pipe
  [2]: https://docs.python.org/3.6/library/collections.html#collections.deque
  [3]: http://matplotlib.org/faq/usage_faq.html#what-is-a-backend
  [4]: http://matplotlib.org/examples/animation/random_data.html

