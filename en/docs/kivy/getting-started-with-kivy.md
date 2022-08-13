---
title: "Getting started with kivy"
slug: "getting-started-with-kivy"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Touch, grab and move
The following example creates a canvas with 2 points and 1 line in between.
You will be able to move the point and the line around.


    from kivy.app import App
    from kivy.graphics import Ellipse, Line
    from kivy.uix.boxlayout import BoxLayout
    
    
    class CustomLayout(BoxLayout):
    
        def __init__(self, **kwargs):
            super(CustomLayout, self).__init__(**kwargs)
    
            self.canvas_edge = {}
            self.canvas_nodes = {}
            self.nodesize = [25, 25]
    
            self.grabbed = {}
    
            #declare a canvas
            with self.canvas.after:
                pass
    
            self.define_nodes()
            self.canvas.add(self.canvas_nodes[0])
            self.canvas.add(self.canvas_nodes[1])
            self.define_edge()
            self.canvas.add(self.canvas_edge)
        
    
        def define_nodes(self):
            """define all the node canvas elements as a list"""
    
            self.canvas_nodes[0] = Ellipse(
                size = self.nodesize,
                pos =  [100,100]
                )
    
            self.canvas_nodes[1] = Ellipse(
                size = self.nodesize,
                pos =  [200,200]
                )
    
        def define_edge(self):
            """define an edge canvas elements"""
    
    
            self.canvas_edge = Line(
                points =  [
                    self.canvas_nodes[0].pos[0] + self.nodesize[0] / 2,
                    self.canvas_nodes[0].pos[1] + self.nodesize[1] / 2,
                    self.canvas_nodes[1].pos[0] + self.nodesize[0] / 2,
                    self.canvas_nodes[1].pos[1] + self.nodesize[1] / 2
                    ],
                joint = 'round',
                cap = 'round',
                width = 3
                )
    
    
        def on_touch_down(self, touch):
    
            for key, value in self.canvas_nodes.items():
                if (value.pos[0] - self.nodesize[0]) <= touch.pos[0] <= (value.pos[0] + self.nodesize[0]):
                    if (value.pos[1] - self.nodesize[1]) <= touch.pos[1] <= (value.pos[1] + self.nodesize[1]):
                        touch.grab(self)
                        self.grabbed = self.canvas_nodes[key]
                        return True
    
        def on_touch_move(self, touch):
    
            if touch.grab_current is self:
                self.grabbed.pos = [touch.pos[0] - self.nodesize[0] / 2, touch.pos[1] - self.nodesize[1] / 2]
                self.canvas.clear()
                self.canvas.add(self.canvas_nodes[0])
                self.canvas.add(self.canvas_nodes[1])
                self.define_edge()
                self.canvas.add(self.canvas_edge)
            else:
                # it's a normal touch
                pass
    
        def on_touch_up(self, touch):
            if touch.grab_current is self:
                # I receive my grabbed touch, I must ungrab it!
                touch.ungrab(self)
            else:
                # it's a normal touch
                pass
    
    class MainApp(App):
    
        def build(self):
            root = CustomLayout()
            return root
    
    if __name__ == '__main__':
        MainApp().run()

## Simple popup example in Kivy.
The following code illustrates how to do simple popup with Kivy.

    from kivy.app import App
    from kivy.uix.popup import Popup
    from kivy.lang import Builder
    from kivy.uix.button import Button

    Builder.load_string('''
    <SimpleButton>:
        on_press: self.fire_popup()
    <SimplePopup>:
        id:pop
        size_hint: .4, .4
        auto_dismiss: False
        title: 'Hello world!!'
        Button:
            text: 'Click here to dismiss'
            on_press: pop.dismiss()
    ''')


    class SimplePopup(Popup):
        pass

    class SimpleButton(Button):
        text = "Fire Popup !"
        def fire_popup(self):
            pops=SimplePopup()
            pops.open()

    class SampleApp(App):
        def build(self):
            return SimpleButton()

    SampleApp().run()

## Installation & Setup
# Windows

There are two options how to install Kivy:

First ensure python tools are up-to-date.
    
    python -m pip install --upgrade pip wheel setuptools

Then install the basic dependencies.

    python -m pip install docutils pygments pypiwin32 kivy.deps.sdl2 kivy.deps.glew

Although Kivy already has providers for audio & video, GStreamer is required for more advanced stuff.

    python -m pip install kivy.deps.gstreamer --extra-index-url https://kivy.org/downloads/packages/simple/

To make it simpler, `<python>` in the following text means a path to the directory with `python.exe` file.

1. Wheel

    The wheel package provides compiled Kivy, but with removed `cython` source components, which means the core code can't be recompiled using this way. Python code, however, is editable.

   The stable version of Kivy is available on pypi.

       python -m pip install kivy

    The latest version from the official repository is available through nightly-built wheels available on google drive. Visit the link in [docs](https://kivy.org/docs/installation/installation-windows.html#nightly-wheel-installation) matching your python version. After a proper wheel is downloaded, rename it to match the formatting of this example and run the command.

       python -m pip install C:\Kivy-1.9.1.dev-cp27-none-win_amd64.whl

2. Source

   There are more required dependencies needed to install Kivy from source than using the wheels, but the installation is more flexible.

   Create a new file in `<python>\Lib\distutils\distutils.cfg` with these lines to ensure a proper compiler will be used for the source code.

       [build]
       compiler = mingw32

   Then the compiler is needed. Either use some you already have installed, or download `mingwpy`. The important files such as `gcc.exe` will be located in `<python>\Scripts`.

       python -m pip install -i https://pypi.anaconda.org/carlkl/simple mingwpy

   Don't forget to set environment variables to let Kivy know what providers it should use.

       set USE_SDL2=1
       set USE_GSTREAMER=1

   Now install the additional dependencies required for the compilation.

       python -m pip install cython kivy.deps.glew_dev kivy.deps.sdl2_dev
       python -m pip install kivy.deps.gstreamer_dev --extra-index-url https://kivy.org/downloads/packages/simple/

   Check `Paths` section to ensure everything is set properly and install Kivy. Choose one of these options:

       python -m pip install C:\master.zip
       python -m pip install https://github.com/kivy/kivy/archive/master.zip

## Paths

Kivy needs an access to the binaries from some dependencies. This means the correct folders _have to_ be on the environment's `PATH` variable.

    set PATH=<python>\Tools;<python>\Scripts;<python>\share\sdl2\bin;%PATH%

This way Python IDLE IDE can be included to the path with `<python>\Lib\idlelib;`. Then write `idle` into console and IDLE will be ready to use Kivy.

### Simplify it

To avoid repetitive setting of environment variables either set each necessary path [this way](http://superuser.com/questions/737542) or make a batch(`.bat`) file with these lines placed into `<python>`:
 
    set PATH=%~dp0;%~dp0Tools;%~dp0Scripts;%~dp0share\sdl2\bin;%~dp0Lib\idlelib;%PATH%
    cmd.exe

To run Kivy project after installation run `cmd.exe` or the batch file and use `python <filename>.py`


**installation on Ubuntu**
       
  For install kivy on ubuntu with kivy example open terminal and run following command

First add ppa

     sudo add-apt-repository ppa:kivy-team/kivy
     
For install kivy

     sudo apt-get install python-kivy

For install kivy examples

     sudo apt-get install python-kivy-example

     
    



      

## Hello world in kivy.
The following code illustrates how to make 'hello world' app in kivy.To run this app in ios and android save it as main.py and use buildozer.

    from kivy.app import App
    from kivy.uix.label import Label
    from kivy.lang import Builder

    Builder.load_string('''
    <SimpleLabel>:
        text: 'Hello World'
    ''')


    class SimpleLabel(Label):
        pass

    
    class SampleApp(App):
        def build(self):
            return SimpleLabel()

    if __name__ == "__main__":
        SampleApp().run()

## RecycleView
    from kivy.app import App
    from kivy.lang import Builder
    from kivy.uix.button import Button
    
    
    items = [
        {"color":(1, 1, 1, 1), "font_size": "20sp", "text": "white",     "input_data": ["some","random","data"]},
        {"color":(.5,1, 1, 1), "font_size": "30sp", "text": "lightblue", "input_data": [1,6,3]},
        {"color":(.5,.5,1, 1), "font_size": "40sp", "text": "blue",      "input_data": [64,16,9]},
        {"color":(.5,.5,.5,1), "font_size": "70sp", "text": "gray",      "input_data": [8766,13,6]},
        {"color":(1,.5,.5, 1), "font_size": "60sp", "text": "orange",    "input_data": [9,4,6]},
        {"color":(1, 1,.5, 1), "font_size": "50sp", "text": "yellow",    "input_data": [852,958,123]}
    ]
    
    
    class MyButton(Button):
    
        def print_data(self,data):
            print(data)
    
    
    KV = '''
    
    <MyButton>:
        on_release:
            root.print_data(self.input_data)
    
    RecycleView:
        data: []
        viewclass: 'MyButton'
        RecycleBoxLayout:
            default_size_hint: 1, None
            orientation: 'vertical'
    
    '''
    
    
    class Test(App):
        def build(self):
            root = Builder.load_string(KV)
            root.data = [item for item in items]
            return root
    
    
    Test().run()



## Different ways to run a simple app and to interact with widgets
Most kivy apps start with this structure:

    from kivy.app import App
    
    class TutorialApp(App):
        def build(self):
            return 
    TutorialApp().run()


There is several way to go from here :

*All the codes below (except example 1 and 3) have the same widget and similar features, but show different way to build the app.* 

**Example 1: returning a single widget (simple Hello World App)**

    from kivy.app import App
    from kivy.uix.button import Button
    class TutorialApp(App):
        def build(self):
            return Button(text="Hello World!")
    TutorialApp().run()

**Example 2: returning several widgets + the button prints the label's text**

    from kivy.app import App
    from kivy.uix.boxlayout import BoxLayout
    from kivy.uix.label import Label
    from kivy.uix.button import Button
    
    class TutorialApp(App):
        def build(self):
            mylayout = BoxLayout(orientation="vertical")
            mylabel = Label(text= "My App")
            mybutton =Button(text="Click me!")  
            mylayout.add_widget(mylabel)
            mybutton.bind(on_press= lambda a:print(mylabel.text))
            mylayout.add_widget(mybutton)
            return mylayout
    TutorialApp().run()

**Example 3: using a class (single widget) + the button prints "My Button"**

    from kivy.app import App
    from kivy.uix.button import Button
    
    class Mybutton(Button):
        text="Click me!"
        on_press =lambda a : print("My Button")    
        
    class TutorialApp(App):
        def build(self):
            return Mybutton()
    TutorialApp().run()

**Example 4: it's the same as ex. 2 but it shows how to use a class**

    from kivy.app import App
    from kivy.uix.boxlayout import BoxLayout
    from kivy.uix.label import Label
    from kivy.uix.button import Button
    
    class MyLayout(BoxLayout):
        #You don't need to understand these 2 lines to make it work!
        def __init__(self, **kwargs):
            super(MyLayout, self).__init__(**kwargs)
            
            self.orientation="vertical"
            mylabel = Label(text= "My App")
            self.add_widget(mylabel)
            mybutton =Button(text="Click me!")
            mybutton.bind(on_press= lambda a:print(mylabel.text))
            self.add_widget(mybutton)
            
    class TutorialApp(App):
        def build(self):
            return MyLayout()
    TutorialApp().run()

With .kv language
-----------------

**Example 5: the same but showing how to use kv language *within* python**

    from kivy.app import App
    from kivy.uix.boxlayout import BoxLayout 
    # BoxLayout: it's in the python part, so you need to import it
    
    from kivy.lang import Builder
    Builder.load_string("""
    <MyLayout>
        orientation:"vertical"
        Label: # it's in the kv part, so no need to import it
            id:mylabel
            text:"My App"
        Button:
            text: "Click me!"
            on_press: print(mylabel.text)
    """)
    class MyLayout(BoxLayout):
        pass
    class TutorialApp(App):
        def build(self):
            return MyLayout()
    TutorialApp().run()


**Example 6: the same with the `kv` part in a `Tutorial.kv` file **

In .py:

    from kivy.app import App
    from kivy.uix.boxlayout import BoxLayout
    
    class MyLayout(BoxLayout):
        pass
    class TutorialApp(App):  
    #the kv file name will be Tutorial (name is before the "App")
        def build(self):
            return MyLayout()
    TutorialApp().run()

In Tutorial.kv:

    <MyLayout> # no need to import stuff in kv!
        orientation:"vertical"
        Label:
            id:mylabel
            text:"My App"
        Button:
            text: "Click me!"
            on_press: print(mylabel.text)
        
**Example 7: link to specific kv file + a def in python receiving the label.text **

In .py:

    from kivy.app import App
    from kivy.uix.boxlayout import BoxLayout
    
    class MyLayout(BoxLayout):
        def printMe(self_xx, yy):
            print(yy)
    class TutorialApp(App): 
        def build(self):
            self.load_kv('myapp.kv')
            return MyLayout()
    TutorialApp().run()

In myapp.kv:
    <MyLayout>
        orientation:"vertical"
        Label:
            id:mylabel
            text:"My App"
        Button:
            text: "Click me!"
            on_press: root.printMe(mylabel.text)

        
**Example 8: the button prints the label's text (with a def in python using `ids`(the "IDs") )**

Notice that:

 - `self_xx` from example 7 is replaced by `self`

In .py:

    from kivy.app import App
    from kivy.uix.boxlayout import BoxLayout
    
    class MyLayout(BoxLayout):
        def printMe(self):
            print(self.ids.mylabel.text)
    class TutorialApp(App):
        def build(self):
            self.load_kv('myapp.kv')
            return MyLayout()
    TutorialApp().run()

In myapp.kv:

    <MyLayout>
        orientation:"vertical"
        Label:
            id:mylabel
            text:"My App"
        Button:
            text: "Click me!"
            on_press: root.printMe()
        
        
        
**Example 9: the button prints the label's text (with a def in python using StringProperty)**

In .py:

    from kivy.app import App
    from kivy.uix.boxlayout import BoxLayout
    from kivy.properties import StringProperty
    class MyLayout(BoxLayout):
        stringProperty_mylabel= StringProperty("My App")
        def printMe(self):
            print(self.stringProperty_mylabel)
    
    class TutorialApp(App):
        def build(self):
            return MyLayout()
    TutorialApp().run()

In Tutorial.kv:

    <MyLayout>
        orientation:"vertical"
        Label:
            id:mylabel
            text:root.stringProperty_mylabel
        Button:
            text: "Click me!"
            on_press: root.printMe()
        
**Example 10: the button prints the label's text (with a def in python using ObjectProperty)**

In .py:

    from kivy.app import App
    from kivy.uix.boxlayout import BoxLayout
    from kivy.properties import ObjectProperty
    class MyLayout(BoxLayout):
        objectProperty_mylabel= ObjectProperty(None)
        def printMe(self):
            print(self.objectProperty_mylabel.text)
    
    class TutorialApp(App):
        def build(self):
            return MyLayout()
    TutorialApp().run()

In Tutorial.kv:

    <MyLayout>
        orientation:"vertical"
        objectProperty_mylabel:mylabel
        Label:
            id:mylabel
            text:"My App"
        Button:
            text: "Click me!"
            on_press: root.printMe()
            



