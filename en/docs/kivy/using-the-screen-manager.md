---
title: "Using the Screen Manager"
slug: "using-the-screen-manager"
draft: false
images: []
weight: 9910
type: docs
toc: true
---

# Circular Imports #
This is a big issue in Kivy, Python, and many programming languages

When one resource is required by two files, it is normal to place this resource in the file that will be using it most. But if this happens with two resources, and they end up in opposite files, then importing both into Python will result in a circular import. 

Python will import the first file, but this file imports the second. In the second, this imports the first file, which in turn imports the second and so on. Python throws the error `ImportError : cannot import name <classname>`

This can be solved by using a third file, and importing this third file into the first two. This is `resources.py` in the second example.

## Screen Manager
In the following example there are 2 Screens:  SettingsScreen  and MenuScreen

Using the first button, on the current screen will change your screen to the other screen.

Here is the code:


    from kivy.app import App
    from kivy.lang import Builder
    from kivy.uix.screenmanager import ScreenManager, Screen
    
    # Create both screens. Please note the root.manager.current: this is how
    # you can control the ScreenManager from kv. Each screen has by default a
    # property manager that gives you the instance of the ScreenManager used.
    Builder.load_string("""
    <MenuScreen>:
        BoxLayout:
            Button:
                text: 'First Button on Menu'
                on_press: root.manager.current = 'settings'
            Button:
                text: 'Second Button on Menu'
    
    <SettingsScreen>:
        BoxLayout:
            Button:
                text: 'First Button on Settings'
                on_press: root.manager.current = 'menu'
            Button:
                text: 'Second Button on Settings'
    
    """)
    
    # Declare both screens
    class MenuScreen(Screen):
        pass
    
    class SettingsScreen(Screen):
        pass
    
    # Create the screen manager
    sm = ScreenManager()
    sm.add_widget(MenuScreen(name='menu'))
    sm.add_widget(SettingsScreen(name='settings'))
    
    
    class TestApp(App):
    
        def build(self):
            return sm
    
    if __name__ == '__main__':
        TestApp().run()



## Simple Screen Manager Usage
    # A line used mostly as the first one, imports App class
    # that is used to get a window and launch the application
    from kivy.app import App
    
    # Casual Kivy widgets that reside in kivy.uix
    from kivy.uix.label import Label
    from kivy.uix.button import Button
    from kivy.uix.boxlayout import BoxLayout
    from kivy.uix.screenmanager import ScreenManager, Screen
    from kivy.uix.screenmanager import SlideTransition
    
    # Inherit Screen class and make it look like
    # a simple page with navigation

    class CustomScreen(Screen):
    
        # It's necessary to initialize a widget the class inherits
        # from to access its methods such as 'add_widget' with 'super()'

        def __init__(self, **kwargs):
            # Py2/Py3 note: although in Py3 'super()' is simplified
            # it's a good practice to use Py2 syntax, so that the
            # code is compatibile in both versions
            super(CustomScreen, self).__init__(**kwargs)
    
            # Put a layout in the Screen which will take
            # Screen's size and pos.
    
            # The 'orientation' represents a direction
            # in which the widgets are added into the
            # BoxLayout - 'horizontal' is the default
            layout = BoxLayout(orientation='vertical')
    
            # Add a Label with the name of Screen
            # and set its size to 50px
            layout.add_widget(Label(text=self.name, font_size=50))
    
            # Add another layout to handle the navigation
            # and set the height of navigation to 20%
            # of the CustomScreen
            navig = BoxLayout(size_hint_y=0.2)
    
            # Create buttons with a custom text
            prev = Button(text='Previous')
            next = Button(text='Next')
    
            # Bind to 'on_release' events of Buttons
            prev.bind(on_release=self.switch_prev)
            next.bind(on_release=self.switch_next)
    
            # Add buttons to navigation
            # and the navigation to layout
            navig.add_widget(prev)
            navig.add_widget(next)
            layout.add_widget(navig)
    
            # And add the layout to the Screen
            self.add_widget(layout)
    
        # *args is used to catch arguments that are returned
        # when 'on_release' event is dispatched

        def switch_prev(self, *args):
            # 'self.manager' holds a reference to ScreenManager object
            # and 'ScreenManager.current' is a name of a visible Screen
            # Methods 'ScreenManager.previous()' and 'ScreenManager.next()'
            # return a string of a previous/next Screen's name
            self.manager.transition = SlideTransition(direction="right")
            self.manager.current = self.manager.previous()
    
        def switch_next(self, *args):
            self.manager.transition = SlideTransition(direction="right")
            self.manager.current = self.manager.next()
     
    
    class ScreenManagerApp(App):
    
        # 'build' is a method of App used in the framework it's
        # expected that the method returns an object of a Kivy widget

        def build(self):
            # Get an object of some widget that will be the core
            # of the application - in this case ScreenManager
            root = ScreenManager()
    
            # Add 4 CustomScreens with name 'Screen <order>`
            for x in range(4):
                root.add_widget(CustomScreen(name='Screen %d' % x))
    
            # Return the object
            return root
    
    
    # This is only a protection, so that if the file
    # is imported it won't try to launch another App

    if __name__ == '__main__':
        # And run the App with its method 'run'
        ScreenManagerApp().run()



