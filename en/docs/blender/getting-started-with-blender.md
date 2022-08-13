---
title: "Getting started with blender"
slug: "getting-started-with-blender"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World! (Add-On)
    # not all of this is required, but just here for reference
    bl_info = {
        "name": "Hello World",                          # name of the add-on
        "author": "Blender developer",                  # name of the author
        "version": (1, 0),                              # version number for the add-on
        "blender": (2, 78, 0),                          # version of Blender the add-on is compatible with
        "location": "Space > Hello World",              # where the user can find the add-on
        "description": "Greets something",              # add-on description
        "warning": "Beta version",                      # whatever the user needs to be warned about
        "wiki_url": "",                                 # documentation link
        "category": "Development",                      # add-on category
        }
    
    # the blender python module
    import bpy
    # this is just for convenience - could just use as bpy.props.StringProperty, but there are normally lots of properties
    from bpy.props import StringProperty
    
    
    class HelloWorld(bpy.types.Operator):
        """Says hello to the world."""       # python docstring 
        bl_idname = "greetings.hello_world"  # this will be callable with bpy.ops.greetings.hello_world()
        bl_label = "Hello World"             # the user-friendly name for this operator (e.g., in a button)
        bl_options = {'REGISTER', 'UNDO'}    # 'UNDO' is only here for reference (you can't actually take back what you say)
    
        name = StringProperty(
                name="name",
                default="world",
                description="Who to say hello to",
                )
    
        def execute(self, context):
            # Note: The execute method is called when the user clicks on the operator or calls it from python.
    
            message = "Hello, " + self.name + "!"
            
            # print to console
            print(message)
            
            # show a popup that automatically goes away (in info area's header)
            self.report({'INFO'}, message)
            
            # show a popup under the cursor that doesn't go away automatically
            self.report({'ERROR'}, message)
    
            # generally return {'FINISHED'} or {'CANCELLED'} at the end of the execute method
            return {'FINISHED'}
    
    
    # this is automatically called when the add-on is enabled
    def register():
        # simply register the class
        bpy.utils.register_class(HelloWorld)
        
    
    # this is automatically called when the add-on is disabled
    def unregister():
        # simply unregister the class
        bpy.utils.unregister_class(HelloWorld)
    
    
    # common "pythonic" approach to main()...call register() automatically
    if __name__ == "__main__":
        register()


Save this in an python file (.py), then install as a regular add-on in Blender. Type "space" in just about any area in Blender and type "Hello World" to find the operator you built.

## The viewport and its windows
Blender's **viewport** is a dynamic, changeable interface composed of many different **windows**. With the program running by default, the **viewport** is composed of 5 different windows. Windows can be identified by looking for their small square indicator icons either in the top or bottom-left corner. They may look like these:

[![the 3D view icon][1]][1] (the 3D view icon)

[![the timeline icon][2]][2] (the timeline icon)

[![the properties icon][3]][3] (the properties icon)

These small images denote the type of window they're attached to, and the window type can be changed by clicking on one of them and choosing another window.

All of the windows are **resizable** and **splittable**, meaning that they can each be split into two pieces, changed in size, or be combined together into one window.

To try this functionality, first take note of the location of the **screen layout selector** which appears at the very top of the screen just after the menu buttons:

[![screen layout selector][4]][4]

This selector will let you get back to the default window layout at any time, and acts (like many of Blender's selectors) as a dynamic list. This means that if you'd like to save this layout before you start experimenting, click the **+** button to copy the layout, and then make your changes. Return to the layout by clicking the layout name and selecting the default again.

Now that the layout can be returned to, drag one of the **grab handles** at the corner of the main window - it looks like this:

[![grab handle image][5]][5]

Your cursor will transform into a crosshair and the window will split in half.

[![window splitting example][6]][6]

Combining two windows together can be done with the **grab handle** from the opposite corner of the window. In the image above, the grab handle used to split the window was in the *bottom-left* corner: thus the **grab handle** used to combine the window with another is the one in the *top-right*.

To combine the window with another, just drag this second **grab handle** in the direction you want to combine. You may have to pull it away from itself first, if you want to collapse it inwards, like so:

[![combining window example][7]][7]


  [1]: https://i.stack.imgur.com/hNeHZ.png
  [2]: https://i.stack.imgur.com/hV2t1.png
  [3]: https://i.stack.imgur.com/eNSB3.png
  [4]: https://i.stack.imgur.com/LQoHF.png
  [5]: https://i.stack.imgur.com/Goprv.png
  [6]: https://i.stack.imgur.com/frf9x.gif
  [7]: https://i.stack.imgur.com/A6zaJ.gif

## Installation or Setup
 - Go to https://www.blender.org/download/
   
 - Choose your operating system
 - Click the proper mirror next to the version of blender for your operating system. You can usually just click the mirror closest to your current location. [(more info)][2]

[![Mirrors][3]][2]  

 - Also, at the bottom of the page are also links to the daily experimental builds and the source code. This can allow you to get access to the latest features (at the cost of stability).

 - You have successfully downloaded blender!


Once you have downloaded blender, to open it, simply unzip, and then open blender.exe or blender.app

  [1]: https://i.stack.imgur.com/PAeld.png
  [2]: http://blender.stackexchange.com/questions/66418/which-mirror-should-i-pick-when-downloading-blender
  [3]: https://i.stack.imgur.com/r5mOL.png

