---
title: "Getting Started with Programming in Blender"
slug: "getting-started-with-programming-in-blender"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Whilst most of the Blender source code is written in C and C++, Extensions (Add-ons) are coded entirely in Python.

Blender comes with >90 extensions installed, but they are not all activated by default.

Blender extensions are installed and activated through the User Preferences window (accessible through the `File` menu or with the shortcut `Ctrl + Alt + u`).

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

