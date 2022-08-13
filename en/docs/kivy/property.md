---
title: "Property"
slug: "property"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Difference between Properties and Binding.
In brief: 

 - Properties make it easy to pass updates from the python side to the user interface
 - Binding passes the changes that happened on the user interface to the python side.


    from kivy.app import App
    from kivy.uix.boxlayout import BoxLayout
    from kivy.lang import Builder
    from kivy.properties import StringProperty
    from kivy.properties import ObjectProperty
    from kivy.uix.textinput import TextInput
    from kivy.event import EventDispatcher
    Builder.load_string("""
    <CustLab1@Label>
        size_hint:0.3,1
    <CustLab2@Label>
        text: "Result"
        size_hint: 0.5,1
    <CustButton@Button>
        text: "+1"
        size_hint: 0.1,1    
    <CustTextInput@TextInput>:
        multiline: False
        size_hint:0.1,1
        
    <Tuto_Property>:
        orientation: "vertical"
        padding:10,10
        spacing: 10
        Label:
            text: "Press the 3 button (+1) several times and then modify the number in the TextInput.The first counter (with StringProperty but no binding) doesn't take into account the change that happened in the app, but the second one does.String Property makes it easy to pass the update from the python side to the user interface, binding pass the changes that happened on the user interface to the python side. "
            text_size: self.size
            padding: 20,20
            
        Property_no_Binding:
        Property_with_Binding:
        Simple:
    
    <Property_no_Binding>:
        spacing: 10
        label_ObjectProperty: result
        CustLab1:
            text: "With Property but no Binding"
        CustButton:
            on_press: root.counter_textInput_StringProperty()
        CustTextInput:
            id:textinput_id
            text: root.textInput_StringProperty
        CustLab2:
            id: result    
            
    <Property_with_Binding>:
        spacing: 10
        label_ObjectProperty: result
        CustLab1:
            text: "With Property and Binding"
        CustButton:
            on_press: root.counter_textInput_StringProperty()
        CustTextInput:
            id:textinput_id
            text: root.textInput_StringProperty
            on_text: root.textInput_StringProperty = self.text   ## this is the binding 
        CustLab2:
            id: result
            
    <Simple>
        spacing: 10
        CustLab1:
            text: "Without Property"
        CustButton:
            on_press: root.simple(textinput_id, result)
        CustTextInput:
            id:textinput_id
            text: "0"
        CustLab2:
            id: result
    
    
            
    """)
    
    class Property_no_Binding(BoxLayout):
        textInput_StringProperty= StringProperty("0")
        label_ObjectProperty = ObjectProperty(None)
        def counter_textInput_StringProperty(self):
            self.label_ObjectProperty.text= ("Before the counter was updated:\n\n textinput_id.text:" + self.ids.textinput_id.text + "\n\n textInput_StringProperty:" + self.textInput_StringProperty)
            self.textInput_StringProperty =str(int(self.textInput_StringProperty)+1)     
            
    class Property_with_Binding(BoxLayout):
        textInput_StringProperty= StringProperty("0")
        label_ObjectProperty = ObjectProperty(None)
        def counter_textInput_StringProperty(self):
            self.label_ObjectProperty.text= ("Before the counter was updated:\n\n textinput_id.text:" + self.ids.textinput_id.text + "\n\n textInput_StringProperty:" + self.textInput_StringProperty)
            self.textInput_StringProperty =str(int(self.textInput_StringProperty)+1)     
        pass
        
    class Simple(BoxLayout):
        def simple(self,textinput_id, result):
            result.text = ("Before the counter was updated:\n\nIn the TextInput:" + textinput_id.text)
            textinput_id.text = str(int(textinput_id.text) + 1)
            pass
    class Tuto_Property(BoxLayout): 
    
        
        # def __init__(self, **kwargs):
            # super(All, self).__init__(**kwargs)
            # app=App.get_running_app()
            # self.objproper_number.bind(text=lambda *a: self.change(app))
            # print(self.parent)
            
        # def counter(self,app): 
            # print("Stringproperty:",app.numbertext) 
            # print("ObjectProperty:",self.objproper_number.text)
            # print("text:",self.ids.number.text,"\n")
            # app.numbertext=str(int(app.numbertext)+1)
    
        # def change(self, app):
            # app.numbertext=self.objproper_number.text
        pass
    class MyApp(App):
        numbertext = StringProperty("0")
        def build(self):       
            return Tuto_Property()
    
    
    MyApp().run()

