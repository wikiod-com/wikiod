---
title: "Attributes"
slug: "attributes"
draft: false
images: []
weight: 9928
type: docs
toc: true
---

## Syntax
- [AddComponentMenu(string menuName)]
- [AddComponentMenu(string menuName, int order)]
- [CanEditMultipleObjects]
- [ContextMenuItem(string name, string function)]
- [ContextMenu(string name)]
- [CustomEditor(Type inspectedType)]
- [CustomEditor(Type inspectedType, bool editorForChildClasses)]
- [CustomPropertyDrawer(Type type)]
- [CustomPropertyDrawer(Type type, bool useForChildren)]
- [DisallowMultipleComponent]
- [DrawGizmo(GizmoType gizmo)]
- [DrawGizmo(GizmoType gizmo, Type drawnGizmoType)]
- [ExecuteInEditMode]
- [Header(string header)]
- [HideInInspector]
- [InitializeOnLoad]
- [InitializeOnLoadMethod]
- [MenuItem(string itemName)]
- [MenuItem(string itemName, bool isValidateFunction)]
- [MenuItem(string itemName, bool isValidateFunction, int priority)]
- [Multiline(int lines)]
- [PreferenceItem(string name)]
- [Range(float min, float max)]
- [RequireComponent(Type type)]
- [RuntimeInitializeOnLoadMethod]
- [RuntimeInitializeOnLoadMethod(RuntimeInitializeLoadType loadType)]
- [SerializeField]
- [Space(float height)]
- [TextArea(int minLines, int maxLines)]
- [Tooltip(string tooltip)]

# SerializeField #

Unity's serialization system can be used to do the following:
- **Can** serialize public nonstatic fields (of serializable types)
- **Can** serialize nonpublic nonstatic fields marked with the [SerializeField] attribute
- **Cannot** serialize static fields
- **Cannot** serialize static properties

Your field, even if marked with the SerializeField attribute, will only be attributed if it is of a type that Unity can serialize, which are:
- All classes inheriting from UnityEngine.Object (e.g. GameObject, Component, MonoBehaviour, Texture2D)
- All basic data types like int, string, float, bool
- Some built-in types like Vector2/3/4, Quaternion, Matrix4x4, Color, Rect, LayerMask
- Arrays of a serializable type
- List of a serializable type
- Enums
- Structs

## Common inspector attributes
<!-- language-all: c# -->

    [Header( "My variables" )]
    public string MyString;

    [HideInInspector]
    public string MyHiddenString;

    [Multiline( 5 )]
    public string MyMultilineString;

    [TextArea( 2, 8 )]
    public string MyTextArea;

    [Space( 15 )]
    public int MyInt;

    [Range( 2.5f, 12.5f )]
    public float MyFloat;

    [Tooltip( "This is a tip for MyDouble" )]
    public double MyDouble;

    [SerializeField]
    private double myHiddenDouble;

[![Result][1]][1]

When hovering over the label of a field:

[![Result2][2]][2]

----------


    [Header( "My variables" )]
    public string MyString;

**Header** places a bold label containing the text above the attributed field. This is often used for labeling groups to make them stand out against other labels.

    [HideInInspector]
    public string MyHiddenString;

**HideInInspector** prevents public fieldsfrom being shown in the inspector. This is useful for accessing fields from other parts of code where they aren't otherwise visible or mutable.

    [Multiline( 5 )]
    public string MyMultilineString;

**Multiline** creates a textbox with a specified number of lines. Exceeding this amount will neither expand the box nor wrap the text.

    [TextArea( 2, 8 )]
    public string MyTextArea;

**TextArea** allows multiline-style text with automatic word-wrapping and scroll bars if the text exceeds the allotted area.

    [Space( 15 )]
    public int MyInt;

**Space** forces the inspector to add extra space between previous and current items -useful in distinguishing and separating groups.

    [Range( 2.5f, 12.5f )]
    public float MyFloat;

**Range** forces a numerical value between a minimum and a maximum. This attribute also works on integers and doubles, even though min and max are specified as floats.

    [Tooltip( "This is a tip for MyDouble" )]
    public double MyDouble;

**Tooltip** shows an additional description whenever the field's label is hovered over.

    [SerializeField]
    private double myHiddenDouble;

**SerializeField** forces Unity to serialize the field - useful for private fields.


  [1]: http://i.stack.imgur.com/H4ilz.png
  [2]: http://i.stack.imgur.com/PNAeT.png

## Editor attributes
<!-- language-all: c# -->


    [InitializeOnLoad]
    public class AttributesExample : MonoBehaviour
    {
        
        static AttributesExample() 
        {
            [...]
        }
    
        [InitializeOnLoadMethod]
        private static void Foo()
        {
            [...]
        }
    }


----------

    [InitializeOnLoad]
    public class AttributesExample : MonoBehaviour
    {
        
        static AttributesExample() 
        {
            [...]
        }

The InitializeOnLoad attribute allows the user to initialize a class without any interaction from the user. This happens whenever the editor launches or on a recompile. The static constructor guarantees that this will be called before any other static functions.

    [InitializeOnLoadMethod]
    private static void Foo()
    {
        [...]
    }

The InitializeOnLoad attribute allows the user to initialize a class without any interaction from the user. This happens whenever the editor launches or on a recompile. The order of execution is not guaranteed for methods using this attribute.


----------

    [CanEditMultipleObjects]
    public class AttributesExample : MonoBehaviour 
    {

        public int MyInt;

        private static string prefsText = "";

        [PreferenceItem( "My Prefs" )]
        public static void PreferencesGUI()
        {
            prefsText = EditorGUILayout.TextField( "Prefs Text", prefsText );
        }

        [MenuItem( "Attributes/Foo" )]
        private static void Foo()
        {
            [...]
        }

        [MenuItem( "Attributes/Foo", true )]
        private static bool FooValidate() 
        {
            return false;
        }
    }

The result of the [PreferenceItem] attribute

[![Result 2][2]][2]

The result of the [MenuItem] attribute

[![Result 3][3]][3]


----------

    [CanEditMultipleObjects]
    public class AttributesExample : MonoBehaviour 

The CanEditMultipleObjects attribute allows you to edit values from your component over multiple GameObjects. Without this component you won't see your component appear like normal when selecting multiple GameObjects but instead you will see the message "Multi-object editing not supported"
>This attribute is for custom editors to support multi editing. Non-custom editors automatically support multi editing.

    [PreferenceItem( "My Prefs" )]
    public static void PreferencesGUI()

The PreferenceItem attribute allows to you create an extra item in Unity's preferences menu. The receiving method needs to be static for it to be used.

    [MenuItem( "Attributes/Foo" )]
    private static void Foo()
    {
        [...]
    }

    [MenuItem( "Attributes/Foo", true )]
    private static bool FooValidate() 
    {
        return false;
    }

The MenuItem attribute allows you to create custom menu items to execute functions. This example uses a validator function as well (which always returns false) to prevent execution of the function.

----------

    [CustomEditor( typeof( MyComponent ) )]
    public class AttributesExample : Editor
    {
        [...]
    }

The CustomEditor attribute allows you to create custom editors for your components. These editors will be used for drawing your component in the inspector and need to derive from the Editor class.

    [CustomPropertyDrawer( typeof( MyClass ) )]
    public class AttributesExample : PropertyDrawer 
    {
        [...]
    }

The CustomPropertyDrawer attribute allows you to create a custom property drawer for in the inspector. You can use these drawers for your custom data types so that they can be seen used in the inspector.

    [DrawGizmo( GizmoType.Selected )]
    private static void DoGizmo( AttributesExample obj, GizmoType type )
    {
        [...]
    }

The DrawGizmo attribute allows you to draw custom gizmos for your components. These gizmos will be drawn in the Scene View. You can decide when to draw the gizmo by using the GizmoType parameter in the DrawGizmo attribute.
> The receiving method requires two parameters, the first is the component to draw the gizmo for and the second is the state that the object who needs the gizmo drawn is in.

  [2]: http://i.stack.imgur.com/QHJAC.png
  [3]: http://i.stack.imgur.com/aOY7Z.png

## Component attributes
<!-- language-all: c# -->

    [DisallowMultipleComponent]
    [RequireComponent( typeof( Rigidbody ) )]
    public class AttributesExample : MonoBehaviour 
    {
        [...]
    }


----------

    [DisallowMultipleComponent]

The DisallowMultipleComponent attribute prevents users adding multiple instances of this component to one GameObject.

    [RequireComponent( typeof( Rigidbody ) )]

The RequireComponent attribute allows you to specify another component (or more) as requirements for when this component is added to a GameObject. When you add this component to a GameObject, the required components will be automatically added (if not already present) and those components cannot be removed until the one that requires them is removed.

## Runtime attributes
<!-- language-all: c# -->

    [ExecuteInEditMode]
    public class AttributesExample : MonoBehaviour 
    {

        [RuntimeInitializeOnLoadMethod]
        private static void FooBar() 
        {
            [...]
        }

        [RuntimeInitializeOnLoadMethod( RuntimeInitializeLoadType.BeforeSceneLoad )]
        private static void Foo() 
        {
            [...]
        }

        [RuntimeInitializeOnLoadMethod( RuntimeInitializeLoadType.AfterSceneLoad )]
        private static void Bar() 
        {
            [...]
        }

        void Update() 
        {
            if ( Application.isEditor )
            {
                [...]
            } 
            else
            {
                [...]
            }
        }
    }


----------

    [ExecuteInEditMode]
    public class AttributesExample : MonoBehaviour 

The ExecuteInEditMode attribute forces Unity to execute this script's magic methods even while the game is not playing.

> The functions are not constantly called like in play mode
> - Update is only called when something in the scene changed.
> - OnGUI is called when the Game View receives an Event.
> - OnRenderObject and the other rendering callback functions are called on every repaint of the Scene View or Game View.


    [RuntimeInitializeOnLoadMethod]
    private static void FooBar()

    [RuntimeInitializeOnLoadMethod( RuntimeInitializeLoadType.BeforeSceneLoad )]
    private static void Foo() 

    [RuntimeInitializeOnLoadMethod( RuntimeInitializeLoadType.AfterSceneLoad )]
    private static void Bar() 

The RuntimeInitializeOnLoadMethod attribute allows a runtime class method to be called when the game loads the runtime, without any interaction from the user.

You can specify if you want the method to be invoked before or after scene load (after is default). The order of execution is not guaranteed for methods using this attribute.

## Menu attributes
<!-- language-all: c# -->

    [AddComponentMenu( "Examples/Attribute Example" )]
    public class AttributesExample : MonoBehaviour 
    {
        [ContextMenuItem( "My Field Action", "MyFieldContextAction" )]
        public string MyString;
    
        private void MyFieldContextAction() 
        {
            [...]
        }
    
        [ContextMenu( "My Action" )]
        private void MyContextMenuAction() 
        {
            [...]
        }
    }

The result of the [AddComponentMenu] attribute

[![Result 1][1]][1]

The result of the [ContextMenuItem] attribute

[![Result 2][2]][2]

The result of the [ContextMenu] attribute

[![Result 3][3]][3]


----------

    [AddComponentMenu( "Examples/Attribute Example" )]
    public class AttributesExample : MonoBehaviour 

The AddComponentMenu attribute allows you to place your component anywhere in the Component menu instead of the Component->Scripts menu.

    [ContextMenuItem( "My Field Action", "MyFieldContextAction" )]
    public string MyString;

    private void MyFieldContextAction() 
    {
        [...]
    }

The ContextMenuItem attribute allows you to define functions that can be added to the context menu of a field. These functions will be executed upon selection.

    [ContextMenu( "My Action" )]
    private void MyContextMenuAction() 
    {
        [...]
    }

The ContextMenu attribute allows you to define functions that can be added to the context menu of the component.


  [1]: http://i.stack.imgur.com/LpxJ2.png
  [2]: http://i.stack.imgur.com/shInS.png
  [3]: http://i.stack.imgur.com/5DyzB.png

