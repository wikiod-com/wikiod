---
title: "User Interface System (UI)"
slug: "user-interface-system-ui"
draft: false
images: []
weight: 9928
type: docs
toc: true
---

## Subscribing to event in code
By default, one should subscribe to event using inspector, but sometimes it's better to do it in code. In this example we subscribe to click event of a button in order to handle it.

<!-- language: lang-cs -->

    using UnityEngine;
    using UnityEngine.UI;
    
    [RequireComponent(typeof(Button))]
    public class AutomaticClickHandler : MonoBehaviour
    {
        private void Awake()
        {
            var button = this.GetComponent<Button>();
            button.onClick.AddListener(HandleClick);
        }
    
        private void HandleClick()
        {
            Debug.Log("AutomaticClickHandler.HandleClick()", this);
        }
    }

The UI components usually provide their main listener easily :

- Button : [onClick](https://docs.unity3d.com/ScriptReference/UI.Button-onClick.html)
- Dropdown : [onValueChanged](https://docs.unity3d.com/ScriptReference/UI.Dropdown-onValueChanged.html)
- InputField : [onEndEdit](https://docs.unity3d.com/ScriptReference/UI.InputField-onEndEdit.html), [onValidateInput](https://docs.unity3d.com/ScriptReference/UI.InputField-onValidateInput.html), [onValueChanged](https://docs.unity3d.com/ScriptReference/UI.InputField-onValueChanged.html)
- Scrollbar : [onValueChanged](https://docs.unity3d.com/ScriptReference/UI.Scrollbar-onValueChanged.html)
- ScrollRect : [onValueChanged](https://docs.unity3d.com/ScriptReference/UI.ScrollRect-onValueChanged.html)
- Slider : [onValueChanged](https://docs.unity3d.com/ScriptReference/UI.Slider-onValueChanged.html)
- Toggle : [onValueChanged](https://docs.unity3d.com/ScriptReference/UI.Toggle-onValueChanged.html)

## Adding mouse listeners
Sometimes, you want to add listeners on particular events not natively provided by the components, in particular mouse events. To do so, you will have to add them by yourself using an `EventTrigger` component :

<!-- language: c# -->

    using UnityEngine;
    using UnityEngine.EventSystems;
    
    [RequireComponent(typeof( EventTrigger ))]
    public class CustomListenersExample : MonoBehaviour
    {
        void Start( )
        {
            EventTrigger eventTrigger = GetComponent<EventTrigger>( );
            EventTrigger.Entry entry = new EventTrigger.Entry( );
            entry.eventID = EventTriggerType.PointerDown;
            entry.callback.AddListener( ( data ) => { OnPointerDownDelegate( (PointerEventData)data ); } );
            eventTrigger.triggers.Add( entry );
        }
    
        public void OnPointerDownDelegate( PointerEventData data )
        {
            Debug.Log( "OnPointerDownDelegate called." );
        }
    }

Various eventID are possible :

- PointerEnter
- PointerExit
- PointerDown
- PointerUp
- PointerClick
- Drag
- Drop
- Scroll
- UpdateSelected
- Select
- Deselect
- Move
- InitializePotentialDrag
- BeginDrag
- EndDrag
- Submit
- Cancel

