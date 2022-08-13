---
title: "Events usage with HandlerManager"
slug: "events-usage-with-handlermanager"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Simple Event and EventHandler usage syntax
HanlderManager providing:

    public class HandlerManagerProvider {
    
        private static HandlerManager handlerManager;
    
        private HandlerManagerProvider() {
            
        }
        
        public static HandlerManager get() {
            return handlerManager != null ? handlerManager : (handlerManager = new HandlerManager(null));
        }
    }
    


EventHandler:

    import com.google.gwt.event.shared.EventHandler;

    public interface CustomEventHandler extends EventHandler {
    
        void doMyAction(CustomEvent event);

    }

Event:

    import com.google.gwt.event.shared.GwtEvent;
    
    public class CustomEvent extends GwtEvent<CustomEventHandler> {
    
        public static final Type<CustomEventHandler> TYPE = new Type<>();
    
        @Override
        public Type<CustomEventHandler> getAssociatedType() {
            return TYPE;
        }
    
        @Override
        protected void dispatch(CustomEventHandler) {
            handler.doMyAction(this);
        }
    
    }

Handling event (code snippet):

    import com.google.gwt.event.shared.HandlerManager;
    import com.google.gwt.event.shared.HandlerRegistration;

    [...]

    private List<HandlerRegistration> registrations = new ArrayList<>();
    private final HandlerManager handlerManager = HandlerManagerProvider.get();

    private void bind() {
        registrations.add(handlerManager.addHandler(CustomEvent.TYPE, new CustomEventHandler() {
    
            @Override
            public void doMyAction(CustomEvent event) {
                //Action after event firing
            }
        }));
    }

Firing event (code snippet):

    private final HandlerManager handlerManager = HandlerManagerProvider.get();

    public void fireEvent() {
        handlerManager.fireEvent(new CustomEvent());
    }

Cleaning registrations after e.g. some window destroy (event handling for this window is no longer needed):

    private void clearRegistrations() {
        for (HandlerRegistration registration : registrations)
            registration.removeHandler();
        registrations.clear();
    }

