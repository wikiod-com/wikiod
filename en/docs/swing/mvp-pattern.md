---
title: "MVP Pattern"
slug: "mvp-pattern"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Simple MVP Example
To illustrate a simple example usage of the MVP pattern, consider the following code which creates a simple UI with only a button and a label. When the button is clicked, the label updates with the number of times the button has been clicked.

We have 5 classes:
- Model - The POJO to maintain state (M in MVP)
- View - The class with UI code (V in MVP)
- ViewListener - Interface providing methods to responding to actions in the view
- Presenter - Responds to input, and updates the view (P in MVP)
- Application - The "main" class to pull everything together and launch the app

A minimal "model" class which just maintains a single `count` variable.
    
    /**
     * A minimal class to maintain some state 
     */
    public class Model {
        private int count = 0;
    
        public void addOneToCount() {
            count++;
        }
    
        public int getCount() {
            return count;
        }
    }


A minimal interface to notify the listeners:

    /**
     * Provides methods to notify on user interaction
     */
    public interface ViewListener {
        public void onButtonClicked();
    }

The view class constructs all UI elements. The view, and *only* the view, should have reference to UI elements (ie. no buttons, text fields, etc. in the presenter or other classes).
    
    /**
     * Provides the UI elements
     */

    import java.awt.GridLayout;
    import java.awt.event.ActionEvent;
    import java.awt.event.ActionListener;
    import java.util.ArrayList;
    import javax.swing.JButton;
    import javax.swing.JFrame;
    import javax.swing.JLabel;
    import javax.swing.WindowConstants;

    public class View {
        // A list of listeners subscribed to this view
        private final ArrayList<ViewListener> listeners;
        private final JLabel label;
        
        public View() {
            final JFrame frame = new JFrame();
            frame.setSize(200, 100);
            frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
            frame.setLayout(new GridLayout());
    
            final JButton button = new JButton("Hello, world!");

            button.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent e) {
                    notifyListenersOnButtonClicked();
                }
            });
            frame.add(button);
    
            label = new JLabel();
            frame.add(label);
    
            this.listeners = new ArrayList<ViewListener>();
    
            frame.setVisible(true);
        }
    
        // Iterate through the list, notifying each listner individualy 
        private void notifyListenersOnButtonClicked() {
            for (final ViewListener listener : listeners) {
                listener.onButtonClicked();
            }
        }
    
        // Subscribe a listener
        public void addListener(final ViewListener listener) {
            listeners.add(listener);
        }
    
        public void setLabelText(final String text) {
            label.setText(text);
        }
    }


----------


The notification logic may also be coded like this in Java8:


            ...
            final Button button = new Button("Hello, world!");
            // In order to do so, our interface must be changed to accept the event parametre
            button.addActionListener((event) -> {
                notifyListeners(ViewListener::onButtonClicked, event);
                // Example of calling methodThatTakesALong, would be the same as callying:
                // notifyListeners((listener, long)->listener.methodThatTakesALong(long), 10L)
                notifyListeners(ViewListener::methodThatTakesALong, 10L);
            });
            frame.add(button);
            ...

    /**
     * Iterates through the subscribed listeneres notifying each listener individually.
     * Note: the {@literal '<T>' in private <T> void} is a Bounded Type Parametre. 
     *
     * @param <T>      Any Reference Type (basically a class).
     * 
     * @param consumer A method with two parameters and no return, 
     *                 the 1st parametre is a ViewListner, 
     *                 the 2nd parametre is value of type T.
     * 
     * @param data     The value used as parametre for the second argument of the
     *                 method described by the parametre consumer.
     */
    private <T> void notifyListeners(final BiConsumer<ViewListener, T> consumer, final T data) {
        // Iterate through the list, notifying each listener, java8 style 
        listeners.forEach((listener) -> {

            // Calls the funcion described by the object consumer.
            consumer.accept(listener, data);

            // When this method is called using ViewListener::onButtonClicked
            // the line: consumer.accept(listener,data); can be read as:
            // void accept(ViewListener listener, ActionEvent data) {
            //     listener.onButtonClicked(data);
            // }
            
        });
    }
The interface must be refactored in order to take the ActionEvent as a parametre:

    public interface ViewListener {
        public void onButtonClicked(ActionEvent evt);
        // Example of methodThatTakesALong signature
        public void methodThatTakesALong(long );
    }

Here only one notify-method is needed, the actual listener method and its parameter are passed on as parameters. In case needed this can also be used for something a little less nifty than actual event handling, it all works as long as there is a method in the interface, e.g.:

            notifyListeners(ViewListener::methodThatTakesALong, -1L);


----------


The presenter can take in the view and add itself as a listener. When the button is clicked in the view, the view notifies all listeners (including the presenter). Now that the presenter is notified, it can take appropriate action to update the model (ie. the state of the application), and then update the view accordingly.
    
    /**
     * Responsible to responding to user interaction and updating the view
     */
    public class Presenter implements ViewListener {
        private final View view;
        private final Model model;
    
        public Presenter(final View view, final Model model) {
            this.view = view;
            view.addListener(this);
            this.model = model;
        }
    
        @Override
        public void onButtonClicked() {
            // Update the model (ie. the state of the application)
            model.addOneToCount();
            // Update the view
            view.setLabelText(String.valueOf(model.getCount()));
        }
    }

To put everything together, the view can be created and injected into the presenter. Similarly, an initial model can be created and injected. While both *can* be created in the presenter, injecting them into the constructor allows for much simpler testing.
    
    public class Application {
        public Application() {
            final View view = new View();
            final Model model = new Model();
            new Presenter(view, model);
        }
    
        public static void main(String... args) {
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    new Application();
                }
            });
        }
    }


