---
title: "Getting started with swing"
slug: "getting-started-with-swing"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Incrementing with a button
    import javax.swing.JButton;
    import javax.swing.JFrame;
    import javax.swing.JLabel;
    import javax.swing.JPanel;
    import javax.swing.SwingUtilities;
    import javax.swing.WindowConstants;
    
    /**
     * A very simple Swing example.
     */
    public class SwingExample {
        /**
         * The number of times the user has clicked the button.
         */
        private long clickCount;
        
        /**
         * The main method: starting point of this application.
         *
         * @param arguments the unused command-line arguments.
         */
        public static void main(final String[] arguments) {
            new SwingExample().run();
        }
    
        /**
         * Schedule a job for the event-dispatching thread: create and show this
         * application's GUI.
         */
        private void run() {
            SwingUtilities.invokeLater(this::createAndShowGui);
        }
        
        /**
         * Create the simple GUI for this application and make it visible.
         */
        private void createAndShowGui() {
            // Create the frame and make sure the application exits when the user closes
            // the frame.
            JFrame mainFrame = new JFrame("Counter");
            mainFrame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
            
            // Add a simple button and label.
            JPanel panel = new JPanel();
            JButton button = new JButton("Click me!");
            JLabel label = new JLabel("Click count: " + clickCount);
            panel.add(button);
            panel.add(label);
            mainFrame.getContentPane().add(panel);
            
            // Add an action listener to the button to increment the count displayed by
            // the label.
            button.addActionListener(actionEvent -> {
                clickCount++;
                label.setText("Click count: " + clickCount);
            });
            
            // Size the frame.
            mainFrame.setBounds(80, 60, 400, 300);
            //Center on screen
            mainFrame.setLocationRelativeTo(null);
            //Display frame
            mainFrame.setVisible(true);
        }
    }

**Result**

As the button labeled "Click me!" is pressed the click count will increase by one:

[![Running program][1]][1]


  [1]: http://i.stack.imgur.com/rH2OG.png

## "Hello World!" on window title with compatibility
Using `java.lang.Runnable` we make our "Hello World!" example available to Java users with versions dating all the way back to the 1.2 release:

    import javax.swing.JFrame;
    import javax.swing.SwingUtilities;
    import javax.swing.WindowConstants;

    public class Main {
        public static void main(String[] args){
            SwingUtilities.invokeLater(new Runnable(){

                @Override
                public void run(){
                    JFrame frame = new JFrame("Hello World!");
                    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
                    frame.setSize(200, 100);
                    frame.setVisible(true);
                }
            });
        }
    }

## "Hello World!" on window title with lambda
    import javax.swing.JFrame;
    import javax.swing.SwingUtilities;
    import javax.swing.WindowConstants;

    public class Main {
        public static void main(String[] args) {
            SwingUtilities.invokeLater(() -> {
                JFrame frame = new JFrame("Hello World!");
                frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
                frame.setSize(200, 100);
                frame.setVisible(true);
            });
        }
    }

Inside the `main` method:
<br>On the first line `SwingUtilities.invokeLater` is called and a lambda expression with a block of code `() -> {...}` is passed to it. This executes the passed lambda expression on the EDT, which is short for Event Dispatch Thread, instead of the main thread. This is necessary, because inside the lambda expression's code block, there are Swing components going to be created and updated.

Inside the code block of the lambda expression:
<br>On the first line, a new `JFrame` instance called `frame` is created using `new JFrame("Hello World!")`. This creates a window instance with "Hello World!" on its title. Afterwards on the second line the `frame` is configured to `EXIT_ON_CLOSE`. Otherwise the window will just be closed, but the execution of the program is going to remain active. The third line configures the `frame` instance to be 200 pixels in width and 100 pixels in height using the `setSize` method. Until now the execution won't show up anything at all. Only after calling `setVisible(true)` on the fourth line, the `frame` instance is configured to appear on the screen.

