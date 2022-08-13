---
title: "Using Swing for Graphical User Interfaces"
slug: "using-swing-for-graphical-user-interfaces"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

Quitting the application on window close
========================================

It's easy to forget to quit the application when the window is closed. Remember to add the following line.

    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE); //Quit the application when the JFrame is closed

## Creating an Empty Window (JFrame)
Creating the JFrame
===================

Creating a window is easy. You just have to create a `JFrame`.

    JFrame frame = new JFrame();

Titling the Window
==================

You may wish to give your window a title. You can so do by passing a string when creating the `JFrame`, or by calling `frame.setTitle(String title)`.

    JFrame frame = new JFrame("Super Awesome Window Title!");
    //OR
    frame.setTitle("Super Awesome Window Title!");

Setting the Window Size
=======================

The window will be as small as possible when it has been created. To make it bigger, you can set its size explicitly:

    frame.setSize(512, 256);

Or you can have the frame size itself based on the size of its contents with the `pack()` method. 

    frame.pack();

The `setSize()` and `pack()` methods are mutually exclusive, so use one or the other.

What to do on Window Close
==========================

Note that the application will **not** quit when the window has been closed. You can quit the application after the window has been closed by telling the `JFrame` to do that.

    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);

Alternatively, you can tell the window to do something else when it is closed.

    WindowConstants.DISPOSE_ON_CLOSE //Get rid of the window
    WindowConstants.EXIT_ON_CLOSE //Quit the application
    WindowConstants.DO_NOTHING_ON_CLOSE //Don't even close the window
    WindowConstants.HIDE_ON_CLOSE //Hides the window - This is the default action

Creating a Content Pane
=======================

An optional step is to create a content pane for your window. This is not needed, but if you want to do so, create a `JPanel` and call `frame.setContentPane(Component component)`.

    JPanel pane = new JPanel();
    frame.setContentPane(pane);

Showing the Window
==================

After creating it, you will want to create your components, then show the window. Showing the window is done as such.

    frame.setVisible(true);

Example
=======

For those of you who like to copy and paste, here's some example code.

    JFrame frame = new JFrame("Super Awesome Window Title!"); //Create the JFrame and give it a title
    frame.setSize(512, 256); //512 x 256px size
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE); //Quit the application when the JFrame is closed

    JPanel pane = new JPanel(); //Create the content pane
    frame.setContentPane(pane); //Set the content pane

    frame.setVisible(true); //Show the window

[![Example output][1]][1]


  [1]: http://i.stack.imgur.com/apsSc.png

## Adding Components
A component is some sort of user interface element, such as a button or a text field.

Creating a Component
====================

Creating components is near identical to creating a window. Instead of creating a `JFrame` however, you create that component. For example, to create a `JButton`, you do the following.

    JButton button = new JButton();

Many components can have parameters passed to them when created. For example, a button can be given some text to display.

    JButton button = new JButton("Super Amazing Button!");

If you don't want to create a button, a list of common components can be found in another example on this page.

The parameters that can be passed to them vary from component to component. A good way of checking what they can accept is by looking at the paramters within your IDE (If you use one). The default shortcuts are listed below.

- IntelliJ IDEA - Windows / Linux: `CTRL + P`
- IntelliJ IDEA - OS X / macOS: `CMD + P`
- Eclipse: `CTRL + SHIFT + Space`
- NetBeans: `CTRL + P`

[![Example of viewing method parameter information in IntelliJ IDEA][1]][1]

Showing the Component
=====================

After a component has been created, you would typically set its parameters. After than, you need to put it somewhere, such as on your `JFrame`, or on your content pane if you created one.

    frame.add(button); //Add to your JFrame
    //OR
    pane.add(button); //Add to your content pane
    //OR
    myComponent.add(button); //Add to whatever

Example
=======

Here's an example of creating a window, setting a content pane, and adding a button to it.

    JFrame frame = new JFrame("Super Awesome Window Title!"); //Create the JFrame and give it a title
    frame.setSize(512, 256); //512 x 256px size
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE); //Quit the application when the JFrame is closed
    
    JPanel pane = new JPanel(); //Create the content pane
    frame.setContentPane(pane); //Set the content pane
    
    JButton button = new JButton("Super Amazing Button!"); //Create the button
    pane.add(button); //Add the button to the content pane
    
    frame.setVisible(true); //Show the window

[![Example output][2]][2]


  [1]: http://i.stack.imgur.com/c5MgX.png
  [2]: http://i.stack.imgur.com/WM7Hq.png

## Setting Parameters for Components

Components have various parameters that can be set for them. They vary from component to component, so a good way to see what parameters can be set for components is to start typing `componentName.set`, and let your IDE's autocomplete (If you use an IDE) suggest methods. The default shortcut in many IDEs, if it doesn't show up automatically, is `CTRL + Space`.

[![Autocomplete in IntelliJ IDEA][1]][1]

## Common parameters that are shared between all components

|Description|Method|
|---|---|
|Sets the smallest size that the component can be (only if the layout manager honors the minimumSize property)|`setMinimumSize(Dimension minimumSize)`|
|Sets the biggest size that the component can be (only if the layout manager honors the maximumSize property)|`setMaximumSize(Dimension maximumSize)`|
|Sets the perferred size of the component (only if the layout manager honors the preferredSize property)|`setPreferredSize(Dimension preferredSize)`|
|Shows or hides the component|`setVisible(boolean aFlag)`|
|Sets whether the component should respond to user input|`setEnabled(boolean enabled)`|
|Sets the font of text|`setFont(Font font)`|
|Sets the text of the tooltip|`setToolTipText(String text)`|
|Sets the Backgroundcolor of the component|`setBackground(Color bg)`|
|Sets the Foregroundcolor (font color) of the component|`setForeground(Color bg)`|

## Common parameters in other components

|Common Components|Description|Method|
|---|---|---|
|`JLabel`, `JButton`, `JCheckBox`, `JRadioButton`, `JToggleButton`, `JMenu`, `JMenuItem`, `JTextArea`, `JTextField`|Sets the text displayed|`setText(String text)`|
|`JProgressBar`, `JScrollBar`, `JSlider`, `JSpinner`|Set's a numerical value between the component's min and max values|`setValue(int n)`|
|`JProgressBar`, `JScrollBar`, `JSlider`, `JSpinner`|Set's the smallest possible value that the `value` property can be|`setMinimum(int n)`|
|`JProgressBar`, `JScrollBar`, `JSlider`, `JSpinner`|Set's the biggest possible value that the `value` property can be|`setMaxmimum(int n)`|
|`JCheckBox`, `JToggleBox`|Set's whether the value is true or false (Eg: Should a checkbox be checked?)|`setSelected(boolean b)`|

  [1]: http://i.stack.imgur.com/v1wsD.png

## Common Components
|Description|Class|
|---|---|
|Button|`JButton`|
|Checkbox|`JCheckBox`|
|Drop down menu / Combo box|`JComboBox`|
|Label|`JLabel`|
|List|`JList`|
|Menu bar|`JMenuBar`|
|Menu in a menu bar|`JMenu`|
|Item in a menu|`JMenuItem`|
|Panel|`JPanel`|
|Progress bar|`JProgressBar`|
|Radio button|`JRadioButton`|
|Scroll bar|`JScrollBar`|
|Slider|`JSlider`|
|Spinner / Number picker|`JSpinner`|
|Table|`JTable`|
|Tree|`JTree`|
|Text area / Multiline text field|`JTextArea`|
|Text field|`JTextField`|
|Tool bar|`JToolBar`|

## Making Interactive User Interfaces
Having a button there is all well and good, but what's the point if clicking it does nothing? `ActionListener`s are used to tell your button, or other component to do something when it is activated.

Adding `ActionListener`s is done as such.

    buttonA.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
            //Code goes here...
            System.out.println("You clicked the button!");
        }
    });

Or, if you're using Java 8 or above...

    buttonA.addActionListener(e -> {
        //Code
        System.out.println("You clicked the button!");
    });

Example (Java 8 and above)
==========================

    JFrame frame = new JFrame("Super Awesome Window Title!"); //Create the JFrame and give it a title
    frame.setSize(512, 256); //512 x 256px size
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE); //Quit the application when the JFrame is closed

    JPanel pane = new JPanel(); //Create a pane to house all content
    frame.setContentPane(pane);

    JButton button = new JButton("Click me - I know you want to.");
    button.addActionListener(e -> {
        //Code goes here
        System.out.println("You clicked me! Ouch.");
    });
    pane.add(buttonA);

    frame.setVisible(true); //Show the window


## Organizing Component Layout
Adding components one after another results in a UI that's hard to use, because the components all are **somewhere**. The components are ordered from top to bottom, each component in a separate "row".

To remedy this and provide you as developer with a possibility to layout components easily Swing has `LayoutManager`s.

These LayoutManagers are covered more extensively in Introduction to Layout Managers  <!-- editor's note: TBD -->
as well as the separate Layout Manager topics:

- [Grid Layout][1]
- [GridBag Layout][2]


  [1]: https://www.wikiod.com/swing/gridlayout
  [2]: https://www.wikiod.com/swing/gridbag-layout

